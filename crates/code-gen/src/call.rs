/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
//! `CodeBuilder` helper functions for function calls and arguments.

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::reg_pool::RegisterPool;
use crate::state::FunctionFixup;
use crate::{
    ArgumentAndTempScope, RepresentationOfRegisters, SpilledRegisterRegion,
    MAX_REGISTER_INDEX_FOR_PARAMETERS,
};
use source_map_node::Node;
use std::collections::HashSet;
use swamp_semantic::{pretty_module_name, ArgumentExpression, InternalFunctionDefinitionRef};
use swamp_types::prelude::Signature;
use swamp_types::TypeKind;
use swamp_vm_types::types::{BasicTypeKind, BasicTypeRef, Destination, TypedRegister, VmType};
use swamp_vm_types::{FrameMemoryRegion, REG_ON_FRAME_SIZE};

pub struct CopyArgument {
    pub canonical_target: TypedRegister,
    pub source_temporary: TypedRegister,
}
pub struct EmitArgumentInfo {
    pub argument_and_temp_scope: ArgumentAndTempScope,
    pub copy_back_of_registers_mutated_by_callee: Vec<MutableReturnReg>,
}

pub struct MutableReturnReg {
    pub target_location_after_call: Destination,
    pub parameter_reg: TypedRegister,
}

impl CodeBuilder<'_> {
    // TODO: for mutable arguments we want to leave output part of the spilling
    // - store abi registers (do not save r0 if that is the destination for the call itself, since we want to clobber it in that case)
    // - store temp registers (including the newly created replacement base regs)
    // - call (callee is allowed to clobber r0, r1, r2, r128)
    // - restore temp registers (so our r128 is valid again)
    // - issue copy backs of the mutables scalars. (replacement) base reg is valid, and the r1 and r2 are clobbered, which is good.
    // - restore abi registers
    // - copy back of r0 for immutable scalars to target register,  unless the target was r0 itself, in that case it was left out of store abi register mask
    pub fn spill_required_registers(&mut self, node: &Node, comment: &str) -> ArgumentAndTempScope {
        const ABI_ARGUMENT_RETURN_AND_ARGUMENT_REGISTERS: usize =
            MAX_REGISTER_INDEX_FOR_PARAMETERS as usize + 1; // r0-r6
        const ABI_ARGUMENT_MASK: u8 =
            ((1u16 << ABI_ARGUMENT_RETURN_AND_ARGUMENT_REGISTERS) - 1) as u8;

        let abi_parameter_frame_memory_region = self.temp_frame_space_for_register(
            ABI_ARGUMENT_RETURN_AND_ARGUMENT_REGISTERS as u8,
            &format!("emit abi arguments r0-r6 {comment}"),
        );
        
        self.builder.add_st_masked_regs_to_frame(
            abi_parameter_frame_memory_region.addr,
            ABI_ARGUMENT_MASK,
            node,
            "spill masked registers to stack frame memory.",
        );

        let abi_parameter_region = SpilledRegisterRegion {
            registers: RepresentationOfRegisters::Mask(ABI_ARGUMENT_MASK),
            frame_memory_region: abi_parameter_frame_memory_region,
        };

        let (first_temp_register_index, temp_register_probable_live_count) =
            self.temp_registers.start_index_and_number_of_allocated();
        debug_assert_eq!(first_temp_register_index, 128);

        let temp_register_region = if temp_register_probable_live_count > 0 {
            let temp_register_frame_memory_region = self.temp_frame_space_for_register(temp_register_probable_live_count, &format!("emit temp arguments from r{first_temp_register_index} count:{temp_register_probable_live_count} {comment}"));
            let temp_register_region = SpilledRegisterRegion {
                registers: RepresentationOfRegisters::Range {
                    start_reg: first_temp_register_index,
                    count: temp_register_probable_live_count,
                },
                frame_memory_region: temp_register_frame_memory_region,
            };

            self.builder.add_st_contiguous_regs_to_frame(
                temp_register_frame_memory_region,
                first_temp_register_index,
                temp_register_probable_live_count,
                node,
                "spill contiguous range of registers to stack frame memory",
            );
            Some(temp_register_region)
        } else {
            None
        };

        ArgumentAndTempScope {
            argument_registers: abi_parameter_region,
            scratch_registers: temp_register_region,
        }
    }

    pub fn setup_return_pointer_reg(
        &mut self,
        output_destination: &Destination,
        return_basic_type: BasicTypeRef,
        node: &Node,
    ) {
        let r0 = TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));

        let return_pointer_reg = self.emit_compute_effective_address_to_register(
            output_destination,
            node,
            "r0: create an absolute pointer to r0 if needed",
        );

        self.builder.add_mov_reg(
            &r0,
            &return_pointer_reg,
            node,
            "r0: copy the return pointer into r0",
        );
    }

    fn emit_single_argument(
        &mut self,
        argument_expr: &ArgumentExpression,
        argument_to_use: &TypedRegister,
        target_canonical_argument_register: &TypedRegister,
        parameter_basic_type: &BasicTypeRef,
        copy_back_phase_one: &mut Vec<MutableReturnReg>,
        node: &Node,
        ctx: &Context,
    ) {
        match argument_expr {
            ArgumentExpression::BorrowMutableReference(lvalue) => {
                let original_destination = self.emit_lvalue_address(lvalue, ctx);

                if parameter_basic_type.should_be_copied_back_when_mutable_arg_or_return() {
                    // Load the primitive from memory
                    self.emit_transfer_value_to_register(
                        argument_to_use,
                        &original_destination,
                        node,
                        "must get primitive from lvalue and pass as copy back (by value)",
                    );

                    // Add a copy back to the original location (base register will be restored by spill/restore)
                    copy_back_phase_one.push(MutableReturnReg {
                        target_location_after_call: original_destination,
                        parameter_reg: target_canonical_argument_register.clone(),
                    });
                } else {
                    let flattened_source_pointer_reg = self
                        .emit_compute_effective_address_to_register(
                            &original_destination,
                            node,
                            "flattened into absolute pointer",
                        );
                    self.builder.add_mov_reg(
                        argument_to_use,
                        &flattened_source_pointer_reg,
                        node,
                        "copy absolute address",
                    );
                }
            }
            ArgumentExpression::Expression(expr) => {
                // For expressions that need memory (like VecStorage literals), we need to check
                // if they should be materialized into temporary frame space first
                if Self::rvalue_needs_memory_location_to_materialize_in(&mut self.state.layout_cache, expr) {
                    // Use the helper function to get a pointer to the temporary storage
                    let temp_ptr = self.emit_scalar_rvalue_or_pointer_to_temporary(expr, ctx, true);
                    
                    self.builder.add_mov_reg(
                        argument_to_use,
                        &temp_ptr,
                        node,
                        "copy temporary storage address to argument register"
                    );
                } else {
                    // Normal case: expression can be materialized directly into register
                    self.emit_expression_into_register(
                        argument_to_use,
                        expr,
                        "argument expression into specific argument register",
                        ctx,
                    );
                }
            }
        }
    }

    pub(crate) fn emit_arguments(
        &mut self,
        output_destination: &Destination,
        node: &Node,
        signature: &Signature,
        self_variable: Option<&TypedRegister>,
        arguments: &[ArgumentExpression],
        is_host_call: bool,
        ctx: &Context,
    ) -> EmitArgumentInfo {
        let mut copy_back_operations: Vec<MutableReturnReg> = Vec::new();
        let has_return_value = !matches!(&*signature.return_type.kind, TypeKind::Unit);

        // Step 1: Spill live registers before we start using ABI registers
        let spill_scope = self.spill_required_registers(node, "spill before emit arguments");

        // Step 2: Handle return value setup
        if has_return_value {
            let return_basic_type = self.state.layout_cache.layout(&signature.return_type);

            if return_basic_type.is_aggregate() {
                // For aggregates: initialize the destination space first, then set up r0 as pointer to destination
                self.setup_return_pointer_reg(output_destination, return_basic_type, node);
            } else {
                // For primitives: add r0 to copy-back list (function writes to r0, we copy to destination)
                let r0 =
                    TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));
                copy_back_operations.push(MutableReturnReg {
                    target_location_after_call: output_destination.clone(),
                    parameter_reg: r0,
                });
            }
        }

        // Step 3: Prepare argument registers and handle temporary register conflicts
        let mut temp_to_abi_copies = Vec::new();
        let mut argument_registers = RegisterPool::new(1, 10); // r1-r10 for arguments

        for (index_in_signature, type_for_parameter) in signature.parameters.iter().enumerate() {
            let parameter_basic_type = self
                .state
                .layout_cache
                .layout(&type_for_parameter.resolved_type);
            let target_canonical_argument_register = argument_registers.alloc_register(
                VmType::new_unknown_placement(parameter_basic_type.clone()),
                &format!("{index_in_signature}:{}", type_for_parameter.name),
            );

            let argument_to_use = if self.argument_needs_to_be_in_a_temporary_register_first(
                &target_canonical_argument_register,
            ) {
                let temp_reg = self.temp_registers.allocate(
                    target_canonical_argument_register.ty.clone(),
                    &format!(
                        "temporary argument for '{}'",
                        target_canonical_argument_register.comment
                    ),
                );
                let copy_argument = CopyArgument {
                    canonical_target: target_canonical_argument_register.clone(),
                    source_temporary: temp_reg.register.clone(),
                };
                temp_to_abi_copies.push(copy_argument);
                temp_reg.register
            } else {
                target_canonical_argument_register.clone()
            };

            // Handle self variable (first parameter) vs regular arguments
            if index_in_signature == 0 && self_variable.is_some() {
                let self_reg = self_variable.as_ref().unwrap();
                if self_reg.index != argument_to_use.index {
                    self.builder.add_mov_reg(
                        &argument_to_use,
                        self_reg,
                        node,
                        &format!(
                            "move self_variable ({}) to first argument register",
                            self_reg.ty
                        ),
                    );
                }
            } else {
                // Regular argument - get from arguments array
                let argument_vector_index = if self_variable.is_some() {
                    index_in_signature - 1
                } else {
                    index_in_signature
                };
                let argument_expr_or_location = &arguments[argument_vector_index];

                self.emit_single_argument(
                    argument_expr_or_location,
                    &argument_to_use,
                    &target_canonical_argument_register,
                    &parameter_basic_type,
                    &mut copy_back_operations,
                    node,
                    ctx,
                );
            }
        }

        // Step 4: Copy from temporary registers to final ABI argument registers
        for (index, copy_argument) in temp_to_abi_copies.iter().enumerate() {
            let parameter_in_signature = &signature.parameters[index];
            self.builder.add_mov_reg(
                &copy_argument.canonical_target,
                &copy_argument.source_temporary,
                node,
                &format!(
                    "copy argument {index} ({}) in place from temporary '{}'",
                    parameter_in_signature.name, copy_argument.source_temporary.comment
                ),
            );
        }

        EmitArgumentInfo {
            argument_and_temp_scope: spill_scope,
            copy_back_of_registers_mutated_by_callee: copy_back_operations,
        }
    }

    pub(crate) fn emit_post_call(
        &mut self,
        spilled_arguments: EmitArgumentInfo,
        node: &Node,
        comment: &str,
    ) {
        // Phase 1: Save current mutable parameter values to temporary safe space before registers get clobbered
        let mut temp_saved_values = Vec::new();
        for copy_back in &spilled_arguments.copy_back_of_registers_mutated_by_callee {
            let temp_reg = self.temp_registers.allocate(
                copy_back.parameter_reg.ty.clone(),
                &format!(
                    "temp save for copy-back of {}",
                    copy_back.parameter_reg.comment
                ),
            );

            self.builder.add_mov_reg(
                temp_reg.register(),
                &copy_back.parameter_reg,
                node,
                &format!(
                    "save {} to temp before register restoration",
                    copy_back.parameter_reg
                ),
            );

            temp_saved_values.push((temp_reg, copy_back));
        }

        // Phase 2: Restore all spilled registers (temp registers first, then variables, then arguments)
        if let Some(scratch_region) = spilled_arguments.argument_and_temp_scope.scratch_registers {
            self.emit_restore_region(scratch_region, &HashSet::new(), node, comment);
        }

        // Restore argument registers - cool thing with this approach is that we don't have to bother with restoring some of them
        self.emit_restore_region(
            spilled_arguments.argument_and_temp_scope.argument_registers,
            &HashSet::new(),
            node,
            comment,
        );

        // Phase 3: Copy from temporary safe registers to the final destinations
        for (temp_reg, copy_back) in temp_saved_values {
            let temp_source = Destination::Register(temp_reg.register().clone());
            self.emit_copy_value_between_destinations(
                &copy_back.target_location_after_call,
                &temp_source,
                node,
                "copy-back from temp to final destination",
            );
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn emit_restore_region(
        &mut self,
        region: SpilledRegisterRegion,
        output_destination_registers: &HashSet<u8>, // TODO: Remove this
        node: &Node,
        comment: &str,
    ) {
        match region.registers {
            RepresentationOfRegisters::Individual(spilled_registers_list) => {
                if !spilled_registers_list.is_empty() {
                    let mut sorted_regs = spilled_registers_list;
                    sorted_regs.sort_by_key(|reg| reg.index);

                    // Filter out registers that are in output_destination_registers
                    let filtered_regs: Vec<_> = sorted_regs
                        .into_iter()
                        .filter(|reg| !output_destination_registers.contains(&reg.index))
                        .collect();

                    if !filtered_regs.is_empty() {
                        let mut i = 0;
                        while i < filtered_regs.len() {
                            let seq_start_idx = i;
                            let start_reg = filtered_regs[i].index;
                            let mut seq_length = 1;

                            while i + 1 < filtered_regs.len()
                                && filtered_regs[i + 1].index == filtered_regs[i].index + 1
                            {
                                seq_length += 1;
                                i += 1;
                            }

                            let memory_offset = if seq_start_idx > 0 {
                                (filtered_regs[seq_start_idx].index - filtered_regs[0].index)
                                    as usize
                                    * REG_ON_FRAME_SIZE.0 as usize
                            } else {
                                0
                            };

                            let specific_mem_location = FrameMemoryRegion {
                                addr: region.frame_memory_region.addr
                                    + swamp_vm_types::MemoryOffset(memory_offset as u32),
                                size: REG_ON_FRAME_SIZE,
                            };

                            self.builder.add_ld_contiguous_regs_from_frame(
                                start_reg,
                                specific_mem_location,
                                seq_length,
                                node,
                                &format!(
                                    "restoring r{}-r{} (sequence) {comment}",
                                    start_reg,
                                    start_reg + seq_length - 1
                                ),
                            );

                            i += 1;
                        }
                    }
                }
            }

            RepresentationOfRegisters::Mask(original_spill_mask) => {
                let mut mask_to_actually_restore = original_spill_mask;

                for i in 0..8 {
                    let reg_idx = i as u8;
                    if (original_spill_mask >> i) & 1 != 0
                        && output_destination_registers.contains(&reg_idx)
                    {
                        mask_to_actually_restore &= !(1 << i); // Clear the bit: don't restore this one
                    }
                }

                if mask_to_actually_restore != 0 {
                    self.builder.add_ld_masked_regs_from_frame(
                        mask_to_actually_restore,
                        region.frame_memory_region,
                        node,
                        &format!("restore registers using mask {comment}"),
                    );
                }
            }
            RepresentationOfRegisters::Range { start_reg, count } => {
                let base_mem_addr_of_spilled_range = region.frame_memory_region.addr;

                // Find contiguous sequences of registers that need to be restored
                let mut i = 0;
                while i < count {
                    while i < count && output_destination_registers.contains(&(start_reg + i)) {
                        i += 1;
                    }

                    if i < count {
                        let seq_start_reg = start_reg + i;
                        let seq_start_offset = (i as usize) * REG_ON_FRAME_SIZE.0 as usize;
                        let mut seq_length = 1;

                        while i + seq_length < count
                            && !output_destination_registers.contains(&(start_reg + i + seq_length))
                        {
                            seq_length += 1;
                        }

                        let specific_mem_location = FrameMemoryRegion {
                            addr: base_mem_addr_of_spilled_range
                                + swamp_vm_types::MemoryOffset(seq_start_offset as u32),
                            size: REG_ON_FRAME_SIZE,
                        };

                        self.builder.add_ld_contiguous_regs_from_frame(
                            seq_start_reg,
                            specific_mem_location,
                            seq_length,
                            node,
                            &format!(
                                "restoring spilled contiguous range of registers from stack frame r{}-r{} {comment}",
                                seq_start_reg,
                                seq_start_reg + seq_length - 1
                            ),
                        );

                        i += seq_length;
                    }
                }
            }
        }
    }

    pub(crate) fn emit_call(
        &mut self,
        node: &Node,
        internal_fn: &InternalFunctionDefinitionRef,
        comment: &str,
    ) {
        let function_name = internal_fn.associated_with_type.as_ref().map_or_else(
            || {
                format!(
                    "{}::{}",
                    pretty_module_name(&internal_fn.defined_in_module_path),
                    internal_fn.assigned_name
                )
            },
            |associated_with_type| {
                format!(
                    "{}::{}:{}",
                    pretty_module_name(&internal_fn.defined_in_module_path),
                    associated_with_type,
                    internal_fn.assigned_name
                )
            },
        );
        let call_comment = &format!("calling `{function_name}` ({comment})", );

        let patch_position = self.builder.add_call_placeholder(node, call_comment);
        self.state.function_fixups.push(FunctionFixup {
            patch_position,
            fn_id: internal_fn.program_unique_id,
            internal_function_definition: internal_fn.clone(),
        });
        //}
    }
    pub(crate) fn emit_internal_call(
        &mut self,
        target_reg: &Destination,
        node: &Node,
        internal_fn: &InternalFunctionDefinitionRef,
        arguments: &Vec<ArgumentExpression>,
        ctx: &Context,
    ) {
        let argument_info = self.emit_arguments(
            target_reg,
            node,
            &internal_fn.signature,
            None,
            arguments,
            false,
            ctx,
        );

        self.emit_call(node, internal_fn, "call"); // will be fixed up later

        self.emit_post_call(argument_info, node, "restore spilled after call");
    }

    /// If you're not on the last argument for "Outer Function", you have to put
    /// that value away somewhere safe for a bit. Otherwise, when you're figuring out the next arguments,
    /// you might accidentally overwrite it.
    /// But if you are on the last argument, you can just drop it right where it needs to go.
    const fn argument_needs_to_be_in_a_temporary_register_first(
        &self,
        reg: &TypedRegister,
    ) -> bool {
        // TODO: for now just assume it is
        true
    }
}
