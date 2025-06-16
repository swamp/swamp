/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

//! `CodeBuilder` helper functions for function calls and arguments.

use crate::code_bld::{CodeBuilder, EmitArgumentInfo, MutableReturnReg};
use crate::ctx::Context;
use crate::reg_pool::RegisterPool;
use crate::state::FunctionFixup;
use crate::{ArgumentAndTempScope, RepresentationOfRegisters, SpilledRegisterRegion};
use seq_map::SeqMap;
use source_map_node::Node;
use std::collections::HashSet;
use swamp_semantic::{pretty_module_name, ArgumentExpression, InternalFunctionDefinitionRef};
use swamp_types::prelude::Signature;
use swamp_vm_types::types::{
    u32_type, BasicType, BasicTypeKind, Destination, TypedRegister, VmType,
};
use swamp_vm_types::{FrameMemoryRegion, MemoryLocation, REG_ON_FRAME_SIZE};

pub struct CopyArgument {
    pub canonical_target: TypedRegister,
    pub source_temporary: TypedRegister,
}

impl CodeBuilder<'_> {
    fn is_argument_already_in_correct_register(
        &self,
        index_in_signature: usize,
        argument_register: &TypedRegister,
        self_variable: Option<&TypedRegister>,
        _argument_expr_or_location: Option<&ArgumentExpression>,
        argument_vector_index: usize,
        arguments: &[ArgumentExpression],
    ) -> bool {
        if index_in_signature == 0 && self_variable.is_some() {
            let self_reg = self_variable.as_ref().unwrap();
            self_reg.index == argument_register.index
        } else if index_in_signature > 0 && argument_vector_index < arguments.len() {
            match &arguments[argument_vector_index] {
                ArgumentExpression::BorrowMutableReference(lvalue) => {
                    if lvalue.access_chain.is_empty() {
                        let var_reg = self.get_variable_register(&lvalue.starting_variable);
                        var_reg.index == argument_register.index
                    } else {
                        false
                    }
                }
                ArgumentExpression::Expression(expr) => {
                    if let swamp_semantic::ExpressionKind::VariableAccess(var_ref) = &expr.kind {
                        let var_reg = self.get_variable_register(var_ref);
                        var_reg.index == argument_register.index
                    } else {
                        false
                    }
                }
            }
        } else {
            false
        }
    }

    pub fn find_replacements_for_mutable_primitive_arguments(
        &mut self,
        arguments: &[ArgumentExpression],
        node: &Node,
        ctx: &Context,
    ) -> SeqMap<u8, TypedRegister> {
        let mut stable_base_ptr_cache = SeqMap::new();
        for argument in arguments {
            if let ArgumentExpression::BorrowMutableReference(lvalue) = argument {
                let parameter_basic_type = layout_type(&argument.ty());
                if parameter_basic_type.should_be_copied_back_when_mutable_arg_or_return() {
                    // first we need to save the base register into a temporary register
                    // so it won't be clobbered
                    // TODO: We should keep track if we have done this for an earlier base reg with the same index
                    let original_destination = self.emit_lvalue_address(lvalue, ctx);
                    let original_base_reg = original_destination
                        .register_involved_in_destination()
                        .unwrap();
                    if !stable_base_ptr_cache.contains_key(&original_base_reg.index) {
                        let replacement_base_reg = self.temp_registers.allocate(
                            VmType::new_contained_in_register(u32_type()),
                            "temporary sav reg",
                        );
                        self.builder.add_mov_reg(
                            replacement_base_reg.register(),
                            original_base_reg,
                            node,
                            "copy the base reg in a temporary register",
                        );
                        stable_base_ptr_cache
                            .insert(
                                original_base_reg.index,
                                replacement_base_reg.register.clone(),
                            )
                            .unwrap();
                    }
                }
            }
        }

        stable_base_ptr_cache
    }

    // TODO: for mutable arguments we want to leave output part of the spilling
    // We want to scan through the arguments,
    // see if there are mutable primitives
    // if there are, create temporary registers and save them
    // to a lookup cache. those replacements will be used for load and save operations
    // of those primitives.
    // the temporary registers will be saved and restored since the HWM has been increased.
    // Which is exactly what we want. so for example the r128 holds a valid address for the
    // store operation.
    //
    // So the sequence is:
    // - store abi registers (do not save r0 if that is the destination for the call itself, since we want to clobber it in that case)
    // - store temp registers (including the newly created replacement base regs)
    // - call (callee is allowed to clobber r0, r1, r2, r128)
    // - restore temp registers (so our r128 is valid again)
    // - issue copy backs of the mutables primitives. (replacement) base reg is valid, and the r1 and r2 are clobbered, which is good.
    // - restore abi registers
    // - copy back of r0 for immutable primitives to target register,  unless the target was r0 itself, in that case it was left out of store abi register mask
    pub fn spill_required_registers(
        &mut self,
        r0_used_as_return: bool,
        is_host_call: bool,
        node: &Node,
        comment: &str,
    ) -> ArgumentAndTempScope {
        const ABI_ARGUMENT_RETURN_AND_ARGUMENT_REGISTERS: usize = 6; // r0-r5
        const ABI_ARGUMENT_MASK: u8 = 0x3f;

        let mask_to_use = if r0_used_as_return {
            ABI_ARGUMENT_MASK & 0x7E
        } else {
            ABI_ARGUMENT_MASK
        };

        let abi_parameter_frame_memory_region = self.temp_frame_space_for_register(
            ABI_ARGUMENT_RETURN_AND_ARGUMENT_REGISTERS as u8,
            &format!("emit abi arguments r0-r5 {comment}"),
        );
        self.builder.add_st_masked_regs_to_frame(
            abi_parameter_frame_memory_region.addr,
            ABI_ARGUMENT_MASK,
            node,
            "spill masked registers to stack frame memory",
        );

        let abi_parameter_region = SpilledRegisterRegion {
            registers: RepresentationOfRegisters::Mask(mask_to_use),
            frame_memory_region: abi_parameter_frame_memory_region,
        };

        let (first_temp_register_index, temp_register_probable_live_count) =
            self.temp_registers.start_index_and_number_of_allocated();
        debug_assert_eq!(first_temp_register_index, 128);

        let temp_register_region = if temp_register_probable_live_count > 0 && !is_host_call {
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

    pub fn prepare_copy_back_for_primitive_return_value(
        &mut self,
        output_destination: &Destination,
        copy_back_mutable_reg_pairs: &mut Vec<MutableReturnReg>,
        return_basic_type: BasicType,
    ) {
        // For simple types, we need to copy from r0 to destination after the call
        let r0 = TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));

        copy_back_mutable_reg_pairs.push(MutableReturnReg {
            target_location_after_call: output_destination.clone(),
            parameter_reg: r0,
        });
    }
    pub fn setup_return_pointer_reg(
        &mut self,
        output_destination: &Destination,
        return_basic_type: BasicType,
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

    #[allow(clippy::too_many_lines)]
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
        let mut copy_back_phase_one: Vec<MutableReturnReg> = Vec::new();
        //let mut copy_back_phase_two: Vec<MutableReturnReg> = Vec::new();

        let base_reg_replacement_lookup =
            self.find_replacements_for_mutable_primitive_arguments(arguments, node, ctx);

        let r0_is_used_as_return = !signature.return_type.is_unit();

        let scope =
            self.spill_required_registers(false, is_host_call, node, "spill before emit arguments"); // TODO: when to use r0_is_used_as_return

        // Handle return primitive or aggregate types
        if r0_is_used_as_return {
            let return_basic_type = layout_type(&signature.return_type);

            if return_basic_type.is_aggregate() {
                self.setup_return_pointer_reg(output_destination, return_basic_type, node);
            } else {
                self.prepare_copy_back_for_primitive_return_value(
                    output_destination,
                    &mut copy_back_phase_one,
                    return_basic_type,
                );
            }
        }

        let mut copy_arguments_in_place = Vec::new();

        let mut argument_registers = RegisterPool::new(1, 10);

        for (index_in_signature, type_for_parameter) in signature.parameters.iter().enumerate() {
            let parameter_basic_type = layout_type(&type_for_parameter.resolved_type);
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
                copy_arguments_in_place.push(copy_argument);
                temp_reg.register
            } else {
                target_canonical_argument_register.clone()
            };

            // Determine if we need to spill this register
            let argument_vector_index = if self_variable.is_some() {
                index_in_signature.saturating_sub(1)
            } else {
                index_in_signature
            };

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
                let argument_expr_or_location = &arguments[argument_vector_index];

                match argument_expr_or_location {
                    ArgumentExpression::BorrowMutableReference(lvalue) => {
                        let original_destination = self.emit_lvalue_address(lvalue, ctx);

                        if parameter_basic_type.should_be_copied_back_when_mutable_arg_or_return() {
                            // first we need to save the base register into a temporary register
                            // so it won't be clobbered
                            // TODO: We should keep track if we have done this for an earlier base reg with the same index
                            let original_base_reg = original_destination
                                .register_involved_in_destination()
                                .unwrap();

                            let base_reg_to_use = base_reg_replacement_lookup
                                .get(&original_base_reg.index)
                                .expect("could not find base reg");

                            // Construct a replacement location with the new temporary register (replacement_base_reg)
                            let replacement_memory_location = MemoryLocation {
                                base_ptr_reg: base_reg_to_use.clone(),
                                offset: original_destination.grab_memory_location().offset,
                                ty: original_destination.vm_type().clone(),
                            };
                            let replacement_location =
                                Destination::Memory(replacement_memory_location);

                            // Load the primitive from memory
                            self.emit_transfer_value_to_register(
                                &argument_to_use,
                                &replacement_location,
                                node,
                                "must get primitive from lvalue and pass as copy back (by value)",
                            );

                            // Add a copy back with the replacement_location back to the primitive location
                            copy_back_phase_one.push(MutableReturnReg {
                                target_location_after_call: replacement_location,
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
                                &argument_to_use,
                                &flattened_source_pointer_reg,
                                node,
                                "copy absolute address",
                            );
                        }
                    }
                    ArgumentExpression::Expression(expr) => {
                        self.emit_expression_into_register(
                            &argument_to_use,
                            expr,
                            "argument expression into specific argument register",
                            ctx,
                        );
                    }
                }
            }
        }

        for (index, copy_argument) in copy_arguments_in_place.iter().enumerate() {
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
            argument_and_temp_scope: scope,
            copy_back_of_registers_mutated_by_callee: copy_back_phase_one,
        }
    }

    pub fn copy_backs(&mut self, copy_backs: Vec<MutableReturnReg>, node: &Node) -> HashSet<u8> {
        let mut all_registers_in_copy_back = HashSet::new();
        for copy_back in copy_backs {
            if let Some(some_reg) = copy_back
                .target_location_after_call
                .register_involved_in_destination()
            {
                all_registers_in_copy_back.insert(some_reg.index);
            }

            // TODO: add a helper function, maybe called `copy_back_to_memory_or_reg` or similar.
            match &copy_back.target_location_after_call {
                Destination::Register(reg) => {
                    self.builder.add_mov_reg(
                        reg,
                        &copy_back.parameter_reg,
                        node,
                        &format!(
                            "copy return value to {reg} frp, {}",
                            copy_back.parameter_reg
                        ),
                    );
                }
                Destination::Memory(memory_location) => match memory_location.ty.basic_type.kind {
                    BasicTypeKind::U8 | BasicTypeKind::B8 => {
                        self.builder.add_st8_using_ptr_with_offset(
                            memory_location,
                            &copy_back.parameter_reg,
                            node,
                            &format!(
                                "copy byte return value from {} to memory destination",
                                copy_back.parameter_reg
                            ),
                        );
                    }
                    _ => {
                        self.builder.add_st32_using_ptr_with_offset(
                            memory_location,
                            &copy_back.parameter_reg,
                            node,
                            &format!(
                                "copy return value from {} to memory destination",
                                copy_back.parameter_reg
                            ),
                        );
                    }
                },
                Destination::Unit => {
                    panic!("should not happen")
                }
            }
        }
        all_registers_in_copy_back
    }

    pub(crate) fn emit_post_call(
        &mut self,
        spilled_arguments: EmitArgumentInfo,
        node: &Node,
        comment: &str,
    ) {
        if let Some(scratch_region) = spilled_arguments.argument_and_temp_scope.scratch_registers {
            self.emit_restore_region(scratch_region, &HashSet::new(), node, comment);
        }

        let all_registers_in_copy_back = self.copy_backs(
            spilled_arguments.copy_back_of_registers_mutated_by_callee,
            node,
        );

        self.emit_restore_region(
            spilled_arguments.argument_and_temp_scope.argument_registers,
            &all_registers_in_copy_back,
            node,
            comment,
        );
    }

    #[allow(clippy::too_many_lines)]
    // TODO:@investigate:
    // Instead of filtering out individual registers that should not be restored
    // It seems to be better to make those decision and store the proper regions in the first place
    // Not sure what is the fastest approach. Maybe there is a slight advantage to spilling "too much"
    // as it is written now. Since you have unbroken ranges in the spilling, it takes less instructions.
    pub fn emit_restore_region(
        &mut self,
        region: SpilledRegisterRegion,
        output_destination_registers: &HashSet<u8>,
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
                        &format!("restore (filtered by output dest) using mask {comment}"),
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
