//! `CodeBuilder` helper functions for function calls and arguments.

use crate::code_bld::{CodeBuilder, MutableReturnReg};
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::reg_pool::RegisterPool;
use crate::state::FunctionFixup;
use crate::SpilledRegister;
use seq_map::SeqMap;
use source_map_node::Node;
use swamp_semantic::{
    pretty_module_name, InternalFunctionDefinitionRef, MutRefOrImmutableExpression,
};
use swamp_types::Signature;
use swamp_vm_types::types::{BasicTypeKind, Destination, TypedRegister, VmType};
use swamp_vm_types::MemoryLocation;

impl CodeBuilder<'_> {
    /// Checks if an argument is already in the correct register and doesn't need to be spilled
    /// This is purely for optimization, it doesn't change the actual outcome.
    fn is_argument_already_in_correct_register(
        &self,
        index_in_signature: usize,
        argument_register: &TypedRegister,
        self_variable: Option<&TypedRegister>,
        _argument_expr_or_location: Option<&MutRefOrImmutableExpression>,
        argument_vector_index: usize,
        arguments: &[MutRefOrImmutableExpression],
    ) -> bool {
        if index_in_signature == 0 && self_variable.is_some() {
            let self_reg = self_variable.as_ref().unwrap();
            self_reg.index == argument_register.index
        } else if index_in_signature > 0 && argument_vector_index < arguments.len() {
            match &arguments[argument_vector_index] {
                MutRefOrImmutableExpression::Location(lvalue) => {
                    if lvalue.access_chain.is_empty() {
                        let var_reg = self.get_variable_register(&lvalue.starting_variable);
                        var_reg.index == argument_register.index
                    } else {
                        false
                    }
                }
                MutRefOrImmutableExpression::Expression(expr) => {
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

    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_arguments(
        &mut self,
        output_destination: &Destination,
        node: &Node,
        signature: &Signature,
        self_variable: Option<&TypedRegister>,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> (Vec<SpilledRegister>, Vec<MutableReturnReg>) {
        let mut copy_back_mutable_reg_pairs: Vec<MutableReturnReg> = Vec::new();

        let mut saved_base_ptr_cache: SeqMap<(usize, u8), TypedRegister> = SeqMap::new();

        // Track the return pointer register that needs to be preserved
        let mut return_pointer_reg_to_preserve: Option<TypedRegister> = None;

        if !signature.return_type.is_unit() {
            let return_basic_type = layout_type(&signature.return_type);

            if return_basic_type.is_represented_as_a_pointer_in_reg() {
                let r0 =
                    TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));

                let return_pointer_reg = self.emit_absolute_pointer_if_needed(
                    output_destination,
                    node,
                    "create an absolute pointer to r0 if needed",
                );

                self.builder.add_mov_reg(
                    &r0,
                    &return_pointer_reg,
                    node,
                    "r0: copy the return pointer into r0",
                );

                return_pointer_reg_to_preserve = Some(return_pointer_reg);
            } else {
                // For simple types, we need to copy from r0 to destination after the call
                let r0 =
                    TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));

                copy_back_mutable_reg_pairs.push(MutableReturnReg {
                    target_location_after_call: output_destination.clone(),
                    parameter_reg: r0,
                });
            }
        }
        let mut argument_registers = RegisterPool::new(1, 10);

        let mut protected_argument_registers = Vec::new();
        let mut spilled_arguments = Vec::new();

        // Spill the return pointer register if it exists and will be overwritten by arguments
        // TODO: Would be so much more fun to have a more coherent way of handling the registers, spilling and pinning.
        if let Some(return_pointer_reg) = &return_pointer_reg_to_preserve {
            let save_region = self.temp_frame_space_for_register("emit_arguments_return_ptr");
            self.builder.add_st_regs_to_frame(
                save_region.addr,
                return_pointer_reg,
                1,
                node,
                &format!("spill return pointer register to stack memory {return_pointer_reg:?}"),
            );
            spilled_arguments.push(SpilledRegister {
                register: return_pointer_reg.clone(),
                frame_memory_region: save_region,
            });
        }

        for (index_in_signature, type_for_parameter) in signature.parameters.iter().enumerate() {
            let parameter_basic_type = layout_type(&type_for_parameter.resolved_type);
            let argument_register = argument_registers.alloc_register(
                VmType::new_unknown_placement(parameter_basic_type.clone()),
                &format!("emit argument {index_in_signature}"),
            );

            // Determine if we need to spill this register
            let argument_vector_index = if self_variable.is_some() {
                index_in_signature.saturating_sub(1)
            } else {
                index_in_signature
            };

            let already_in_correct_register = self.is_argument_already_in_correct_register(
                index_in_signature,
                &argument_register,
                self_variable,
                None,
                argument_vector_index,
                arguments,
            );

            let needs_spill =
                ctx.register_is_protected(&argument_register) && !already_in_correct_register;
            if needs_spill {
                let save_region = self.temp_frame_space_for_register("emit_arguments");
                self.builder.add_st_regs_to_frame(
                    save_region.addr,
                    &argument_register,
                    1,
                    node,
                    &format!("spill register to stack memory {argument_register:?}"),
                );
                spilled_arguments.push(SpilledRegister {
                    register: argument_register.clone(),
                    frame_memory_region: save_region,
                });
            }

            let mut argument_ctx = ctx.clone();
            argument_ctx.add_protected_registers(&protected_argument_registers);

            if index_in_signature == 0 && self_variable.is_some() {
                let self_reg = self_variable.as_ref().unwrap();

                if self_reg.index != argument_register.index {
                    self.builder.add_mov_reg(
                        &argument_register,
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
                let debug_pos = self.builder.position();

                match argument_expr_or_location {
                    MutRefOrImmutableExpression::Location(lvalue) => {
                        let detailed_location = self.emit_lvalue_address(lvalue, ctx);

                        // Flag to track if we need to save the base pointer
                        let mut saved_base_ptr = None;

                        let is_primitive = matches!(
                            parameter_basic_type.kind,
                            BasicTypeKind::S32
                                | BasicTypeKind::Fixed32
                                | BasicTypeKind::U32
                                | BasicTypeKind::U8
                                | BasicTypeKind::B8
                                | BasicTypeKind::U16
                        );

                        if is_primitive {
                            if parameter_basic_type
                                .should_be_copied_back_when_mutable_arg_or_return()
                            {
                                // I missed that it will cause a lot of extra complexity for mutable arguments
                                // that are passed by value and copied back.
                                if let Destination::Memory(mem_location) = &detailed_location {
                                    // Try to find an existing saved base pointer for this structure
                                    let base_id =
                                        lvalue.starting_variable.unique_id_within_function;
                                    let base_reg_index = mem_location.base_ptr_reg.index;
                                    let cache_key = (base_id, base_reg_index);

                                    let saved_reg = if let Some(existing_reg) =
                                        saved_base_ptr_cache.get(&cache_key)
                                    {
                                        existing_reg.clone()
                                    } else {
                                        let temp_reg =
                                            self.temp_registers.allocate_pinned_register(
                                                mem_location.base_ptr_reg.ty.clone(),
                                                "save base pointer for copy-back",
                                            );

                                        self.builder.add_mov_reg(
                                            temp_reg.register(),
                                            &mem_location.base_ptr_reg,
                                            node,
                                            "save base ptr before overwriting registers",
                                        );

                                        let save_region = self.temp_frame_space_for_register(
                                            "emit_arguments_saved_base_ptr",
                                        );

                                        self.builder.add_st_regs_to_frame(
                                            save_region.addr,
                                            temp_reg.register(),
                                            1,
                                            node,
                                            &format!("spill saved base pointer register to stack memory {:?}", temp_reg.register()),
                                        );

                                        let spilled_reg = SpilledRegister {
                                            register: temp_reg.register().clone(),
                                            frame_memory_region: save_region,
                                        };
                                        spilled_arguments.push(spilled_reg);

                                        saved_base_ptr_cache
                                            .insert(cache_key, temp_reg.register().clone())
                                            .unwrap();

                                        temp_reg.register().clone()
                                    };

                                    // Registers for the cache must be pinned
                                    self.temp_registers.pin_register(&saved_reg);

                                    // TODO: @performance. Maybe accumulate add_st_regs_to_frame.
                                    if !spilled_arguments
                                        .iter()
                                        .any(|a| a.register.index == saved_reg.index)
                                    {
                                        let save_region = self.temp_frame_space_for_register(
                                            "emit_arguments_cached_base_ptr",
                                        );
                                        self.builder.add_st_regs_to_frame(
                                            save_region.addr,
                                            &saved_reg,
                                            1,
                                            node,
                                            &format!("spill cached base pointer register to stack memory {saved_reg:?}"),
                                        );

                                        spilled_arguments.push(SpilledRegister {
                                            register: saved_reg.clone(),
                                            frame_memory_region: save_region,
                                        });
                                    }

                                    self.builder.add_load_primitive(
                                        &argument_register,
                                        &saved_reg,
                                        mem_location.offset,
                                        node,
                                        "emit primitive value using saved base ptr",
                                    );

                                    saved_base_ptr = Some((saved_reg, mem_location.offset));
                                } else {
                                    self.emit_load_into_register(
                                        &argument_register,
                                        &detailed_location,
                                        node,
                                        "load primitive value into argument register",
                                    );
                                }
                            } else {
                                self.emit_load_into_register(
                                    &argument_register,
                                    &detailed_location,
                                    node,
                                    "load primitive value into argument register",
                                );
                            }
                        } else {
                            let abs_pointer = self.emit_absolute_pointer_if_needed(
                                &detailed_location,
                                node,
                                "calculate absolute address for struct field reference",
                            );

                            self.builder.add_mov_reg(
                                &argument_register,
                                &abs_pointer,
                                node,
                                "load calculated address of lvalue into argument register",
                            );
                        }

                        if parameter_basic_type.should_be_copied_back_when_mutable_arg_or_return() {
                            if let Some((saved_reg, offset)) = saved_base_ptr {
                                copy_back_mutable_reg_pairs.push(MutableReturnReg {
                                    target_location_after_call: Destination::Memory(
                                        MemoryLocation {
                                            base_ptr_reg: saved_reg,
                                            offset,
                                            ty: VmType::new_contained_in_register(
                                                parameter_basic_type.clone(),
                                            ),
                                        },
                                    ),
                                    parameter_reg: argument_register.clone(),
                                });
                            } else {
                                copy_back_mutable_reg_pairs.push(MutableReturnReg {
                                    target_location_after_call: detailed_location,
                                    parameter_reg: argument_register.clone(),
                                });
                            }
                        }
                    }
                    MutRefOrImmutableExpression::Expression(expr) => {
                        self.emit_expression_into_register(
                            &argument_register,
                            expr,
                            "argument expression into specific argument register",
                            ctx,
                        );
                    }
                }

                if debug_pos == self.builder.position() {
                    // eprintln!("problem with {argument_expr_or_location:?}");
                }
                //assert_ne!(debug_pos, self.builder.position());
            }

            protected_argument_registers.push(argument_register.clone());
        }

        (spilled_arguments, copy_back_mutable_reg_pairs)
    }

    pub(crate) fn emit_post_call(
        &mut self,
        spilled_arguments: &[SpilledRegister],
        copy_back: &[MutableReturnReg],
        node: &Node,
        comment: &str,
    ) {
        let mut registers_to_unpin = Vec::new();

        for copy_back in copy_back {
            // TODO: add a helper function, maybe called `copy_back_to_memory_or_reg` or similar.
            match &copy_back.target_location_after_call {
                Destination::Register(reg) => {
                    self.builder.add_mov_reg(
                        reg,
                        &copy_back.parameter_reg,
                        node,
                        "copy return value from r0 to register",
                    );
                }
                Destination::Memory(memory_location) => {
                    match memory_location.ty.basic_type.kind {
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
                    }
                    // For memory destinations, store the return value from r0 to the memory location

                    registers_to_unpin.push(memory_location.base_ptr_reg.clone());
                }
                Destination::Unit => {
                    panic!("should not happen")
                }
            }
        }

        self.emit_restore_spilled_registers(spilled_arguments, node, comment);

        // all copy operations are complete at this point, unpin all registers
        for reg in registers_to_unpin {
            self.temp_registers.unpin_register(&reg);
        }
    }

    pub fn emit_restore_spilled_registers(
        &mut self,
        spilled_arguments: &[SpilledRegister],
        node: &Node,
        comment: &str,
    ) {
        for arg in spilled_arguments {
            self.builder.add_ld_regs_from_frame(
                &arg.register,
                arg.frame_memory_region.addr,
                1, // TODO: Calculate a count for all spilled registers
                node,
                &format!("restoring spilled arguments {comment}"),
            );
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
        let call_comment = &format!("calling {function_name} ({comment})", );
        /*
               if let Some(found) = self
                   .state
                   .function_infos
                   .get(&internal_fn.program_unique_id)
               {
                   self.builder.add_call(
                       &InstructionPosition(found.ip_range.start.0.saturating_sub(1)),
                       node,
                       call_comment,
                   );
               } else {

        */
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
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) {
        let (spilled_arguments, copy_back) = self.emit_arguments(
            target_reg,
            node,
            &internal_fn.signature.signature,
            None,
            arguments,
            ctx,
        );

        self.emit_call(node, internal_fn, "call"); // will be fixed up later

        self.emit_post_call(
            &spilled_arguments,
            &copy_back,
            node,
            "restore spilled after call",
        );
    }
}
