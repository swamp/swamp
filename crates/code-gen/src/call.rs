//! `CodeBuilder` helper functions for function calls and arguments.
use crate::SpilledRegister;
use crate::code_bld::{CodeBuilder, MutableReturnReg};
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::reg_pool::RegisterPool;
use crate::state::FunctionFixup;
use source_map_node::Node;
use swamp_semantic::{InternalFunctionDefinitionRef, MutRefOrImmutableExpression};
use swamp_types::Signature;
use swamp_vm_types::types::{Destination, TypedRegister, VmType};
use tracing::info;

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_arguments(
        &mut self,
        output_destination: &Destination,
        node: &Node,
        signature: &Signature,
        self_variable: Option<&TypedRegister>,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> (Vec<SpilledRegister>, Vec<crate::code_bld::MutableReturnReg>) {
        let mut all_mutable_arguments_including_hidden = Vec::new();

        let mut copy_back_mutable_reg_pairs = Vec::new();

        if !signature.return_type.is_unit() {
            let return_basic_type = layout_type(&signature.return_type);
            info!(?return_basic_type, "r0: makes decision for r0");

            if return_basic_type.is_represented_as_a_pointer_in_reg() {
                let r0 = TypedRegister::new_vm_type(
                    0,
                    VmType::new_unknown_placement(return_basic_type.clone()),
                );

                let return_pointer_reg = self.emit_absolute_pointer_if_needed(
                    output_destination,
                    node,
                    "create an absolute pointer to r0 if needed",
                );

                info!(
                    ?return_pointer_reg,
                    ?return_basic_type,
                    "r0: it is a pointer, so we copy the absolute pointer into it"
                );

                self.builder.add_mov_reg(
                    &r0,
                    &return_pointer_reg,
                    node,
                    "r0: copy the return pointer into r0",
                );
            } else if let Destination::Register(return_param_reg) = output_destination {
                info!(
                    ?return_param_reg,
                    ?return_basic_type,
                    "r0: it is a simple type, so we need to copy back the register"
                );

                copy_back_mutable_reg_pairs.push(MutableReturnReg {
                    target_location_after_call: Destination::Register(return_param_reg.clone()),
                    parameter_reg: return_param_reg.clone(),
                });
                // For primitive returns to registers, add the register to the hidden arguments
                all_mutable_arguments_including_hidden.push(return_param_reg);
            }
        }
        let mut argument_registers = RegisterPool::new(1, 10);

        let mut protected_argument_registers = Vec::new();
        let mut spilled_arguments = Vec::new();

        for (index_in_signature, type_for_parameter) in signature.parameters.iter().enumerate() {
            let parameter_basic_type = layout_type(&type_for_parameter.resolved_type);
            let argument_register = argument_registers.alloc_register(
                VmType::new_unknown_placement(parameter_basic_type.clone()),
                &format!("emit argument {index_in_signature}"),
            );

            if ctx.register_is_protected(&argument_register) {
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
                let argument_vector_index = if self_variable.is_some() {
                    index_in_signature - 1
                } else {
                    index_in_signature
                };
                let argument_expr_or_location = &arguments[argument_vector_index];
                let debug_pos = self.builder.position();

                match argument_expr_or_location {
                    MutRefOrImmutableExpression::Location(lvalue) => {
                        let detailed_location = self.emit_lvalue_address(lvalue, ctx);
                        if parameter_basic_type.should_be_copied_back_when_mutable_arg_or_return() {
                            copy_back_mutable_reg_pairs.push(crate::code_bld::MutableReturnReg {
                                target_location_after_call: detailed_location,
                                parameter_reg: argument_register.clone(),
                            });
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
        for copy_back in copy_back {
            match &copy_back.target_location_after_call {
                Destination::Register(reg) => {
                    self.builder
                        .add_mov_reg(reg, &copy_back.parameter_reg, node, "copy back reg");
                }
                Destination::Memory(memory_location) => {} /* TODO:
                self.store_register_contents_to_memory(
                node,
                base_ptr_reg,
                 *offset,
                &copy_back.parameter_reg,
                "copy back from mem",
                ),
                 */
                Destination::Unit => {}
            }
        }
        self.emit_restore_spilled_registers(spilled_arguments, node, comment);
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
                    "{:?}::{}",
                    internal_fn.defined_in_module_path, internal_fn.assigned_name
                )
            },
            |associated_with_type| {
                format!(
                    "{:?}::{}:{}",
                    internal_fn.defined_in_module_path,
                    associated_with_type,
                    internal_fn.assigned_name
                )
            },
        );
        let call_comment = &format!("calling {function_name} ({comment})",);
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
