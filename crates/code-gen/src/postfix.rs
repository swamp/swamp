use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::{FlagState, single_intrinsic_fn};
use swamp_semantic::{Function, Postfix, PostfixKind, StartOfChain};
use swamp_types::Type;
use swamp_vm_types::types::{Destination, TypedRegister, VmType};

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_postfix_chain(
        &mut self,
        output_destination: &Destination,
        start_expression: &StartOfChain,
        chain: &[Postfix],
        ctx: &Context,
    ) {
        let mut current_location = self.emit_start_of_chain(start_expression, ctx);
        let mut t_flag_result = FlagState::default();

        //info!(t=?current_location.vm_type(), "start r value chain");

        for (index, element) in chain.iter().enumerate() {
            //info!(t=?element.ty, index,t=?current_location.vm_type(), ?element.kind, "chain element");
            let is_last = index == chain.len() - 1;
            match &element.kind {
                PostfixKind::StructField(anonymous_struct, field_index) => {
                    let struct_layout =
                        layout_type(&Type::AnonymousStruct(anonymous_struct.clone()));

                    let offset_item = struct_layout.get_field_offset(*field_index).unwrap();

                    current_location = current_location.add_offset(
                        offset_item.offset,
                        VmType::new_unknown_placement(offset_item.ty.clone()),
                    );
                }
                PostfixKind::SliceSubscript(slice_type, int_expression) => {
                    let element_basic_type = layout_type(&slice_type.element);

                    todo!()
                    /*
                    current_location = self.subscript_helper_from_location_to_location(
                        current_location,
                        &element_basic_type,
                        int_expression,
                        BoundsCheck::KnownSizeAtCompileTime(slice_type.fixed_size as u16),
                        &int_expression.node,
                        "emit rvalue",
                        ctx,
                    );

                     */
                }

                PostfixKind::VecSubscript(vec_type, int_expression) => {
                    current_location = self.vec_subscript_helper(
                        &current_location,
                        &vec_type.element,
                        int_expression,
                        ctx,
                    );
                }

                PostfixKind::MapSubscript(map_type, key_expression) => {
                    current_location = self.map_subscript_helper(
                        &current_location,
                        &map_type.key,
                        key_expression,
                        ctx,
                    );
                }

                PostfixKind::MemberCall(function_to_call, arguments) => {
                    let hwm = self.temp_registers.save_mark();

                    let absolute_self_pointer_register = self.emit_absolute_pointer_if_needed(
                        &current_location,
                        &element.node,
                        "resolve to absolute pointer before member call",
                    );

                    let target_destination = if is_last {
                        output_destination
                    } else {
                        let temp_reg =
                            self.temp_register_for_analyzed_type(&element.ty, "chain_result");
                        &Destination::Register(temp_reg.register)
                    };

                    match &**function_to_call {
                        Function::Internal(internal_fn) => {
                            if let Some((intrinsic_fn, intrinsic_arguments)) =
                                single_intrinsic_fn(&internal_fn.body)
                            {
                                //info!(?intrinsic_fn, "intrinsic");
                                let merged_arguments = Self::merge_arguments_keep_literals(
                                    arguments,
                                    intrinsic_arguments,
                                );

                                let z_result = self.emit_single_intrinsic_call_with_self(
                                    target_destination,
                                    &start_expression.node,
                                    intrinsic_fn,
                                    Some(element.ty.clone()),
                                    Some(&absolute_self_pointer_register),
                                    &merged_arguments,
                                    ctx,
                                    "rvalue intrinsic call ",
                                );

                                if is_last {
                                    t_flag_result = z_result;
                                }
                            } else {
                                let return_type = &internal_fn.signature.signature.return_type;
                                let return_basic_type = layout_type(return_type);

                                // HACK: TODO: If the target is a memory location, we need to set r0 properly
                                match target_destination {
                                    Destination::Memory(mem_loc) => {
                                        // For a memory destination, we need to set up r0 to point to the memory
                                        // where the result should be stored
                                        let return_reg = self.temp_registers.allocate(
                                            VmType::new_unknown_placement(
                                                return_basic_type.clone(),
                                            ),
                                            "member_call_return_value",
                                        );

                                        self.builder.add_mov_reg(
                                            return_reg.register(),
                                            &mem_loc.base_ptr_reg,
                                            &element.node,
                                            "setting up return register to point to destination memory"
                                        );

                                        Some(return_reg)
                                    }
                                    _ => None,
                                };

                                if let Destination::Memory(mem_loc) = target_destination {
                                    // HACK: For memory destination setup r0 to point to destination memory
                                    let return_type = &internal_fn.signature.signature.return_type;
                                    let return_basic_type = layout_type(return_type);

                                    if return_basic_type.is_represented_as_a_pointer_in_reg() {
                                        let r0 = TypedRegister {
                                            index: 0, // r0
                                            ty: VmType::new_unknown_placement(
                                                return_basic_type.clone(),
                                            ),
                                            comment: "return value register".to_string(),
                                        };

                                        self.builder.add_mov_reg(
                                            &r0,
                                            &mem_loc.base_ptr_reg,
                                            &element.node,
                                            "setting up r0 to point to destination memory for struct return"
                                        );
                                    }
                                }

                                let (spilled_argument_registers, copy_back) = self.emit_arguments(
                                    target_destination,
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(&absolute_self_pointer_register),
                                    arguments,
                                    ctx,
                                );
                                self.emit_call(&element.node, internal_fn, "emit_rvalue call");

                                self.emit_post_call(
                                    &spilled_argument_registers,
                                    &copy_back,
                                    &element.node,
                                    "emit_rvalue postcall",
                                );
                            }
                        }
                        Function::External(external_function_def) => {
                            self.emit_host_self_call(
                                target_destination,
                                &start_expression.node,
                                external_function_def,
                                &absolute_self_pointer_register,
                                arguments,
                                ctx,
                            );
                        }
                        Function::Intrinsic(intrinsic_def) => {
                            let z_result = self.emit_single_intrinsic_call_with_self(
                                target_destination,
                                &start_expression.node,
                                &intrinsic_def.intrinsic,
                                Some(element.ty.clone()),
                                Some(&absolute_self_pointer_register),
                                arguments,
                                ctx,
                                "rvalue intrinsic call ",
                            );

                            if is_last {
                                t_flag_result = z_result;
                            }
                        }
                        _ => panic!(
                            "{}",
                            &format!("not supported as a member call {function_to_call:?}")
                        ),
                    }

                    if is_last {
                        // For the last element, we should use the output destination
                        // but ensure we have the correct type information from element.ty
                        current_location = output_destination.clone();
                    } else {
                        current_location = target_destination.clone();
                    }

                    if !is_last {
                        self.temp_registers.restore_to_mark(hwm);
                    }

                    //info!(?current_location, "after member call");
                }
                PostfixKind::OptionalChainingOperator => {
                    todo!()
                }
                PostfixKind::NoneCoalescingOperator(expression) => {
                    let hwm = self.temp_registers.save_mark();
                    let temp_reg = self.temp_register_for_analyzed_type(&expression.ty, "");

                    // materialize an u8
                    let resolved_location = self
                        .emit_load_primitive_from_detailed_location_if_needed(
                            &current_location,
                            &element.node,
                            "",
                        );

                    self.builder.add_tst_u8(
                        resolved_location.register(),
                        &element.node,
                        "test if optional tag is Some",
                    );

                    let patch = self
                        .builder
                        .add_jmp_if_equal_placeholder(&element.node, "jump if some");

                    /*
                    if let DetailedLocationResolved::TempRegister(temp_reg) = resolved_location {
                        self.temp_registers.free(temp_reg);
                    }

                     */

                    self.emit_expression(output_destination, expression, ctx);

                    self.temp_registers.restore_to_mark(hwm);

                    self.builder.patch_jump_here(patch);

                    current_location =
                        Destination::Register(output_destination.grab_register().clone());
                    //info!(?current_location, "after none coalesce");
                }
            }

            //info!(t=?element.ty, index, t=?current_location.vm_type(), ?element.kind, "after element");
        }

        match output_destination {
            Destination::Register(output_reg) => {
                if !matches!(current_location, Destination::Register(ref reg) if reg == output_reg)
                {
                    self.emit_load_from_location(
                        output_reg,
                        &current_location,
                        &start_expression.node,
                        "rvalue postfix chain",
                    );
                }
            }
            Destination::Memory(mem_loc) => {
                self.emit_load_from_location(
                    &mem_loc.base_ptr_reg,
                    &current_location,
                    &start_expression.node,
                    "rvalue postfix chain to memory",
                );
            }
            Destination::Unit => {
                // No need to load anything if the destination is Unit
            }
        }
    }
}
