use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::{FlagState, single_intrinsic_fn};
use swamp_semantic::{Function, Postfix, PostfixKind, StartOfChain, StartOfChainKind};
use swamp_types::Type;
use swamp_vm_types::types::{Destination, RValueOrLValue, VmType};
use swamp_vm_types::{MemoryLocation, MemoryOffset};

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

                    let call_return_destination = if is_last {
                        output_destination.clone()
                    } else {
                        // we are not at the end of the chain, create temporary
                        let return_type = &element.ty;
                        let return_basic_type = layout_type(return_type);

                        self.allocate_frame_space_and_return_destination_to_it(&return_basic_type, &element.node, "create temporary return destination for when not in the end of the chain")
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
                                    &call_return_destination,
                                    &start_expression.node,
                                    intrinsic_fn,
                                    Some(element.ty.clone()),
                                    Some(&RValueOrLValue::Scalar(
                                        absolute_self_pointer_register.clone(),
                                    )),
                                    &merged_arguments,
                                    ctx,
                                    "rvalue intrinsic call ",
                                );

                                if is_last {
                                    t_flag_result = z_result;
                                }
                            } else {
                                let argument_infos = self.emit_arguments(
                                    &call_return_destination,
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(&absolute_self_pointer_register),
                                    arguments,
                                    ctx,
                                );
                                self.emit_call(&element.node, internal_fn, "emit_rvalue call");

                                self.emit_post_call(
                                    argument_infos,
                                    &element.node,
                                    "emit_rvalue postcall",
                                );
                            }
                        }
                        Function::External(external_function_def) => {
                            self.emit_host_self_call(
                                &call_return_destination,
                                &start_expression.node,
                                external_function_def,
                                &absolute_self_pointer_register,
                                arguments,
                                ctx,
                            );
                        }
                        Function::Intrinsic(intrinsic_def) => {
                            let z_result = self.emit_single_intrinsic_call_with_self(
                                &call_return_destination,
                                &start_expression.node,
                                &intrinsic_def.intrinsic,
                                Some(element.ty.clone()),
                                Some(&RValueOrLValue::Scalar(absolute_self_pointer_register)),
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

                    current_location = call_return_destination.clone();

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

        let needs_final_load = !(
            // No load needed if the last element was a member call
            (!chain.is_empty() && matches!(chain.last().unwrap().kind, PostfixKind::MemberCall(_, _)))

                // No load needed if current location is already the right register
                || matches!(
        (output_destination, &current_location),
        (Destination::Register(out), Destination::Register(curr)) if out.index == curr.index
    )

                // No load needed for Unit destination
                || matches!(output_destination, Destination::Unit)
        );

        if needs_final_load {
            match output_destination {
                Destination::Register(output_reg) => {
                    if !matches!(current_location, Destination::Register(ref reg) if reg == output_reg)
                    {
                        self.emit_load_into_register(
                            output_reg,
                            &current_location,
                            &start_expression.node,
                            "rvalue postfix chain",
                        );
                    }
                }
                Destination::Memory(mem_loc) => {
                    if mem_loc.ty.is_represented_as_pointer_inside_register() {
                        // Complex type - we need to store to memory
                        self.emit_store_to_pointer_target(
                            &mem_loc.base_ptr_reg,
                            &current_location,
                            &start_expression.node,
                            "rvalue postfix chain to memory",
                        );
                    } else {
                        self.emit_load_into_register(
                            &mem_loc.base_ptr_reg,
                            &current_location,
                            &start_expression.node,
                            "load rvalue into temp register",
                        );

                        self.builder.add_st32_using_ptr_with_offset(
                            mem_loc,
                            &mem_loc.base_ptr_reg,
                            &start_expression.node,
                            "store from temp register to memory destination",
                        );
                    }
                }
                Destination::Unit => {}
            }
        }
    }

    pub(crate) fn emit_start_of_chain(
        &mut self,
        start: &StartOfChain,
        ctx: &Context,
    ) -> Destination {
        match &start.kind {
            StartOfChainKind::Expression(expr) => {
                let reg = self.emit_scalar_rvalue(expr, ctx);

                // If the register contains a primitive value directly, return it as is
                if !reg.ty.is_represented_as_pointer_inside_register() {
                    return Destination::Register(reg);
                }

                // For pointers to primitive values, return a memory location
                // This avoids unnecessary load instructions later
                Destination::Memory(MemoryLocation {
                    ty: reg.ty.clone(),
                    base_ptr_reg: reg,
                    offset: MemoryOffset(0),
                })
            }
            StartOfChainKind::Variable(variable) => {
                let variable_reg = self.get_variable_register(variable);

                // Same logic for variables - return memory location for pointers
                if !variable_reg.ty.is_represented_as_pointer_inside_register() {
                    return Destination::Register(variable_reg.clone());
                }

                Destination::Memory(MemoryLocation {
                    base_ptr_reg: variable_reg.clone(),
                    offset: MemoryOffset(0),
                    ty: variable_reg.ty.clone(),
                })
            }
        }
    }
}
