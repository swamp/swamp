use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::{layout_optional_type, layout_type};
use crate::single_intrinsic_fn;
use swamp_semantic::{Function, Postfix, PostfixKind, StartOfChain, StartOfChainKind};
use swamp_types::Type;
use swamp_vm_types::types::{Destination, RValueOrLValue, VmType, u8_type};
use swamp_vm_types::{MemoryLocation, MemoryOffset, PatchPosition};
use tracing::info;

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
        let mut none_patches = Vec::new();
        let mut none_coalesce_final_load_skip: Option<PatchPosition> = None;

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
                PostfixKind::SliceViewSubscript(slice_type, int_expression) => {
                    current_location = self.vec_subscript_helper(
                        &current_location,
                        &slice_type.element,
                        int_expression,
                        ctx,
                    );
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
                    info!(?map_type, "map_subscript");
                    current_location = self.map_subscript_helper(
                        &current_location,
                        &map_type,
                        key_expression,
                        ctx,
                    );
                    info!(?current_location, ?map_type, "map_subscript");
                }

                PostfixKind::MemberCall(function_to_call, arguments) => {
                    let hwm = self.temp_registers.save_mark();

                    let absolute_self_pointer_register = self
                        .emit_compute_effective_address_to_register(
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
                            } else {
                                let argument_infos = self.emit_arguments(
                                    &call_return_destination,
                                    &start_expression.node,
                                    &internal_fn.signature,
                                    Some(&absolute_self_pointer_register),
                                    arguments,
                                    false,
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
                            self.emit_single_intrinsic_call_with_self(
                                &call_return_destination,
                                &start_expression.node,
                                &intrinsic_def.intrinsic,
                                Some(element.ty.clone()),
                                Some(&RValueOrLValue::Scalar(absolute_self_pointer_register)),
                                arguments,
                                ctx,
                                "rvalue intrinsic call ",
                            );
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
                    let hwm = self.temp_registers.save_mark();

                    // Load and check the optional tag
                    let resolved_location = self.emit_materialize_value_to_register(
                        &current_location,
                        &element.node,
                        "load optional tag",
                    );

                    self.builder.add_tst_u8(
                        resolved_location.register(),
                        &element.node,
                        "test if optional tag is None",
                    );

                    // If None, add patch to jump to end of all processing
                    let skip_to_end_patch = self
                        .builder
                        .add_jmp_if_equal_placeholder(&element.node, "jump if None");
                    none_patches.push(skip_to_end_patch);

                    // If Some, update current_location to point to the payload
                    let optional_layout = layout_optional_type(&element.ty);
                    let (_, _, payload_offset, _) = optional_layout.unwrap_info().unwrap();

                    let payload_type = &element.ty.clone();

                    current_location = current_location.add_offset(
                        payload_offset,
                        VmType::new_unknown_placement(layout_type(payload_type).clone()),
                    );

                    if !is_last {
                        self.temp_registers.restore_to_mark(hwm);
                    }
                }
                PostfixKind::NoneCoalescingOperator(expression) => {
                    assert!(is_last, "None coalescing operator must be last in chain");

                    // Load and test the tag byte directly from memory
                    let temp_reg = self
                        .temp_registers
                        .allocate(VmType::new_unknown_placement(u8_type()), "temp for tag");
                    if let Destination::Memory(mem_loc) = &current_location {
                        self.builder.add_ld8_from_pointer_with_offset_u16(
                            temp_reg.register(),
                            &mem_loc.base_ptr_reg,
                            MemoryOffset(0),
                            &element.node,
                            "load optional tag",
                        );

                        self.builder.add_tst_u8(
                            temp_reg.register(),
                            &element.node,
                            "test tag - sets T=0 if None (0), T=1 if Some (1)",
                        );

                        // If T=1 (Some), skip fallback
                        let skip_fallback_patch = self
                            .builder
                            .add_jmp_if_true_placeholder(&element.node, "jump if T=1 (Some)");

                        // None case: evaluate the fallback expression
                        self.emit_expression(output_destination, expression, ctx);

                        // Jump past the final load block to prevent fallback from being clobbered
                        // TODO: Support multiple ?? expressions in the future.
                        let skip_final_load = self
                            .builder
                            .add_jump_placeholder(&element.node, "jump past final load");

                        self.builder.patch_jump_here(skip_fallback_patch);

                        let optional_layout = layout_optional_type(&element.ty);
                        let (_, _, payload_offset, _) = optional_layout.unwrap_info().unwrap();

                        // `Some`: just update current_location to point to payload
                        current_location = current_location.add_offset(
                            payload_offset,
                            VmType::new_unknown_placement(layout_type(&element.ty).clone()),
                        );

                        none_coalesce_final_load_skip = Some(skip_final_load);
                    } else {
                        panic!("Optional value should always be in memory");
                    }
                }
            }

            //info!(t=?element.ty, index, t=?current_location.vm_type(), ?element.kind, "after element");
        }

        // After all chain processing is done, patch all None jumps to here
        if !none_patches.is_empty() {
            let node = &chain.last().unwrap().node;

            // First patch all the None jumps to this position
            for patch in none_patches {
                self.builder.patch_jump_here(patch);
            }

            // Then write None to the output destination
            match output_destination {
                Destination::Register(reg) => {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_unknown_placement(u8_type()),
                        "temp for None tag",
                    );
                    self.builder.add_mov8_immediate(
                        temp_reg.register(),
                        0,
                        node,
                        "write None tag to temp",
                    );
                    let memory_location = MemoryLocation {
                        base_ptr_reg: reg.clone(),
                        offset: MemoryOffset(0),
                        ty: VmType::new_unknown_placement(u8_type()),
                    };
                    self.builder.add_st8_using_ptr_with_offset(
                        &memory_location,
                        temp_reg.register(),
                        node,
                        "store None tag to memory",
                    );
                }
                Destination::Memory(mem_loc) => {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_unknown_placement(u8_type()),
                        "temp for None tag",
                    );
                    self.builder.add_mov8_immediate(
                        temp_reg.register(),
                        0,
                        node,
                        "write None tag to temp",
                    );
                    self.builder.add_st8_using_ptr_with_offset(
                        mem_loc,
                        temp_reg.register(),
                        node,
                        "store None tag to memory",
                    );
                }
                Destination::Unit => {}
            }
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
                    if output_destination.ty().is_aggregate() {
                        let absolute_pointer_reg = self.emit_compute_effective_address_to_register(
                            &current_location,
                            &start_expression.node,
                            "after postfix we need absolute pointer",
                        );
                        self.builder.add_mov_reg(
                            output_reg,
                            &absolute_pointer_reg,
                            &start_expression.node,
                            &format!("{} move absolute pointer in place", ctx.comment()),
                        );
                    } else if !matches!(current_location, Destination::Register(ref reg) if reg == output_reg)
                    {
                        self.emit_transfer_value_to_register(
                            output_reg,
                            &current_location,
                            &start_expression.node,
                            "rvalue postfix chain",
                        );
                    }
                }
                Destination::Memory(mem_loc) => {
                    if mem_loc.ty.is_aggregate() {
                        // Complex type - we need to store to memory
                        self.emit_store_value_to_memory_destination(
                            output_destination,
                            &current_location,
                            &start_expression.node,
                            "rvalue postfix chain to memory",
                        );
                    } else {
                        let rhs_value_temp = self.temp_registers.allocate(
                            current_location.vm_type().clone(),
                            "end of chain, load primitive to target",
                        );

                        self.emit_transfer_value_to_register(
                            rhs_value_temp.register(),
                            &current_location,
                            &start_expression.node,
                            "end of chain, load primitive into temp register",
                        );

                        self.emit_store_scalar_to_memory_offset_instruction(
                            mem_loc,
                            rhs_value_temp.register(),
                            &start_expression.node,
                            "store from temp primitive register to final memory destination",
                        );
                    }
                }
                Destination::Unit => {}
            }
        }

        // If we have a None coalescing jump to skip the final load, patch it here
        if let Some(skip_final_load) = none_coalesce_final_load_skip {
            self.builder.patch_jump_here(skip_final_load);
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
                if !reg.ty.is_aggregate() {
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
                if !variable_reg.ty.is_aggregate() {
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
