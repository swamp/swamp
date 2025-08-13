/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::single_intrinsic_fn;
use source_map_node::Node;
use swamp_semantic::{Function, Postfix, PostfixKind, StartOfChain, StartOfChainKind};
use swamp_vm_isa::MemoryOffset;
use swamp_vm_types::types::{u8_type, Destination, VmType};
use swamp_vm_types::MemoryLocation;

impl CodeBuilder<'_> {
    /// Handles the final load/conversion from `current_location` to `output_destination` if needed.
    ///
    fn emit_final_load_if_needed(
        &mut self,
        output_destination: &Destination,
        current_location: &Destination,
        node: &Node,
        chain: &[Postfix],
    ) {
        let needs_final_load = !(
            // No load needed if the last element was a member call
            // Member calls themselves return the correct value in the return
            (!chain.is_empty() && matches!(chain.last().unwrap().kind, PostfixKind::MemberCall(_, _)))

                // No load needed if current location is already the right register
                || matches!(
                    (output_destination, current_location),
                    (Destination::Register(out), Destination::Register(curr)) if out.index == curr.index
            )

                // No load needed for Unit destination
                || matches!(output_destination, Destination::Unit)
        );

        if !needs_final_load {
            return;
        }

        self.emit_copy_value_between_destinations(
            output_destination,
            current_location,
            node,
            "postfix chain final load",
        );
    }

    /// Handles writing None to the output destination for optional types
    fn emit_none_to_destination(&mut self, output_destination: &Destination, node: &Node) {
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
    /// Handles writing Some to the output destination for optional types
    fn emit_some_to_destination(&mut self, output_destination: &Destination, node: &Node) {
        match output_destination {
            Destination::Register(reg) => {
                let temp_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u8_type()),
                    "temp for Some tag",
                );
                self.builder.add_mov8_immediate(
                    temp_reg.register(),
                    1,
                    node,
                    "write Some tag to temp",
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
                    "store Some tag to memory",
                );
            }
            Destination::Memory(mem_loc) => {
                let temp_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u8_type()),
                    "temp for None tag",
                );
                self.builder.add_mov8_immediate(
                    temp_reg.register(),
                    1,
                    node,
                    "write Some tag to temp",
                );
                self.builder.add_st8_using_ptr_with_offset(
                    mem_loc,
                    temp_reg.register(),
                    node,
                    "store Some tag to memory",
                );
            }
            Destination::Unit => {}
        }
    }

    /* Code that might work in the future. haven't gotten it to work yet
                 PostfixKind::OptionalChainingOperator => {
                   original_optional_location = Some(current_location.clone());

                   let hwm = self.temp_registers.save_mark();

                   if let Destination::Memory(mem_loc) = &current_location {
                       let temp_reg = self.temp_registers.allocate(
                           VmType::new_unknown_placement(u8_type()),
                           "temp for optional tag",
                       );


                       // Load ONLY the tag byte
                       self.builder.add_ld8_from_pointer_with_offset_u16(
                           temp_reg.register(),
                           &mem_loc.base_ptr_reg,
                           mem_loc.offset + MemoryOffset(0),
                           &element.node,
                           "load optional tag byte",
                       );

                       // If None, jump to end of entire chain
                       let none_jump = self.builder.add_jmp_if_not_true_placeholder(
                           temp_reg.register(),
                           &element.node,
                           "jump if None to end of chain",
                       );
                       opt_none_jumps.push(none_jump);

                       // If Some, update current_location to payload
                       let optional_layout = current_location.ty();
                       let (_, _, payload_offset, _) = optional_layout.unwrap_info().unwrap();
                       current_location = current_location.add_offset(
                           payload_offset,
                           VmType::new_unknown_placement(
                               self.state.layout_cache.layout(&element.ty).clone(),
                           ),
                       );
                   }

                   if !is_last {
                       self.temp_registers.restore_to_mark(hwm);
                   }
               }
               PostfixKind::NoneCoalescingOperator(expression, optional_type, uncertain) => {
                   if let Destination::Memory(mem_loc) = &original_optional_location.clone().unwrap() {
                       let temp_reg = self.temp_registers.allocate(
                           VmType::new_unknown_placement(u8_type()),
                           "temp for coalesce tag",
                       );

                       // Load tag from current location
                       self.builder.add_ld8_from_pointer_with_offset_u16(
                           temp_reg.register(),
                           &mem_loc.base_ptr_reg,
                           mem_loc.offset + MemoryOffset(0),
                           &element.node,
                           "load tag for coalescing",
                       );

                       // Jump-if-Some -> skip fallback
                       let skip_fallback_expression_if_some = self.builder.add_jmp_if_true_placeholder(
                           temp_reg.register(),
                           &element.node,
                           "skip fallback expression if some (!= 0)",
                       );

                       // 2) Patch all previous '?' jumps to land at fallback emission
                       for jump in opt_none_jumps.drain(..) {
                           self.builder.patch_jump_here(jump);
                       }

                       let some_value_destination = if is_last {
                           output_destination.clone()
                       } else {
                           // intermediate: use a temp just like other chain ops
                           let ty = self.state.layout_cache.layout(&element.ty);
                           self.allocate_frame_space_and_return_destination_to_it(
                               &ty,
                               &element.node,
                               "temp for coalesce fallback",
                           )
                       };

                       // Emit fallback expression (None case)
                       self.emit_expression(&some_value_destination, expression, ctx);

                       let jump_over_extract_payload = self.builder.add_jump_placeholder(&element.node, "fallback is in place, jump over the extract payload");

                       // Some gets here
                       self.builder.patch_jump_here(skip_fallback_expression_if_some);

                       let layout = self.state.layout_cache.layout(&optional_type);
                       let (_, _, payload_offset, _) = layout.unwrap_info().unwrap();

                       let payload_loc = original_optional_location.clone().unwrap()
                           .add_offset(
                               payload_offset,
                               VmType::new_unknown_placement(
                                   self.state.layout_cache.layout(&element.ty).clone(),
                               ));

                       self.emit_copy_value_between_destinations(
                           &some_value_destination,
                           &payload_loc,
                           &element.node,
                           "coalesce payload copy",
                       );

                       self.builder.patch_jump_here(jump_over_extract_payload);
                       current_location = some_value_destination.clone();
                   } else {
                       panic!("should have been a memory location");
                   }
               }
           }
       }

       // End of chain: handle any remaining opt_none_jumps
       if !opt_none_jumps.is_empty() {
           for jump in opt_none_jumps {
               self.builder.patch_jump_here(jump);
           }
           // Emit None to output
           self.emit_none_to_destination(output_destination, &chain.last().unwrap().node);
       }

       // Perform final load/conversion if needed
       self.emit_final_load_if_needed(
           output_destination,
           &current_location,
           &start_expression.node,
           chain,
       );
    */

    /*
    if let Destination::Memory(mem_loc) = &current_location {
                        let hwm = self.temp_registers.save_mark();
                        // Load and test the tag byte directly from memory
                        // Ensure we don't reuse the base_ptr_reg for the tag load
                        let tag_reg = self
                            .temp_registers
                            .allocate(VmType::new_unknown_placement(u8_type()), "?? temp for tag");


                        self.builder.add_ld8_from_pointer_with_offset_u16(
                            tag_reg.register(),
                            &mem_loc.base_ptr_reg,
                            mem_loc.offset + MemoryOffset(0),
                            &element.node,
                            "?? load optional tag",
                        );


                        // None coalescing must perform a load, no matter what

                        let some_value_destination = if is_last {
                            output_destination.clone()
                        } else {
                            // intermediate: use a temp just like other chain ops
                            let ty = self.state.layout_cache.layout(&element.ty);
                            self.allocate_frame_space_and_return_destination_to_it(
                                &ty,
                                &element.node,
                                "?? temp for coalesce fallback",
                            )
                        };


                        // If P=1 (Some), skip fallback
                        let skip_fallback_patch = self.builder.add_jmp_if_true_placeholder(
                            tag_reg.register(),
                            &element.node,
                            "?? jump if 1 (Some)",
                        );

                        // 2) Patch all previous '?' jumps to land at fallback emission
                        for jump in none_patches.drain(..) {
                            self.builder.patch_jump_here(jump);
                        }

                        // None case: evaluate the fallback expression
                        self.emit_expression(&some_value_destination, expression, ctx);

                        // Jump over the loading of the payload, we have already emitted a fallback expression
                        let skip_over_load_from_some_payload = self.builder.add_jump_placeholder(&element.node, "skip load from optional Some payload");

                        // If fallback wasn't needed since it was a Some value jump here
                        self.builder.patch_jump_here(skip_fallback_patch);

                        //let optional_layout = &current_location.vm_type().unwrap().basic_type;
                        let optional_layout = self.state.layout_cache.layout(&optional_type);
                        if let TypeKind::Optional(optional_inner) = &*optional_type.kind {
                            let optional_inner_layout = self.state.layout_cache.layout(&optional_inner);
                            let unwrap_info_result = optional_layout.unwrap_info();
                            if unwrap_info_result.is_none() {
                                error!(?optional_layout, "problem with type");
                            }
                            let (_, _, payload_offset, _) = unwrap_info_result.unwrap();

                            let payload_location = current_location.add_offset(payload_offset, VmType::new_unknown_placement(optional_inner_layout));


                            self.emit_copy_value_between_destinations(
                                &some_value_destination, &payload_location, &element.node, "comment",
                            );

                            if !is_last {
                                self.temp_registers.restore_to_mark(hwm);
                            }
                        } else {
                            panic!("internal error")
                        }

                        self.builder.patch_jump_here(skip_over_load_from_some_payload);
                    } else {
                        panic!("Optional value should always be in memory");
                    }
     */

    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_postfix_chain(
        &mut self,
        initial_output_destination: &Destination,
        start_expression: &StartOfChain,
        chain: &[Postfix],
        ctx: &Context,
    ) {
        let mut current_location = self.emit_start_of_chain(start_expression, ctx);
        let mut optional_chaining_none_patches = Vec::new();

        let has_any_optional_chaining = chain
            .iter()
            .any(|x| matches!(x.kind, PostfixKind::OptionalChainingOperator));

        let (tag_output_destination, payload_output_destination) = if has_any_optional_chaining {
            let (tag_offset, _, payload_offset, _) =
                initial_output_destination.ty().unwrap_info().unwrap();
            let optional_inner_type = &initial_output_destination.ty().get_variant(1).ty;

            let tag_destination = initial_output_destination
                .add_offset(tag_offset, VmType::new_unknown_placement(u8_type()));
            let payload_destination = initial_output_destination.add_offset(
                payload_offset,
                VmType::new_unknown_placement(optional_inner_type.clone()),
            );
            (Some(tag_destination), Some(payload_destination))
        } else {
            (None, None)
        };

        let output_destination = initial_output_destination;

        //info!(t=?current_location.vm_type(), "start r value chain");

        for (index, element) in chain.iter().enumerate() {
            //info!(t=?element.ty, index, t=?current_location.vm_type(), ?element.kind, "chain element");
            let is_last = index == chain.len() - 1;

            match &element.kind {
                PostfixKind::StructField(anonymous_struct, field_index) => {
                    let struct_layout = self.state.layout_cache.layout(anonymous_struct);

                    let offset_item = struct_layout.get_field_offset(*field_index).unwrap();

                    current_location = current_location.add_offset(
                        offset_item.offset,
                        VmType::new_unknown_placement(offset_item.ty.clone()),
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

                PostfixKind::VecSubscriptRange(_vec_type, _range_expression) => {
                    // Implement this in the future when we can handle "true" views into another vec
                    todo!()
                }

                PostfixKind::GridSubscript(grid_type, x_int_expression, y_int_expression) => {
                    current_location = self.grid_subscript_helper(
                        &current_location,
                        &grid_type.element,
                        x_int_expression,
                        y_int_expression,
                        ctx,
                    );
                }

                PostfixKind::SparseSubscript(vec_type, int_expression) => {
                    current_location = self.sparse_subscript_helper(
                        &current_location,
                        &vec_type.element,
                        int_expression,
                        ctx,
                    );
                }

                PostfixKind::MapSubscript(map_type, key_expression) => {
                    current_location = self.map_subscript_helper(
                        &current_location,
                        map_type,
                        key_expression,
                        false,
                        ctx,
                    );
                }

                PostfixKind::MemberCall(function_to_call, arguments) => {
                    // For chained intrinsic calls, materialize the input value into a register first
                    // to prevent it from being overwritten when we allocate the output destination
                    let materialized_input = if !is_last
                        && matches!(function_to_call.as_ref(), Function::Internal(internal_fn) if single_intrinsic_fn(&internal_fn.body).is_some())
                    {
                        Some(self.emit_load_scalar_or_absolute_aggregate_pointer(
                            &current_location,
                            &element.node,
                            "materialize input for chained intrinsic to prevent register conflicts",
                        ))
                    } else {
                        None
                    };

                    let call_return_destination = if is_last {
                        output_destination.clone()
                    } else {
                        // we are not at the end of the chain, create temporary
                        let return_type = &element.ty;
                        let return_basic_type = self.state.layout_cache.layout(return_type);

                        self.allocate_frame_space_and_return_destination_to_it(&return_basic_type, &element.node, "create temporary return destination for when not in the end of the chain")
                    };

                    match function_to_call.as_ref() {
                        Function::Internal(internal_fn) => {
                            if let Some((intrinsic_fn, intrinsic_arguments)) =
                                single_intrinsic_fn(&internal_fn.body)
                            {
                                let merged_arguments = Self::merge_arguments_keep_literals(
                                    arguments,
                                    intrinsic_arguments,
                                );

                                // Use the materialized input if available, otherwise use the current location
                                if let Some(materialized) = materialized_input {
                                    if let Some(reg) = materialized {
                                        self.emit_single_intrinsic_call_with_self(
                                            &call_return_destination,
                                            &start_expression.node,
                                            intrinsic_fn,
                                            Some(&reg),
                                            &merged_arguments,
                                            ctx,
                                            "rvalue intrinsic call ",
                                        );
                                    } else {
                                        self.emit_single_intrinsic_call_with_self_destination(
                                            &call_return_destination,
                                            &start_expression.node,
                                            intrinsic_fn,
                                            Some(&current_location),
                                            &merged_arguments,
                                            ctx,
                                            "rvalue intrinsic call ",
                                        );
                                    }
                                } else {
                                    self.emit_single_intrinsic_call_with_self_destination(
                                        &call_return_destination,
                                        &start_expression.node,
                                        intrinsic_fn,
                                        Some(&current_location),
                                        &merged_arguments,
                                        ctx,
                                        "rvalue intrinsic call ",
                                    );
                                }
                            } else {
                                let absolute_self_pointer_register = self
                                    .emit_compute_effective_address_to_register(
                                        &current_location,
                                        &element.node,
                                        "resolve to absolute pointer before member call",
                                    );

                                // Then set up the arguments
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
                            let absolute_self_pointer_register = self
                                .emit_compute_effective_address_to_register(
                                    &current_location,
                                    &element.node,
                                    "resolve to absolute pointer before member call",
                                );
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
                            self.emit_single_intrinsic_call_with_self_destination(
                                &call_return_destination,
                                &start_expression.node,
                                &intrinsic_def.intrinsic,
                                Some(&current_location),
                                arguments,
                                ctx,
                                "rvalue intrinsic call ",
                            );
                        }
                    }

                    current_location = call_return_destination.clone();

                    //info!(?current_location, "after member call");
                }
                PostfixKind::OptionalChainingOperator => {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(u8_type()),
                        "?. temp for tag",
                    );

                    let mem_location = current_location.memory_location().unwrap();

                    self.builder.add_ld8_from_pointer_with_offset(
                        temp_reg.register(),
                        &mem_location.base_ptr_reg,
                        mem_location.offset + MemoryOffset(0),
                        &element.node,
                        "?. load optional tag byte",
                    );

                    let skip_if_none = self.builder.add_jmp_if_not_true_placeholder(
                        temp_reg.register(),
                        &element.node,
                        "?. jump if None. should be to the set None tag",
                    );

                    optional_chaining_none_patches.push(skip_if_none);

                    // Dive into the payload, rest of postfix expects it
                    let optional_layout = current_location.ty();
                    let (_, _, payload_offset, _) = optional_layout.unwrap_info().unwrap();

                    let payload_type = &element.ty.clone();

                    current_location = current_location.add_offset(
                        payload_offset,
                        VmType::new_unknown_placement(
                            self.state.layout_cache.layout(payload_type).clone(),
                        ),
                    );
                }
            }
        }

        let node = &chain.last().unwrap().node;

        let we_did_optional_chaining = !optional_chaining_none_patches.is_empty();

        if we_did_optional_chaining {
            let last_ty = &chain.last().unwrap().ty;

            // If the last encountered type is already an option
            // we can not wrap it. Swamp do not allow for
            // optionals within optionals
            if last_ty.is_option() {
                // The payload is already an option tagged union.
                // Just copy the whole tag+payload over.
                self.emit_copy_value_between_destinations(
                    &tag_output_destination.clone().unwrap(),
                    &current_location, // tag+payload
                    node,
                    "propagate inner option record",
                );
            } else {
                // If we reach here, then it was a value that can safely be wrapped in Some
                // Write the tag to 1
                self.emit_some_to_destination(&tag_output_destination.clone().unwrap(), node);

                // Emit current location to the payload
                self.emit_copy_value_between_destinations(
                    &payload_output_destination.unwrap(),
                    &current_location,
                    &start_expression.node,
                    "please materialize into the option payload",
                );
            }
        } else {
            // It was a "normal"
            // Perform final load/conversion if needed
            self.emit_final_load_if_needed(
                output_destination,
                &current_location,
                &start_expression.node,
                chain,
            );
        }

        let patch_jump_over_none = if we_did_optional_chaining {
            Some(
                self.builder
                    .add_jump_placeholder(node, "jump over none case"),
            )
        } else {
            None
        };

        // After all chain processing is done, patch all None jumps to here
        if we_did_optional_chaining {
            // First patch all the None jumps to this position
            for patch in optional_chaining_none_patches {
                self.builder.patch_jump_here(patch);
            }

            self.emit_none_to_destination(&tag_output_destination.unwrap(), node);
        }

        if let Some(skip_none_patch) = patch_jump_over_none {
            self.builder.patch_jump_here(skip_none_patch);
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
                if variable_reg.ty.is_reg_copy() {
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
