/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{ArgumentExpression, Expression, WhenBinding};
use swamp_types::TypeKind;
use swamp_vm_types::MemoryLocation;
use swamp_vm_types::types::{Destination, RValueOrLValue, VmType, u8_type};

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_when(
        &mut self,
        target_reg: &Destination,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) {
        let mut all_false_jumps = Vec::new();

        for binding in bindings {
            let binding_gen_type = self.state.layout_cache.layout(&binding.expr.ty());
            let (tag_offset, ..) = binding_gen_type.unwrap_info().unwrap();
            let old_variable_region = self.emit_for_access_or_location(&binding.expr, ctx);
            let hwm = self.temp_registers.save_mark();
            let tag_reg = self
                .temp_registers
                .allocate(VmType::new_unknown_placement(u8_type()), "tag value");
            match old_variable_region {
                RValueOrLValue::Scalar(base_reg) => {
                    self.builder.add_ld8_from_pointer_with_offset_u16(
                        tag_reg.register(),
                        &base_reg,
                        tag_offset,
                        binding.expr.node(),
                        "load tag value",
                    );
                }
                RValueOrLValue::Memory(destination) => {
                    let base_ptr_reg = self
                        .emit_compute_effective_address_from_location_to_register(
                            &destination.memory_location_or_pointer_reg(),
                            &binding.variable.name,
                            "load effective address",
                        );
                    self.builder.add_ld8_from_pointer_with_offset_u16(
                        tag_reg.register(),
                        &base_ptr_reg.ptr_reg,
                        tag_offset,
                        binding.expr.node(),
                        "load tag value",
                    );
                }
            }

            let patch = self.builder.add_jmp_if_not_equal_placeholder(
                tag_reg.register(),
                binding.expr.node(),
                "jump if none",
            );

            all_false_jumps.push(patch);

            self.temp_registers.restore_to_mark(hwm);
        }

        // Here we are sure that all optional types are `Some`, so we load the payloads into the binding variables.
        for binding in bindings {
            let target_binding_variable_reg = self.get_variable_register(&binding.variable).clone();
            self.initialize_variable_the_first_time(&binding.variable); // make sure the target is initialized with correct pointer

            // Get the optional type information to find the payload offset
            let optional_type = binding.expr.ty();
            let payload_memory_offset = match &*optional_type.kind {
                TypeKind::Optional(inner_type) => {
                    // We have an Optional type, get the layout info
                    let binding_gen_type = self.state.layout_cache.layout(&optional_type);
                    let (_, _, payload_offset, _) = binding_gen_type.unwrap_info().unwrap();
                    payload_offset
                }
                _ => panic!("Expected Optional type in when binding"),
            };

            // Get the source memory location
            let source_memory_location = match self.emit_for_access_or_location(&binding.expr, ctx)
            {
                RValueOrLValue::Scalar(base_reg) => MemoryLocation {
                    base_ptr_reg: base_reg,
                    offset: payload_memory_offset,
                    ty: target_binding_variable_reg.ty.clone(),
                },
                RValueOrLValue::Memory(source_loc) => {
                    let aggregate_mem_location =
                        source_loc.grab_aggregate_memory_location_or_pointer_reg(); //self.emit_compute_effective_address_to_register(&destination, &binding.variable.name, "load effective address");
                    let payload_mem_location = aggregate_mem_location.offset(
                        payload_memory_offset,
                        target_binding_variable_reg.ty.basic_type.clone(),
                    );
                    payload_mem_location.location
                }
            };

            // Check if this is a mutable reference that should be treated as an alias
            let is_mutable_reference = match &binding.expr {
                ArgumentExpression::BorrowMutableReference(_) => true,
                _ => false,
            };

            if is_mutable_reference {
                // For mutable references, just store the pointer to the payload
                // This creates an alias instead of copying the data
                let ptr_loc = self.emit_compute_effective_address_from_location_to_register(
                    &source_memory_location,
                    binding.expr.node(),
                    "get address of payload for mutable reference",
                );

                // Copy the computed address to the target register
                self.builder.add_mov_reg(
                    &target_binding_variable_reg,
                    &ptr_loc.ptr_reg,
                    binding.expr.node(),
                    "store payload address in binding variable (alias)",
                );
            } else {
                // For normal expressions, block copy the payload into the binding variable
                let target_destination = if target_binding_variable_reg.ty.is_scalar() {
                    Destination::Register(target_binding_variable_reg)
                } else {
                    Destination::Memory(MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                        target_binding_variable_reg,
                    ))
                };

                let source_destination = Destination::Memory(source_memory_location);
                self.emit_copy_value_between_destinations(
                    &target_destination,
                    &source_destination,
                    binding.expr.node(),
                    "load payload into binding variable",
                );
            }
        }

        self.emit_expression(target_reg, true_expr, ctx);
        let maybe_jump_over_false = if let Some(else_expr) = maybe_false_expr {
            Some(
                self.builder
                    .add_jump_placeholder(&else_expr.node, "jump over false section"),
            )
        } else {
            None
        };

        for false_jump_patch in all_false_jumps {
            self.builder.patch_jump_here(false_jump_patch);
        }

        if let Some(else_expr) = maybe_false_expr {
            self.emit_expression(target_reg, else_expr, ctx);
            self.builder.patch_jump_here(maybe_jump_over_false.unwrap());
        }
    }
}
