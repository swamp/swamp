/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Expression, WhenBinding};
use swamp_types::TypeKind;
use swamp_vm_types::MemoryLocation;
use swamp_vm_types::types::{Place, VmType, u8_type};

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_when(
        &mut self,
        target_reg: &Place,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) {
        let mut all_false_jumps = Vec::new();

        for binding in bindings {
            let binding_gen_type = self.state.layout_cache.layout(&binding.expr.ty);
            let (tag_offset, ..) = binding_gen_type.unwrap_info().unwrap();
            let base_aggregate = self.emit_expression_as_aggregate_pointer(&binding.expr, ctx);
            let hwm = self.temp_registers.save_mark();
            let tag_reg = self
                .temp_registers
                .allocate(VmType::new_unknown_placement(u8_type()), "tag value");

            self.builder.add_ld8_from_pointer_with_offset(
                tag_reg.register(),
                &base_aggregate.location.base_ptr_reg,
                tag_offset,
                &binding.expr.node,
                "load tag value",
            );

            let patch = self.builder.add_jmp_if_not_equal_placeholder(
                tag_reg.register(),
                &binding.expr.node,
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
            let optional_type = &binding.expr.ty;
            let payload_memory_offset = match &*optional_type.kind {
                TypeKind::Optional(_inner_type) => {
                    // We have an Optional type, get the layout info
                    let binding_gen_type = self.state.layout_cache.layout(optional_type);
                    let (_, _, payload_offset, _) = binding_gen_type.unwrap_info().unwrap();
                    payload_offset
                }
                _ => panic!("Expected Optional type in when binding"),
            };

            // Get the source memory location
            let base_aggregate = self.emit_expression_as_aggregate_pointer(&binding.expr, ctx);

            let source_memory_location = MemoryLocation {
                base_ptr_reg: base_aggregate.location.base_ptr_reg,
                offset: payload_memory_offset,
                ty: target_binding_variable_reg.ty.clone(),
            };

            // For when expressions, we always extract the value, never create references
            let target_destination = if target_binding_variable_reg.ty.is_scalar() {
                Place::Register(target_binding_variable_reg)
            } else {
                Place::Memory(MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                    target_binding_variable_reg,
                ))
            };

            let source_destination = Place::Memory(source_memory_location);
            self.emit_copy_value_from_memory_location(
                &target_destination,
                &source_destination.memory_location_or_pointer_reg(),
                &binding.expr.node,
                "load payload into binding variable",
            );
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
