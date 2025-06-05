/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use swamp_semantic::{ArgumentExpression, Expression, WhenBinding};
use swamp_vm_types::types::{Destination, RValueOrLValue, VmType, u8_type};
use swamp_vm_types::{MemoryLocation, MemoryOffset};

impl CodeBuilder<'_> {
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
            let binding_gen_type = layout_type(&binding.expr.ty());
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
                    let memory_location = destination.grab_memory_location();
                    self.builder.add_ld8_from_pointer_with_offset_u16(
                        tag_reg.register(),
                        &memory_location.base_ptr_reg,
                        memory_location.offset + tag_offset,
                        binding.expr.node(),
                        "load tag value",
                    );
                }
            }

            self.builder
                .add_tst_reg(tag_reg.register(), binding.expr.node(), "check tag value");

            self.temp_registers.restore_to_mark(hwm);

            let patch = self
                .builder
                .add_jmp_if_not_equal_placeholder(binding.expr.node(), "jump if none");

            all_false_jumps.push(patch);
        }

        for binding in bindings {
            let target_binding_variable_reg = self.get_variable_register(&binding.variable).clone();

            if binding.has_expression() {
                self.emit_argument_expression_binding(
                    &target_binding_variable_reg,
                    &binding.expr,
                    ctx,
                );
            } else {
                let ArgumentExpression::Expression(variable_access_expression) = &binding.expr
                else {
                    panic!("must be expression");
                };
                let optional_tagged_union_reg =
                    self.emit_scalar_rvalue(variable_access_expression, ctx);

                let tagged_union_binding = optional_tagged_union_reg.ty.underlying();
                let tagged_union = tagged_union_binding.optional_info().unwrap();

                // Optional Tagged Union Payload is grabbed.
                // if it is aggregate type, we block copy it,
                // if it is a primitive we load it into a register
                if target_binding_variable_reg.ty.is_aggregate() {
                    // get the location of the variable
                    let target_memory_location = MemoryLocation {
                        ty: target_binding_variable_reg.ty.clone(),
                        base_ptr_reg: target_binding_variable_reg,
                        offset: MemoryOffset(0),
                    };

                    let source_memory_location = MemoryLocation {
                        base_ptr_reg: optional_tagged_union_reg,
                        offset: tagged_union.payload_offset,
                        ty: target_memory_location.ty.clone(),
                    };

                    self.builder.add_block_copy_with_offset(
                        &target_memory_location,
                        &source_memory_location,
                        binding.expr.node(),
                        "load payload into binding variable",
                    );
                } else {
                    let source_memory_location = MemoryLocation {
                        base_ptr_reg: optional_tagged_union_reg,
                        offset: tagged_union.payload_offset,
                        ty: target_binding_variable_reg.ty.clone(),
                    };

                    self.emit_load_value_from_memory_source(
                        &target_binding_variable_reg,
                        &source_memory_location,
                        binding.expr.node(),
                        "load payload into binding variable",
                    );
                }
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

    /*
    fn get_complex_pointer_into(&mut self, target_reg: TypedRegister, rvalue_or_lvalue: RValueOrLValue, node: &Node, ) {
        match rvalue_or_lvalue {
            RValueOrLValue::Scalar(reg) => reg.clone(),
            RValueOrLValue::Memory(destination) => {
                self.emit_absolute_pointer_if_needed(&destination, node, "complex absolute pointer")
            }
        }
    }

     */
}
