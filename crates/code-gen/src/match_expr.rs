/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Match, NormalPattern, Pattern, PatternElement};
use swamp_vm_isa::MemoryOffset;
use swamp_vm_types::types::{b8_type, u8_type, BasicTypeKind, Destination, VmType};
use swamp_vm_types::MemoryLocation;

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_match_enum(
        &mut self,
        output_destination: &Destination,
        match_expr: &Match,
        ctx: &Context,
    ) {
        let enum_ptr_reg = self.emit_scalar_rvalue(&match_expr.expression, ctx);

        let mut jump_to_exit_placeholders = Vec::new();

        let arm_len_to_consider = if match_expr.contains_wildcard() {
            match_expr.arms.len()
        } else {
            match_expr.arms.len()
        };

        let enum_tag_temp_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u8_type()),
            "temp reg for enum tag",
        ); // TODO: support different tag sizes

        let condition_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(b8_type()),
            "comparison reg",
        );

        self.builder.add_ld8_from_pointer_with_offset(
            enum_tag_temp_reg.register(),
            &enum_ptr_reg,
            MemoryOffset(0), // TODO: take offset from tag union info
            &match_expr.expression.node,
            "read enum tag",
        );

        let underlying_enum = enum_ptr_reg.ty.basic_type.clone();
        let BasicTypeKind::TaggedUnion(enum_type) = &underlying_enum.kind else {
            //panic!("internal error enum {underlying_enum}");
            return;
        };

        for (index, arm) in match_expr.arms.iter().enumerate() {
            let is_last = index == arm_len_to_consider - 1;

            //  Each arm must set the CPU zero flag
            let maybe_guard = match &arm.pattern {
                Pattern::Normal(normal_pattern, maybe_guard) => match normal_pattern {
                    NormalPattern::PatternList(_) => None,
                    NormalPattern::EnumPattern(enum_variant, maybe_patterns) => {
                        self.builder.add_eq_u8_immediate(
                            condition_reg.register(),
                            enum_tag_temp_reg.register(),
                            enum_variant.common().container_index,
                            &arm.expression.node,
                            "check for enum variant",
                        );
                        maybe_guard.as_ref()
                    }
                    NormalPattern::Literal(_) => {
                        todo!()
                    }
                },
                Pattern::Wildcard(_) => {
                    // Wildcard is always true, so no comparison code is needed here at all
                    None
                }
            };

            let did_add_comparison = !matches!(arm.pattern, Pattern::Wildcard(_));

            let maybe_skip_added = if did_add_comparison {
                Some(self.builder.add_jmp_if_not_equal_placeholder(
                    condition_reg.register(),
                    &arm.expression.node,
                    "placeholder for enum match",
                ))
            } else {
                None
            };

            // insert code here to emit patterns to variables
            match &arm.pattern {
                Pattern::Normal(normal_pattern, _maybe_guard) => match normal_pattern {
                    NormalPattern::PatternList(_) => todo!(),
                    NormalPattern::EnumPattern(enum_variant, maybe_patterns) => {
                        if let Some(patterns) = maybe_patterns {
                            for pattern in patterns {
                                match pattern {
                                    PatternElement::Variable(var) => {
                                        // For Variable patterns, we set the pointer to the payload to the variable
                                        let var_reg = self
                                            .variable_registers
                                            .get(&var.unique_id_within_function)
                                            .unwrap()
                                            .clone();

                                        let source_enum_payload_location = MemoryLocation {
                                            base_ptr_reg: enum_ptr_reg.clone(),
                                            offset: enum_type.payload_offset,
                                            ty: var_reg.ty.clone(),
                                        };

                                        self.emit_load_value_from_memory_source(
                                            &var_reg,
                                            &source_enum_payload_location,
                                            &var.name,
                                            "load value from variant payload into variable",
                                        );
                                    }
                                    PatternElement::VariableWithFieldIndex(
                                        variable,
                                        field_index_within_variant_payload_value,
                                    ) => {
                                        let enum_variant_common = enum_type
                                            .get_variant_as_offset_item(
                                                enum_variant.common().index() as usize,
                                            );
                                        let field_offset_item_inside_payload = enum_variant_common
                                            .ty
                                            .get_field_offset(
                                                *field_index_within_variant_payload_value,
                                            )
                                            .unwrap();

                                        let total_offset = enum_type.payload_offset
                                            + field_offset_item_inside_payload.offset;
                                        let var_reg = self
                                            .variable_registers
                                            .get(&variable.unique_id_within_function)
                                            .unwrap()
                                            .clone();

                                        let source_enum_memory_location = MemoryLocation {
                                            base_ptr_reg: enum_ptr_reg.clone(),
                                            offset: total_offset,
                                            ty: var_reg.ty.clone(),
                                        };

                                        self.emit_load_or_calculate_address_from_memory(
                                            &var_reg,
                                            &source_enum_memory_location,
                                            &variable.name,
                                            "load variant from field index",
                                        );
                                    }
                                    PatternElement::Wildcard(_) => {
                                        // Intentionally do nothing, the variable should not be handled
                                    }
                                }
                            }
                        }
                    }
                    NormalPattern::Literal(_) => {
                        todo!()
                    }
                },
                Pattern::Wildcard(_) => {
                    // Intentionally do nothing, the expression will be used below
                }
            }

            // evaluate the guard condition after pattern variables are loaded
            let maybe_guard_skip = maybe_guard.map(|guard| self.emit_condition_context(guard, ctx));

            self.emit_expression(output_destination, &arm.expression, ctx);

            if !is_last {
                let jump_to_exit_placeholder = self.builder.add_jump_placeholder(
                    &arm.expression.debug_last_expression().node,
                    "jump to exit",
                );
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
            }

            if let Some(skip) = maybe_skip_added {
                self.builder.patch_jump_here(skip);
            }
            if let Some(guard_skip) = maybe_guard_skip {
                self.builder.patch_jump_here(guard_skip);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.builder.patch_jump_here(placeholder);
        }
    }

    pub(crate) fn emit_match_literal(
        &mut self,
        output_destination: &Destination,
        match_expr: &Match,
        ctx: &Context,
    ) {
        let mut jump_to_exit_placeholders = Vec::new();

        let arm_len_to_consider = if match_expr.contains_wildcard() {
            match_expr.arms.len()
        } else {
            match_expr.arms.len()
        };

        let arm_bool_condition_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(b8_type()),
            "comparison bool reg",
        );

        let scrutinee_reg = self.emit_scalar_rvalue(&match_expr.expression, ctx);

        for (index, arm) in match_expr.arms.iter().enumerate() {
            let is_last = index == arm_len_to_consider - 1;

            let maybe_guard = match &arm.pattern {
                Pattern::Normal(normal_pattern, maybe_guard) => match normal_pattern {
                    NormalPattern::Literal(literal_expression) => {
                        let literal_expr_reg = self.emit_scalar_rvalue(literal_expression, ctx);
                        let node = literal_expression.node.clone();
                        self.emit_equality_to_bool_target(
                            arm_bool_condition_reg.register(),
                            &scrutinee_reg,
                            true,
                            &literal_expr_reg,
                            &node,
                            ctx,
                        );

                        maybe_guard.as_ref()
                    }
                    NormalPattern::PatternList(_) => panic!("not handled here"),
                    NormalPattern::EnumPattern(a, b) => {
                        panic!("got enum pattern in literals! {a:?} {b:?}")
                    }
                },
                Pattern::Wildcard(_) => {
                    // Wildcard is always true, so no comparison code is needed here at all
                    None
                }
            };

            let did_add_comparison = !matches!(arm.pattern, Pattern::Wildcard(_));

            let maybe_skip_added = if did_add_comparison {
                Some(self.builder.add_jmp_if_not_equal_placeholder(
                    &arm_bool_condition_reg.register,
                    &arm.expression.node,
                    "placeholder for enum match",
                ))
            } else {
                None
            };

            // insert code here to emit patterns to variables
            match &arm.pattern {
                Pattern::Normal(normal_pattern, maybe_guard) => match normal_pattern {
                    NormalPattern::PatternList(_) => panic!("should not be handled here"),

                    NormalPattern::Literal(literal_expression) => {
                        // Intentionally do nothing, the expression will be used below
                    }
                    NormalPattern::EnumPattern(_, _) => panic!("should not be handled here"),
                },
                Pattern::Wildcard(_) => {
                    // Intentionally do nothing, the expression will be used below
                }
            }

            // evaluate the guard condition after pattern variables are loaded
            let maybe_guard_skip =
                maybe_guard.map(|found_guard| self.emit_condition_context(found_guard, ctx));

            self.emit_expression(output_destination, &arm.expression, ctx);

            if !is_last {
                let jump_to_exit_placeholder = self.builder.add_jump_placeholder(
                    &arm.expression.debug_last_expression().node,
                    "jump to exit",
                );
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
            }

            if let Some(skip) = maybe_skip_added {
                self.builder.patch_jump_here(skip);
            }
            if let Some(guard_skip) = maybe_guard_skip {
                self.builder.patch_jump_here(guard_skip);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.builder.patch_jump_here(placeholder);
        }
    }
}
