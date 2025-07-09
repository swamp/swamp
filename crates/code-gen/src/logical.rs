/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
//! Logical helper functions for the code gen emitter
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::{FlagState, FlagStateKind};
use source_map_node::Node;
use swamp_semantic::{
    BinaryOperator, BinaryOperatorKind, BooleanExpression, Expression, ExpressionKind,
    UnaryOperator, UnaryOperatorKind,
};
use swamp_types::TypeKind;
use swamp_vm_types::types::{b8_type, Destination, TypedRegister, VmType};
use swamp_vm_types::PatchPosition;

impl CodeBuilder<'_> {
    pub(crate) fn force_normalized_bool_reg_if_needed(
        &mut self,
        target: &TypedRegister,
        t_flag_state: FlagState,
        node: &Node,
    ) {
        match t_flag_state.kind {
            FlagStateKind::TFlagIsIndeterminate => {
                // intentionally do nothing
            }
            FlagStateKind::TFlagIsTrueWhenSet => {
                // self.builder
                //   .add_stz(target, node, "materialize positive P flag");
            }
            FlagStateKind::TFlagIsTrueWhenClear => {
                self.builder
                    .add_meqz(target, target, node, "materialize inverse boolean");
            }
        }
    }
    pub fn emit_unary_operator_logical(
        &mut self,
        dest_bool_reg: &TypedRegister,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) -> FlagState {
        match &unary_operator.kind {
            UnaryOperatorKind::Not => match &*unary_operator.left.ty.kind {
                TypeKind::Bool => {
                    let bool_result =
                        self.emit_expression_to_boolean(dest_bool_reg, &unary_operator.left, ctx);
                    bool_result.invert_polarity()
                }
                _ => panic!("unknown not"),
            },
            _ => panic!("unary operator do not provide P flag"),
        }
    }

    pub(crate) fn emit_condition_context(
        &mut self,
        condition: &BooleanExpression,
        ctx: &Context,
    ) -> PatchPosition {
        let hwm = self.temp_registers.save_mark();
        let temp_truth_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(b8_type()),
            "true-register for condition",
        );
        let result =
            self.emit_expression_to_boolean(temp_truth_reg.register(), &condition.expression, ctx);

        let patch = self.builder.add_jmp_if_not_equal_polarity_placeholder(
            temp_truth_reg.register(),
            &result.polarity(),
            &condition.expression.node,
            "jump boolean condition false",
        );

        self.temp_registers.restore_to_mark(hwm);

        patch
    }

    pub(crate) fn emit_expression_to_boolean(
        &mut self,
        dest_bool_reg: &TypedRegister,
        condition: &Expression,
        ctx: &Context,
    ) -> FlagState {
        match &condition.kind {
            ExpressionKind::CoerceOptionToBool(option_union_expr) => {
                let region = self.emit_scalar_rvalue(option_union_expr, ctx);

                let tag_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(swamp_vm_types::types::b8_type()),
                    "temp for option tag",
                );

                let (tag_offset, ..) = region.ty.basic_type.unwrap_info().unwrap();
                self.builder.add_ld8_from_pointer_with_offset_u16(
                    tag_reg.register(),
                    &region,
                    tag_offset,
                    &option_union_expr.node,
                    "load option tag",
                );

                self.builder.add_mov_reg(
                    dest_bool_reg,
                    tag_reg.register(),
                    &option_union_expr.node,
                    "test option tag",
                );

                return FlagState {
                    kind: FlagStateKind::TFlagIsTrueWhenSet,
                };
            }
            ExpressionKind::BinaryOp(operator) => match &operator.kind {
                BinaryOperatorKind::LogicalAnd | BinaryOperatorKind::LogicalOr => {
                    return self.emit_binary_operator_logical_to_boolean(
                        dest_bool_reg,
                        operator,
                        ctx,
                    );
                }
                BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                    let left = self.emit_scalar_rvalue(&operator.left, ctx);
                    let right = self.emit_scalar_rvalue(&operator.right, ctx);
                    let is_equal_polarity = matches!(operator.kind, BinaryOperatorKind::Equal);
                    return self.emit_equality_to_bool_target(
                        dest_bool_reg,
                        &left,
                        is_equal_polarity,
                        &right,
                        &operator.node,
                        ctx,
                    );
                }
                BinaryOperatorKind::GreaterEqual
                | BinaryOperatorKind::GreaterThan
                | BinaryOperatorKind::LessThan
                | BinaryOperatorKind::LessEqual => {
                    let left_source = self.emit_scalar_rvalue(&operator.left, ctx);
                    let right_source = self.emit_scalar_rvalue(&operator.right, ctx);

                    return self.emit_binary_operator_relational(
                        dest_bool_reg,
                        &left_source,
                        operator,
                        &right_source,
                    );
                }
                _ => panic!(
                    "binary operator does not provide us with P flag {:?}",
                    condition.kind
                ),
            },
            ExpressionKind::UnaryOp(operator) => match &operator.kind {
                UnaryOperatorKind::Not => {
                    return self.emit_unary_operator_logical(dest_bool_reg, operator, ctx);
                }
                _ => panic!("unary operator does not provide us with P flag"),
            },
            _ => {
                let destination = Destination::Register(dest_bool_reg.clone());
                self.emit_expression(&destination, condition, ctx);
            }
        }

        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_expression_to_normalized_t_flag(
        &mut self,
        dest_bool_reg: &TypedRegister,
        condition: &Expression,
        ctx: &Context,
    ) {
        let result = self.emit_expression_to_boolean(dest_bool_reg, condition, ctx);
        assert_ne!(result.kind, FlagStateKind::TFlagIsIndeterminate);

        if result.kind == FlagStateKind::TFlagIsTrueWhenClear {
            self.builder.add_meqz(
                dest_bool_reg,
                dest_bool_reg,
                &condition.node,
                "normalized z is required",
            );
        }
    }
    pub(crate) fn emit_binary_operator_logical_to_boolean(
        &mut self,
        dest_bool_reg: &TypedRegister,
        binary_operator: &BinaryOperator,
        context: &Context,
    ) -> FlagState {
        let node = &binary_operator.node;

        // the logical is always normalized
        let kind = FlagStateKind::TFlagIsTrueWhenSet;

        match binary_operator.kind {
            BinaryOperatorKind::LogicalOr => {
                self.emit_expression_to_normalized_t_flag(
                    dest_bool_reg,
                    &binary_operator.left,
                    context,
                );

                let jump_after_patch = self.builder.add_jmp_if_true_placeholder(
                    dest_bool_reg,
                    node,
                    "OR: skip rhs because lhs is true",
                );

                self.emit_expression_to_normalized_t_flag(
                    dest_bool_reg,
                    &binary_operator.right,
                    context,
                );

                self.builder.patch_jump_here(jump_after_patch);
            }
            BinaryOperatorKind::LogicalAnd => {
                self.emit_expression_to_normalized_t_flag(
                    dest_bool_reg,
                    &binary_operator.left,
                    context,
                );

                let jump_after_patch = self.builder.add_jmp_if_not_true_placeholder(
                    dest_bool_reg,
                    node,
                    "AND: skip rhs because lhs is false",
                );

                self.emit_expression_to_normalized_t_flag(
                    dest_bool_reg,
                    &binary_operator.right,
                    context,
                );

                self.builder.patch_jump_here(jump_after_patch);
            }

            _ => {
                panic!("unknown operator {binary_operator:?}");
            }
        }

        FlagState { kind }
    }
}
