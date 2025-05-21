//! Logical helper functions for the code gen emitter
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::{GeneratedExpressionResult, GeneratedExpressionResultKind};
use source_map_node::Node;
use swamp_semantic::{
    BinaryOperator, BinaryOperatorKind, BooleanExpression, Expression, ExpressionKind,
    UnaryOperator, UnaryOperatorKind,
};
use swamp_types::Type;
use swamp_vm_types::PatchPosition;
use swamp_vm_types::types::{OutputDestination, TypedRegister, VmType, u32_type};

impl CodeBuilder<'_> {
    pub(crate) fn materialize_t_flag_to_bool_if_needed(
        &mut self,
        target: &TypedRegister,
        t_flag_state: GeneratedExpressionResult,
        node: &Node,
    ) {
        match t_flag_state.kind {
            GeneratedExpressionResultKind::TFlagIsIndeterminate => {
                // intentionally do nothing
            }
            GeneratedExpressionResultKind::TFlagIsTrueWhenSet => {
                self.builder
                    .add_stz(target, node, "materialize positive P flag");
            }
            GeneratedExpressionResultKind::TFlagIsTrueWhenClear => {
                self.builder
                    .add_stnz(target, node, "materialize inverse P flag");
            }
        }
    }
    pub fn emit_unary_operator_logical_to_t_flag(
        &mut self,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match &unary_operator.kind {
            UnaryOperatorKind::Not => match &unary_operator.left.ty {
                Type::Bool => {
                    let bool_result = self.emit_expression_to_t_flag(&unary_operator.left, ctx);
                    bool_result.invert_polarity()
                }
                _ => panic!("unknown not"),
            },
            _ => panic!("unary operator do not provide t flag"),
        }
    }

    pub(crate) fn emit_condition_context(
        &mut self,
        condition: &BooleanExpression,
        ctx: &Context,
    ) -> PatchPosition {
        let result = self.emit_expression_to_t_flag(&condition.expression, ctx);

        

        self.builder.add_jmp_if_not_equal_polarity_placeholder(
            &result.polarity(),
            &condition.expression.node,
            "jump boolean condition false",
        )
    }

    pub(crate) fn emit_expression_to_t_flag(
        &mut self,
        condition: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match &condition.kind {
            ExpressionKind::CoerceOptionToBool(option_union_expr) => {
                let region = self.emit_scalar_rvalue(option_union_expr, ctx);
                // We can shortcut this, since we know that the tag location is basically a bool value
                self.builder.add_tst_u8(
                    &region,
                    &option_union_expr.node,
                    "shortcut directly to z-flag",
                );
                return GeneratedExpressionResult {
                    kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
                };
            }
            ExpressionKind::BinaryOp(operator) => match &operator.kind {
                BinaryOperatorKind::LogicalOr => {
                    return self.emit_binary_operator_logical_to_t_flag(operator, ctx);
                }
                BinaryOperatorKind::LogicalAnd => {
                    return self.emit_binary_operator_logical_to_t_flag(operator, ctx);
                }
                BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                    let left = self.emit_scalar_rvalue(&operator.left, ctx);
                    let right = self.emit_scalar_rvalue(&operator.left, ctx);
                    let is_equal_polarity = matches!(operator.kind, BinaryOperatorKind::Equal);
                    return self.emit_binary_operator_equal_to_t_flag_only(
                        &left,
                        is_equal_polarity,
                        &right,
                        &operator.node,
                        ctx,
                    );
                }
                _ => panic!(
                    "binary operator does not provide us with t flag {:?}",
                    condition.kind
                ),
            },
            ExpressionKind::UnaryOp(operator) => match &operator.kind {
                UnaryOperatorKind::Not => {
                    return self.emit_unary_operator_logical_to_t_flag(operator, ctx);
                }
                _ => panic!("unary operator does not provide us with t flag"),
            },
            _ => {}
        }

        let temp_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(u32_type()), "temp");

        let output_destination = OutputDestination::new_reg(temp_reg.register.clone());
        self.emit_bool_expression(&output_destination, condition, ctx);

        self.builder.add_tst_u8(
            temp_reg.register(),
            &condition.node,
            "set P flag from register",
        );
        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_expression_to_normalized_t_flag(&mut self, condition: &Expression, ctx: &Context) {
        let result = self.emit_expression_to_t_flag(condition, ctx);
        assert_ne!(
            result.kind,
            GeneratedExpressionResultKind::TFlagIsIndeterminate
        );

        if result.kind == GeneratedExpressionResultKind::TFlagIsTrueWhenClear {
            self.builder
                .add_not_t(&condition.node, "normalized z is required");
        }
    }
    pub(crate) fn emit_binary_operator_logical_to_t_flag(
        &mut self,
        binary_operator: &BinaryOperator,
        context: &Context,
    ) -> GeneratedExpressionResult {
        let node = &binary_operator.node;

        // the logical is always normalized
        let kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;

        match binary_operator.kind {
            BinaryOperatorKind::LogicalOr => {
                self.emit_expression_to_normalized_t_flag(&binary_operator.left, context);

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_true_placeholder(node, "OR: skip rhs because lhs is true");

                self.emit_expression_to_normalized_t_flag(&binary_operator.right, context);

                self.builder.patch_jump_here(jump_after_patch);
            }
            BinaryOperatorKind::LogicalAnd => {
                self.emit_expression_to_normalized_t_flag(&binary_operator.left, context);

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_not_true_placeholder(node, "AND: skip rhs because lhs is false");

                self.emit_expression_to_normalized_t_flag(&binary_operator.right, context);

                self.builder.patch_jump_here(jump_after_patch);
            }

            _ => {
                panic!("unknown operator {binary_operator:?}");
            }
        }

        GeneratedExpressionResult { kind }
    }
}
