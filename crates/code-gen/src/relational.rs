use crate::code_bld::CodeBuilder;
use crate::{GeneratedExpressionResult, GeneratedExpressionResultKind};
use swamp_semantic::{BinaryOperator, BinaryOperatorKind};
use swamp_vm_types::types::{BasicTypeKind, TypedRegister};

impl CodeBuilder<'_> {
    pub fn emit_binary_operator_relational_i32_to_t_flag_only(
        &mut self,
        left_source: &TypedRegister,
        binary_operator: &BinaryOperator,
        right_source: &TypedRegister,
    ) -> GeneratedExpressionResult {
        let node = &binary_operator.node;
        match &binary_operator.kind {
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_i32(left_source, right_source, node, "i32 lt");
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(left_source, right_source, node, "i32 le");
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(left_source, right_source, node, "i32 gt");
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(left_source, right_source, node, "i32 ge");
            }
            _ => {
                panic!("was not a condition")
            }
        }
        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
        }
    }

    pub fn emit_binary_operator_relational_f32_to_t_flag_only(
        &mut self,
        left_source: &TypedRegister,
        binary_operator: &BinaryOperator,
        right_source: &TypedRegister,
    ) -> GeneratedExpressionResult {
        let node = &binary_operator.node;
        match &binary_operator.kind {
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_i32(left_source, right_source, node, "f32 lt");
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(left_source, right_source, node, "f32 le");
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(left_source, right_source, node, "f32 gt");
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(left_source, right_source, node, "f32 ge");
            }
            _ => panic!("not a relational operator"),
        }
        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
        }
    }

    pub fn emit_binary_operator_relational_to_t_flag_only(
        &mut self,
        left_source: &TypedRegister,
        binary_operator: &BinaryOperator,
        right_source: &TypedRegister,
    ) -> GeneratedExpressionResult {
        match &left_source.ty.basic_type.kind {
            BasicTypeKind::S32 => self.emit_binary_operator_relational_i32_to_t_flag_only(
                left_source,
                binary_operator,
                right_source,
            ),
            BasicTypeKind::Fixed32 => self.emit_binary_operator_relational_f32_to_t_flag_only(
                left_source,
                binary_operator,
                right_source,
            ),
            _ => panic!("this is not a condition"),
        }
    }
}
