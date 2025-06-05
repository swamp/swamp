/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::FlagStateKind;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{BinaryOperator, BinaryOperatorKind};
use swamp_types::Type;
use swamp_vm_types::types::TypedRegister;

impl CodeBuilder<'_> {
    pub(crate) fn emit_binary_operator(
        &mut self,
        target_reg: &TypedRegister,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) {
        //info!(left=?binary_operator.left.ty, right=?binary_operator.right.ty, "binary_op");

        match &binary_operator.kind {
            BinaryOperatorKind::LogicalOr | BinaryOperatorKind::LogicalAnd => {
                let t_flag_result =
                    self.emit_binary_operator_logical_to_t_flag(binary_operator, ctx);
                self.materialize_t_flag_to_bool_if_needed(
                    target_reg,
                    t_flag_result,
                    &binary_operator.node,
                );
            }
            _ => self.emit_binary_operator_normal(target_reg, binary_operator, ctx),
        }
    }

    fn emit_binary_operator_normal(
        &mut self,
        target_reg: &TypedRegister,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) {
        let hwm = self.temp_registers.save_mark();

        let left_source = self.emit_scalar_rvalue(&binary_operator.left, ctx);
        let right_source = self.emit_scalar_rvalue(&binary_operator.right, ctx);

        match &binary_operator.kind {
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                let is_equal_polarity = matches!(binary_operator.kind, BinaryOperatorKind::Equal);
                let t_flag = self.emit_binary_operator_equal_to_t_flag_only(
                    &left_source,
                    is_equal_polarity,
                    &right_source,
                    &binary_operator.node,
                    ctx,
                );
                self.materialize_t_flag_to_bool_if_needed(
                    target_reg,
                    t_flag,
                    &binary_operator.node,
                );
            }
            BinaryOperatorKind::GreaterEqual
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessEqual => {
                let t_flag = self.emit_binary_operator_relational_to_t_flag_only(
                    &left_source,
                    binary_operator,
                    &right_source,
                );
                self.materialize_t_flag_to_bool_if_needed(
                    target_reg,
                    t_flag,
                    &binary_operator.node,
                );
            }
            _ => match (
                &binary_operator.left.ty.underlying(),
                &binary_operator.right.ty.underlying(),
            ) {
                //(Type::Bool, Type::Bool) => self.emit_binary_operator_logical(binary_operator),
                (Type::Int, Type::Int) => self.emit_binary_operator_i32(
                    target_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (Type::Float, Type::Float) => self.emit_binary_operator_f32(
                    target_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (Type::String, Type::String) => self.emit_binary_operator_string(
                    target_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                _ => todo!(),
            },
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    fn emit_binary_operator_i32(
        &mut self,
        target_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &TypedRegister,
        ctx: &Context,
    ) {
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_add_u32(
                    // u32 is the same as i32 when it comes to wrapping_add
                    target_reg,
                    left_source,
                    right_source,
                    node,
                    "i32 add",
                );
            }
            BinaryOperatorKind::Subtract => {
                self.builder.add_sub_u32(
                    target_reg,
                    left_source,
                    right_source,
                    node,
                    &format!("i32 sub {target_reg:?} = {left_source:?} - {right_source:?}"),
                );
            }
            BinaryOperatorKind::Multiply => {
                self.builder
                    .add_mul_i32(target_reg, left_source, right_source, node, "i32 mul");
            }
            BinaryOperatorKind::Divide => {
                self.builder
                    .add_div_i32(target_reg, left_source, right_source, node, "i32 div");
            }
            BinaryOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(target_reg, left_source, right_source, node, "i32 mod");
            }
            BinaryOperatorKind::LogicalOr => todo!(),
            BinaryOperatorKind::LogicalAnd => todo!(),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => todo!(),
            _ => todo!(), /*
                          {
                              self.builder
                                  .add_cmp32(left_source, right_source, node, "i32 cmp");
                              if let BinaryOperatorKind::Equal = binary_operator_kind {
                                  kind = GeneratedExpressionResultKind::ZFlagIsTrue;
                              } else {
                                  kind = GeneratedExpressionResultKind::ZFlagIsInversion;
                              }
                          }
                           */
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn emit_binary_operator_f32(
        &mut self,
        target_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &TypedRegister,
        ctx: &Context,
    ) {
        let kind = FlagStateKind::TFlagIsIndeterminate;
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder
                    .add_add_f32(target_reg, left_source, right_source, node, "f32 add");
            }
            BinaryOperatorKind::Subtract => {
                self.builder
                    .add_sub_f32(target_reg, left_source, right_source, node, "f32 sub");
            }
            BinaryOperatorKind::Multiply => {
                self.builder
                    .add_mul_f32(target_reg, left_source, right_source, node, "f32 mul");
            }
            BinaryOperatorKind::Divide => {
                self.builder
                    .add_div_f32(target_reg, left_source, right_source, node, "f32 div");
            }
            BinaryOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(target_reg, left_source, right_source, node, "f32 mod");
            }
            BinaryOperatorKind::LogicalOr => panic!("not supported"),
            BinaryOperatorKind::LogicalAnd => panic!("not supported"),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => panic!("handled elsewhere"),
            _ => panic!("unknown"), /*{
                                        self.builder
                                            .add_cmp32(left_source, right_source, node, "f32 eq");
                                        if let BinaryOperatorKind::Equal = binary_operator_kind {
                                            kind = GeneratedExpressionResultKind::ZFlagIsTrue;
                                        } else {
                                            kind = GeneratedExpressionResultKind::ZFlagIsInversion;
                                        }
                                    }*/
        }
    }

    fn emit_binary_operator_string(
        &mut self,
        target_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &TypedRegister,
        ctx: &Context,
    ) {
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_string_append(
                    target_reg,
                    left_source,
                    right_source,
                    node,
                    "string append",
                );
            }

            BinaryOperatorKind::Equal => todo!(),
            BinaryOperatorKind::NotEqual => todo!(),
            _ => panic!("illegal string operator"),
        }
    }
}
