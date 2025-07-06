/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::FlagStateKind;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{BinaryOperator, BinaryOperatorKind};
use swamp_types::TypeKind;
use swamp_vm_types::types::TypedRegister;

impl CodeBuilder<'_> {
    pub(crate) fn emit_binary_operator(
        &mut self,
        target_reg: &TypedRegister,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) {
        match &binary_operator.kind {
            BinaryOperatorKind::LogicalOr | BinaryOperatorKind::LogicalAnd => {
                let t_flag_result =
                    self.emit_binary_operator_logical_to_boolean(target_reg, binary_operator, ctx);
                self.force_normalized_bool_reg_if_needed(
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
        dest_bool_reg: &TypedRegister,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) {
        let hwm = self.temp_registers.save_mark();

        let left_source = self.emit_scalar_rvalue(&binary_operator.left, ctx);
        let right_source = self.emit_scalar_rvalue(&binary_operator.right, ctx);

        match &binary_operator.kind {
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                let is_equal_polarity = matches!(binary_operator.kind, BinaryOperatorKind::Equal);
                let t_flag = self.emit_binary_operator_equality_to_bool(
                    dest_bool_reg,
                    &left_source,
                    is_equal_polarity,
                    &right_source,
                    &binary_operator.node,
                    ctx,
                );
                self.force_normalized_bool_reg_if_needed(
                    dest_bool_reg,
                    t_flag,
                    &binary_operator.node,
                );
            }
            BinaryOperatorKind::GreaterEqual
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessEqual => {
                let t_flag = self.emit_binary_operator_relational(
                    dest_bool_reg,
                    &left_source,
                    binary_operator,
                    &right_source,
                );
                self.force_normalized_bool_reg_if_needed(
                    dest_bool_reg,
                    t_flag,
                    &binary_operator.node,
                );
            }
            _ => match (
                &*binary_operator.left.ty.kind,
                &*binary_operator.right.ty.kind,
            ) {
                (TypeKind::Int, TypeKind::Int) => self.emit_binary_operator_i32(
                    dest_bool_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (TypeKind::Float, TypeKind::Float) => self.emit_binary_operator_f32(
                    dest_bool_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (
                    TypeKind::String(..) | TypeKind::StringStorage(..),
                    TypeKind::String(..) | TypeKind::StringStorage(..),
                ) => self.emit_binary_operator_string(
                    dest_bool_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                _ => todo!(
                    "binary operator {} <-> {}",
                    binary_operator.left.ty,
                    binary_operator.right.ty
                ),
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
            _ => todo!(),
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
            _ => panic!("unknown"),
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
