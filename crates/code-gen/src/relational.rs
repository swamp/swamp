/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::{FlagState, FlagStateKind};
use swamp_semantic::{BinaryOperator, BinaryOperatorKind};
use swamp_vm_types::types::{BasicTypeKind, TypedRegister};

impl CodeBuilder<'_> {
    pub fn emit_binary_operator_relational_i32_to_t_flag_only(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &TypedRegister,
        binary_operator: &BinaryOperator,
        right_source: &TypedRegister,
    ) -> FlagState {
        let node = &binary_operator.node;
        match &binary_operator.kind {
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_i32(dest_bool_reg, left_source, right_source, node, "i32 lt");
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(dest_bool_reg, left_source, right_source, node, "i32 le");
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(dest_bool_reg, left_source, right_source, node, "i32 gt");
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(dest_bool_reg, left_source, right_source, node, "i32 ge");
            }
            _ => {
                panic!("was not a condition")
            }
        }
        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    pub fn emit_binary_operator_relational_u8(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &TypedRegister,
        binary_operator: &BinaryOperator,
        right_source: &TypedRegister,
    ) -> FlagState {
        let node = &binary_operator.node;
        match &binary_operator.kind {
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_u32(dest_bool_reg, left_source, right_source, node, "i32 lt");
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_u32(dest_bool_reg, left_source, right_source, node, "i32 le");
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_u32(dest_bool_reg, left_source, right_source, node, "i32 gt");
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_u32(dest_bool_reg, left_source, right_source, node, "i32 ge");
            }
            _ => {
                panic!("was not a condition")
            }
        }
        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    pub fn emit_binary_operator_relational_f32_to_t_flag_only(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &TypedRegister,
        binary_operator: &BinaryOperator,
        right_source: &TypedRegister,
    ) -> FlagState {
        let node = &binary_operator.node;
        match &binary_operator.kind {
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_i32(dest_bool_reg, left_source, right_source, node, "f32 lt");
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(dest_bool_reg, left_source, right_source, node, "f32 le");
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(dest_bool_reg, left_source, right_source, node, "f32 gt");
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(dest_bool_reg, left_source, right_source, node, "f32 ge");
            }
            _ => panic!("not a relational operator"),
        }
        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    pub fn emit_binary_operator_relational(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &TypedRegister,
        binary_operator: &BinaryOperator,
        right_source: &TypedRegister,
    ) -> FlagState {
        match &left_source.ty.basic_type.kind {
            BasicTypeKind::S32 => self.emit_binary_operator_relational_i32_to_t_flag_only(
                dest_bool_reg,
                left_source,
                binary_operator,
                right_source,
            ),
            BasicTypeKind::Fixed32 => self.emit_binary_operator_relational_f32_to_t_flag_only(
                dest_bool_reg,
                left_source,
                binary_operator,
                right_source,
            ),
            BasicTypeKind::U8 => self.emit_binary_operator_relational_u8(
                dest_bool_reg,
                left_source,
                binary_operator,
                right_source,
            ),
            _ => panic!("this is not a relational operator type"),
        }
    }
}
