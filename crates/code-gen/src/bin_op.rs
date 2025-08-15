/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::FlagStateKind;
use source_map_node::Node;
use swamp_semantic::{BinaryOperator, BinaryOperatorKind, Expression};
use swamp_types::TypeKind;
use swamp_vm_types::types::{u8_type, Place, TypedRegister, VmType};

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
            BinaryOperatorKind::NoneCoalesce => {
                panic!("handled elsewhere")
            }
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                let is_equal_polarity = matches!(binary_operator.kind, BinaryOperatorKind::Equal);
                let t_flag = self.emit_equality_to_bool_target(
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
                    TypeKind::StringView(..) | TypeKind::StringStorage(..),
                    TypeKind::StringView(..) | TypeKind::StringStorage(..),
                ) => self.emit_binary_operator_string(
                    dest_bool_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),

                (TypeKind::StringView(..) | TypeKind::StringStorage(..), TypeKind::Int) => self
                    .emit_binary_operator_string(
                        dest_bool_reg,
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
            BinaryOperatorKind::Multiply => {
                self.builder.add_string_multiply(
                    target_reg,
                    left_source,
                    right_source,
                    node,
                    "string multiply",
                );
            }
            BinaryOperatorKind::Equal => todo!(),
            BinaryOperatorKind::NotEqual => todo!(),
            _ => panic!("illegal string operator"),
        }
    }

    pub(crate) fn emit_none_coalesce_operator(
        &mut self,
        dest: &Place,
        left: &Expression,
        right: &Expression,
        node: &Node,
        ctx: &Context,
    ) {
        let both_are_optionals = matches!(&*left.ty.kind, TypeKind::Optional(_))
            && matches!(&*right.ty.kind, TypeKind::Optional(_));

        let left_optional_basic_type = self.state.layout_cache.layout(&left.ty);

        let destination_storage: Place; // so it lives long enough
        let destination_to_use = if both_are_optionals {
            dest
        } else {
            destination_storage = self.allocate_frame_space_and_return_destination_to_it(
                &left_optional_basic_type,
                node,
                "?? left temp storage for tag + payload",
            );
            &destination_storage
        };

        self.emit_expression(destination_to_use, left, ctx);

        let (tag_offset, _, payload_offset, _) = left_optional_basic_type.unwrap_info().unwrap();

        let tag_location =
            destination_to_use.add_offset(tag_offset, VmType::new_contained_in_register(u8_type()));
        let tag_memory_location = tag_location.memory_location().unwrap();

        let temp_tag_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u8_type()),
            "?? temporary tag register",
        );

        self.builder.add_ld8_from_pointer_with_offset(
            temp_tag_reg.register(),
            tag_memory_location.reg(),
            tag_memory_location.offset,
            node,
            "?? load temporary register from tag_memory",
        );

        let jump_if_none = self.builder.add_jmp_if_not_true_placeholder(
            &temp_tag_reg.register,
            node,
            "?? jump if None ",
        );

        // SOME case ---------------------------------------------------------------
        // Either just use it as is, or unwrap payload
        if both_are_optionals {
            // Case B
            // We have already materialized the expression above, so we are done
        } else {
            // Case A
            let some_payload_basic_type = left_optional_basic_type.get_variant(1);
            let payload_source_location = destination_to_use.add_offset(
                payload_offset,
                VmType::new_unknown_placement(some_payload_basic_type.ty.clone()),
            );
            self.emit_copy_value_between_destinations(dest, &payload_source_location, node, "?? right hand side is NOT optional. must copy from payload area to output destination. unwrap was needed because of different types");
        }

        let jump_to_after_whole_thing = self
            .builder
            .add_jump_placeholder(node, "jump over payload ");
        // NONE case ---------------------------------------------------------------
        self.builder.patch_jump_here(jump_if_none);

        // Emit fallback, right hand is always of the correct type
        self.emit_expression(dest, right, ctx);

        // join ------------------------------------------
        self.builder.patch_jump_here(jump_to_after_whole_thing);
    }
}
