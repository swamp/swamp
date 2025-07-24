/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
//! equality functions for the emitter
//!
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::{FlagState, FlagStateKind};
use source_map_node::Node;
use swamp_vm_types::PointerLocation;
use swamp_vm_types::types::{BasicTypeKind, TypedRegister};

impl CodeBuilder<'_> {
    pub fn emit_equality_to_bool_target(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &TypedRegister,
        is_equal: bool,
        right_source: &TypedRegister,
        node: &Node,
        ctx: &Context,
    ) -> FlagState {
        let polarity = match (
            &left_source.ty.basic_type.kind,
            &right_source.ty.basic_type.kind,
        ) {
            (BasicTypeKind::U8, BasicTypeKind::U8)
            | (BasicTypeKind::B8, BasicTypeKind::B8)
            | (BasicTypeKind::S32, BasicTypeKind::S32)
            | (BasicTypeKind::Fixed32, BasicTypeKind::Fixed32) => {
                self.emit_binary_operator_equal_reg(dest_bool_reg, left_source, node, right_source)
            }
            // Any comparison involving StringStorage: do content comparison
            // TODO: All vec liked should use this
            (
                BasicTypeKind::StringView { .. } | BasicTypeKind::StringStorage { .. },
                BasicTypeKind::StringView { .. } | BasicTypeKind::StringStorage { .. },
            ) => {
                let left_source_ptr_reg = PointerLocation::new(left_source.clone());
                let right_source_ptr_reg = PointerLocation::new(right_source.clone());
                self.emit_binary_operator_block_cmp_vec_like(
                    dest_bool_reg,
                    &left_source_ptr_reg,
                    node,
                    &right_source_ptr_reg,
                )
            }
            (BasicTypeKind::TaggedUnion(a), BasicTypeKind::TaggedUnion(b)) => {
                // TODO: Make simpler case if enum variants are without payload
                // a.are_all_variants_without_payload()

                self.emit_binary_operator_block_cmp(dest_bool_reg, left_source, node, right_source)
            }
            _ => {
                self.emit_binary_operator_block_cmp(dest_bool_reg, left_source, node, right_source)
            }
        };

        if is_equal {
            polarity
        } else {
            polarity.invert_polarity()
        }
    }

    fn emit_binary_operator_equal_reg(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> FlagState {
        self.builder.add_cmp_reg(
            dest_bool_reg,
            left_source,
            right_source,
            node,
            "compare reg and set result to bool reg",
        );

        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_block_cmp(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> FlagState {
        self.builder.add_block_cmp(
            dest_bool_reg,
            left_source,
            right_source,
            left_source.size(),
            node,
            "compare block",
        );

        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_block_cmp_vec_like(
        &mut self,
        dest_bool_reg: &TypedRegister,
        left_source: &PointerLocation,
        node: &Node,
        right_source: &PointerLocation,
    ) -> FlagState {
        self.builder.add_vec_cmp(
            dest_bool_reg,
            &left_source.ptr_reg,
            &right_source.ptr_reg,
            node,
            "compare vec like",
        );

        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }
}
