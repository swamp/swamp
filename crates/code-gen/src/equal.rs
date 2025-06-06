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
use swamp_vm_types::types::{BasicTypeKind, TypedRegister};

impl CodeBuilder<'_> {
    pub fn emit_binary_operator_equal_to_t_flag_only(
        &mut self,
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
            (BasicTypeKind::B8, BasicTypeKind::B8)
            | (BasicTypeKind::S32, BasicTypeKind::S32)
            | (BasicTypeKind::Fixed32, BasicTypeKind::Fixed32) => {
                self.emit_binary_operator_equal_reg(left_source, node, right_source)
            }
            (BasicTypeKind::InternalStringPointer, BasicTypeKind::InternalStringPointer) => {
                self.emit_binary_operator_string_equal(left_source, node, right_source)
            }
            (BasicTypeKind::TaggedUnion(a), BasicTypeKind::TaggedUnion(b)) => {
                // TODO: Make simpler case if enum variants are without payload
                // a.are_all_variants_without_payload()

                self.emit_binary_operator_block_cmp(left_source, node, right_source)
            }
            _ => self.emit_binary_operator_block_cmp(left_source, node, right_source),
        };

        if is_equal {
            polarity
        } else {
            polarity.invert_polarity()
        }
    }

    fn emit_binary_operator_equal_reg(
        &mut self,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> FlagState {
        self.builder.add_cmp_reg(
            left_source,
            right_source,
            node,
            "compare reg and set result to P flag",
        );

        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_string_equal(
        &mut self,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> FlagState {
        self.builder
            .add_string_cmp(left_source, right_source, node, "compare strings");

        FlagState {
            kind: FlagStateKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_block_cmp(
        &mut self,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> FlagState {
        self.builder.add_block_cmp(
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
}
