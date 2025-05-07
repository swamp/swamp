/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::DetailedLocationRevised;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{
    Expression, LocationAccessKind, MutRefOrImmutableExpression, SingleLocationExpression,
};
use swamp_vm_types::MemoryOffset;
use swamp_vm_types::types::{BasicTypeKind, TypedRegister, VmType};

impl CodeBuilder<'_> {
    pub(crate) fn emit_for_access_or_location(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> TypedRegister {
        self.emit_expression_location_mut_ref_or_immutable(mut_or_immutable_expression)
    }

    pub(crate) fn emit_mut_or_immute(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_materialize(found_expression, ctx);
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.emit_lvalue_chain(location_expression);
            }
        }
    }

    pub(crate) fn emit_argument(
        &mut self,
        argument: &MutRefOrImmutableExpression,
        ctx: &Context,
        comment: &str,
    ) {
        match &argument {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_materialize(found_expression, ctx);
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.emit_absolute_pointer(location_expression, ctx, comment);
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_lvalue_chain(
        &mut self,
        location_expression: &SingleLocationExpression,
    ) -> DetailedLocationRevised {
        let mut current_register = self
            .variable_registers
            .get(
                &location_expression
                    .starting_variable
                    .unique_id_within_function,
            )
            .unwrap()
            .clone();

        let node = &location_expression.node;

        let chain_len = location_expression.access_chain.len();
        let accesses_count = chain_len;
        /* if true {
            chain_len.saturating_sub(1)
        } else {
            chain_len
        };
        */

        let mut last_memory_offset = MemoryOffset(0);

        // Loop over the consecutive accesses until we find the actual frame relative address (TypedRegister)
        for access in location_expression.access_chain.iter().take(accesses_count) {
            match &access.kind {
                LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                    let x = current_register.basic_type.frame_placed_type().unwrap();
                    let ty = x.ty().underlying();
                    let offset_item = ty.get_field_offset(*field_index).unwrap();

                    last_memory_offset = offset_item.offset;
                }
                LocationAccessKind::IntrinsicSubscript(
                    _intrinsic_function,
                    arguments_to_the_intrinsic,
                ) => {
                    // Fetching from vector, map, etc. are done using intrinsic calls
                    let (ctx, temp_reg) =
                        self.temp_space_for_type(&access.ty, "intrinsic subscript lvalue"); // TODO: should be more generic pointer
                    self.emit_collection_get(
                        node,
                        &current_register,
                        arguments_to_the_intrinsic,
                        &ctx,
                    );

                    current_register = ctx.register().clone();
                    last_memory_offset = MemoryOffset(0);
                }
            }
        }

        DetailedLocationRevised::Memory {
            base_ptr_reg: current_register,
            offset: last_memory_offset,
            ty: VmType::TempPointer,
        }
    }

    fn emit_collection_set(
        &mut self,
        node: &Node,
        self_collection: &TypedRegister,
        key_or_index: &[Expression],
        element_to_set: &TypedRegister,
    ) {
        let key_address = self.emit_expression_location(&key_or_index[0]);
        match &self_collection.ty().kind {
            BasicTypeKind::InternalStringPointer => {
                todo!()
            }
            BasicTypeKind::InternalVecPointer(expected_type) => {
                assert!(key_address.ty().is_int());
                assert_eq!(expected_type.total_size, element_to_set.size());
                self.builder.add_vec_set(
                    self_collection,
                    &key_address,
                    element_to_set,
                    node,
                    "copy back collection set (vec)",
                );
            }
            BasicTypeKind::InternalMapPointer(_, _) => {
                self.builder.add_map_set(
                    self_collection,
                    &key_address,
                    element_to_set,
                    node,
                    " copy back collection set (map)",
                );
            }
            _ => panic!("unknown collection"),
        }
    }

    fn emit_collection_get(
        &mut self,
        node: &Node,
        self_collection: &TypedRegister,
        key_or_index: &[Expression],
        ctx: &Context,
    ) {
        let key_address = self.emit_expression_location(&key_or_index[0]);
        match &self_collection.ty().kind {
            BasicTypeKind::InternalStringPointer => {
                todo!()
            }
            BasicTypeKind::InternalVecPointer(expected_item_type) => {
                assert!(key_address.ty().is_int());
                assert_eq!(expected_item_type.total_size, ctx.target_size());
                self.builder.add_vec_get(
                    ctx.register(),
                    self_collection,
                    &key_address,
                    node,
                    "collection get (vec)",
                );
            }
            BasicTypeKind::InternalMapPointer(_, _) => {
                self.builder.add_map_fetch(
                    ctx.register(),
                    self_collection,
                    &key_address,
                    node,
                    "collection get (map)",
                );
            }
            _ => panic!("unknown collection"),
        }
    }
}
