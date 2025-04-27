/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::FunctionCodeGen;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{
    Expression, LocationAccessKind, MutRefOrImmutableExpression, SingleLocationExpression,
};
use swamp_vm_types::types::{BasicTypeKind, FramePlacedType};
use tracing::info;

impl FunctionCodeGen<'_> {
    pub(crate) fn emit_for_access_or_location(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> FramePlacedType {
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
                self.emit_lvalue_chain(location_expression, None);
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
                self.emit_location_argument(location_expression, ctx, comment);
            }
        }
    }

    pub(crate) fn emit_expression_location_mut_ref_or_immutable(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> FramePlacedType {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_location(found_expression)
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.emit_lvalue_chain(location_expression, None)
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_lvalue_chain(
        &mut self,
        location_expression: &SingleLocationExpression,
        source_value_to_assign: Option<FramePlacedType>,
    ) -> FramePlacedType {
        let mut frame_relative_base_address = self
            .variable_offsets
            .get(
                &location_expression
                    .starting_variable
                    .unique_id_within_function,
            )
            .unwrap()
            .clone();

        let node = &location_expression.node;

        let mut intermediates = vec![frame_relative_base_address.clone()];

        let chain_len = location_expression.access_chain.len();
        let accesses = if source_value_to_assign.is_some() {
            // For assignment, skip the last access
            location_expression.access_chain.iter().take(chain_len - 1)
        } else {
            // For RHS, it is a normal lookup so we just everything
            location_expression.access_chain.iter().take(chain_len)
        };

        // Loop over the consecutive accesses until we find the actual frame relative address (FramePlacedType)
        for access in accesses {
            match &access.kind {
                LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                    frame_relative_base_address =
                        frame_relative_base_address.move_to_field(*field_index);
                    intermediates.push(frame_relative_base_address.clone());
                }
                LocationAccessKind::IntrinsicCallMut(
                    intrinsic_function,
                    arguments_to_the_intrinsic,
                ) => {
                    // Fetching from vector, map, etc. are done using intrinsic calls
                    let mut converted = Vec::new();
                    for x in arguments_to_the_intrinsic {
                        converted.push(MutRefOrImmutableExpression::Expression(x.clone()));
                    }

                    let ctx = self.temp_space_for_type(&access.ty, "intrinsic call mut");
                    self.emit_collection_get(
                        node,
                        &frame_relative_base_address,
                        arguments_to_the_intrinsic,
                        &ctx,
                    );

                    frame_relative_base_address = ctx.target().clone();
                    let key_expression = &arguments_to_the_intrinsic[0];
                    intermediates.push(frame_relative_base_address.clone());
                }
            }
        }

        // If this is an assignment (LHS), do the assignment and copy-back
        if let Some(value_to_assign) = source_value_to_assign {
            let n = location_expression.access_chain.len();

            // to make the code more clean, we push in the value_to_assign as a last "intermediate"
            intermediates.push(value_to_assign);

            let mut collection_set_updated_field = false;
            // Process all accesses in reverse order
            for i in (0..n).rev() {
                let access = &location_expression.access_chain[i];
                let parent_addr = &intermediates[i];
                let child_addr = &intermediates[i + 1];
                match &access.kind {
                    LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                        if collection_set_updated_field {
                            let parent_field_target = parent_addr.move_to_field(*field_index);

                            // Skip the move since it was updated by the collection set
                            collection_set_updated_field = false;
                        } else {
                            // Set the field in the parent struct
                            let parent_field_target = parent_addr.move_to_field(*field_index);
                            self.builder.add_mov_for_assignment(
                                &parent_field_target,
                                child_addr,
                                node,
                                &format!("copy back field index {}", parent_field_target.ty()),
                            );
                        }
                    }
                    LocationAccessKind::IntrinsicCallMut(
                        _intrinsic_function,
                        arguments_to_the_intrinsic,
                    ) => {
                        // Set the value in the parent container (map_set, vec_set, etc.)
                        let key_expr = &arguments_to_the_intrinsic[0];
                        self.emit_collection_set(
                            &location_expression.node,
                            parent_addr,
                            arguments_to_the_intrinsic,
                            child_addr,
                        );
                        collection_set_updated_field = true;
                    }
                }
            }
        }

        frame_relative_base_address
    }

    fn emit_collection_set(
        &mut self,
        node: &Node,
        self_collection: &FramePlacedType,
        key_or_index: &[Expression],
        element_to_set: &FramePlacedType,
    ) {
        let key_address = self.emit_expression_location(&key_or_index[0]);
        match &self_collection.ty().kind {
            BasicTypeKind::InternalStringHeader => {
                todo!()
            }
            BasicTypeKind::InternalVecHeader => {
                assert!(key_address.ty().is_int());
                self.builder.add_vec_set(
                    self_collection,
                    &key_address,
                    element_to_set,
                    node,
                    "copy back collection set (vec)",
                );
            }
            BasicTypeKind::InternalMapHeader => {
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
        self_collection: &FramePlacedType,
        key_or_index: &[Expression],
        ctx: &Context,
    ) {
        let key_address = self.emit_expression_location(&key_or_index[0]);
        match &self_collection.ty().kind {
            BasicTypeKind::InternalStringHeader => {
                todo!()
            }
            BasicTypeKind::InternalVecHeader => {
                assert!(key_address.ty().is_int());
                self.builder.add_vec_get(
                    ctx.target(),
                    self_collection,
                    &key_address,
                    node,
                    "collection get (vec)",
                );
            }
            BasicTypeKind::InternalMapHeader => {
                self.builder.add_map_fetch(
                    ctx.target(),
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
