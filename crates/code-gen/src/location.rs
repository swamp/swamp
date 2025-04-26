/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ctx::Context;
use crate::{Error, FunctionCodeGen};
use swamp_semantic::{
    Expression, LocationAccessKind, MutRefOrImmutableExpression, SingleLocationExpression,
};
use swamp_vm_types::types::FramePlacedType;

impl FunctionCodeGen<'_> {
    pub(crate) fn emit_for_access_or_location(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> Result<FramePlacedType, Error> {
        self.emit_expression_location_mut_ref_or_immutable(mut_or_immutable_expression)
    }

    pub(crate) fn emit_mut_or_immute(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) -> Result<(), Error> {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_materialize(found_expression, ctx)?;
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.emit_lvalue_chain(location_expression, None)?;
            }
        }

        Ok(())
    }

    pub(crate) fn emit_argument(
        &mut self,
        argument: &MutRefOrImmutableExpression,
        ctx: &Context,
        comment: &str,
    ) -> Result<(), Error> {
        match &argument {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_materialize(found_expression, ctx)?;
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.emit_location_argument(location_expression, ctx, comment)?;
            }
        }
        Ok(())
    }

    pub(crate) fn emit_expression_location_mut_ref_or_immutable(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> Result<FramePlacedType, Error> {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_location(found_expression)
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.emit_lvalue_chain(location_expression, None)
            }
        }
    }

    pub(crate) fn emit_lvalue_chain(
        &mut self,
        location_expression: &SingleLocationExpression,
        source_value_to_assign: Option<FramePlacedType>,
    ) -> Result<FramePlacedType, Error> {
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

        // Loop over the consecutive accesses until we find the actual frame relative address (FramePlacedType)
        for access in &location_expression.access_chain {
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
                    self.emit_single_intrinsic_call_with_self(
                        &location_expression.node,
                        intrinsic_function,
                        Some(access.ty.clone()),
                        Some(frame_relative_base_address.clone()),
                        &converted,
                        &ctx,
                    )?;

                    frame_relative_base_address = ctx.target().clone();
                    intermediates.push(frame_relative_base_address.clone());
                }
            }
        }

        // If this is an assignment (LHS), do the assignment and copy-back
        if let Some(value_to_assign) = source_value_to_assign {
            self.builder.add_mov_for_assignment(
                intermediates.last().unwrap(),
                &value_to_assign,
                &node,
                "assign the rightmost value",
            );

            // Right-to-left: walk the chain in reverse for copy-back
            // (skip the last, which we already did above)
            for (i, access) in location_expression
                .access_chain
                .iter()
                .enumerate()
                .rev()
                .skip(1)
            {
                match &access.kind {
                    LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                        // Set the field in the parent struct
                        let parent_addr = &intermediates[i];
                        let child_addr = &intermediates[i + 1];
                        let parent_field_target = parent_addr.move_to_field(*field_index);
                        self.builder.add_mov_for_assignment(
                            &parent_field_target,
                            &child_addr,
                            &node,
                            "assign the rightmost value",
                        );
                    }
                    LocationAccessKind::IntrinsicCallMut(
                        intrinsic_function,
                        arguments_to_the_intrinsic,
                    ) => {
                        // Set the value in the parent container (map_set, vec_set, etc.)
                        let parent_addr = &intermediates[i];
                        let child_addr = &intermediates[i + 1];
                        let mut converted = Vec::new();
                        for x in arguments_to_the_intrinsic {
                            converted.push(MutRefOrImmutableExpression::Expression(x.clone()));
                        }
                        /*
                        self.emit_collection_set(
                            &location_expression.node,
                            intrinsic_function,
                            Some(parent_addr.clone()),
                            &converted,
                            child_addr,
                        )?
                        ;
                         */
                    }
                }
            }
        }

        Ok(frame_relative_base_address)
    }
}
