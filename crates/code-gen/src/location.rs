/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ctx::Context;
use crate::{Error, FunctionCodeGen};
use swamp_semantic::{LocationAccessKind, MutRefOrImmutableExpression, SingleLocationExpression};
use swamp_vm_types::types::FramePlacedType;

impl FunctionCodeGen<'_> {
    pub(crate) fn gen_for_access_or_location(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> Result<FramePlacedType, Error> {
        self.gen_expression_location_mut_ref_or_immutable(&mut_or_immutable_expression)
    }

    pub(crate) fn gen_mut_or_immute(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) -> Result<(), Error> {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.gen_expression_materialize(found_expression, ctx)?;
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.gen_lvalue_address(location_expression)?;
            }
        }

        Ok(())
    }

    pub(crate) fn gen_argument(
        &mut self,
        argument: &MutRefOrImmutableExpression,
        ctx: &Context,
        comment: &str,
    ) -> Result<(), Error> {
        match &argument {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.gen_expression_materialize(found_expression, ctx)?;
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.gen_location_argument(location_expression, ctx, comment)?;
            }
        }
        Ok(())
    }

    pub(crate) fn gen_expression_location_mut_ref_or_immutable(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> Result<FramePlacedType, Error> {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.gen_expression_location(found_expression)
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.gen_lvalue_address(location_expression)
            }
        }
    }

    pub(crate) fn gen_lvalue_address(
        &mut self,
        location_expression: &SingleLocationExpression,
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

        // Loop over the consecutive accesses until we find the actual location
        for access in &location_expression.access_chain {
            match &access.kind {
                LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                    /*let (memory_offset, memory_size, _max_alignment) =
                       Self::get_struct_field_offset(
                           &anonymous_struct_type.field_name_sorted_fields,
                           *field_index,
                       );

                    */
                    frame_relative_base_address =
                        frame_relative_base_address.move_to_field(*field_index);
                }
                LocationAccessKind::IntrinsicCallMut(
                    intrinsic_function,
                    arguments_to_the_intrinsic,
                ) => {
                    // Fetching from vector, map, etc. are done using intrinsic calls
                    // arguments can be things like the key_value or the int index in a vector
                    let mut converted = Vec::new();
                    for x in arguments_to_the_intrinsic {
                        converted.push(MutRefOrImmutableExpression::Expression(x.clone()));
                    }

                    let ctx = self.temp_space_for_type(&access.ty, "intrinsic call mut");
                    self.gen_single_intrinsic_call_with_self(
                        &location_expression.node,
                        intrinsic_function,
                        Some(access.ty.clone()),
                        Some(frame_relative_base_address.clone()),
                        &converted,
                        &ctx,
                    )?;
                }
            }
        }

        Ok(frame_relative_base_address.clone())
    }
}
