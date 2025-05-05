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
use swamp_vm_types::MemoryOffset;
use swamp_vm_types::types::{AssignmentTarget, BasicTypeKind, TypedRegister};

impl FunctionCodeGen<'_> {
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

    pub(crate) fn emit_expression_location_mut_ref_or_immutable(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
    ) -> TypedRegister {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_location(found_expression)
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                let x = self.emit_lvalue_chain(location_expression);
                // TODO: FIX THIS
                if let AssignmentTarget::Register(found) = x {
                    found
                } else {
                    panic!("expected register")
                }
            }
        }
    }

    /*
           if let Some(value_to_assign) = source_value_to_assign {
               let mut collection_access_index = None;
               let mut collection_args = None;

               for (i, access) in location_expression.access_chain.iter().enumerate() {
                   if let LocationAccessKind::IntrinsicCallMut(_, args) = &access.kind {
                       collection_access_index = Some(i);
                       collection_args = Some(args.clone());
                   }
               }

               // Check what kind of access the last one was
               if let Some(last) = location_expression.access_chain.last() {
                   match &last.kind {
                       LocationAccessKind::IntrinsicCallMut(_, args) => {
                           // Direct collection access - set element directly
                           self.emit_collection_set(
                               node,
                               &current_register,
                               args,
                               &value_to_assign,
                           );
                       }
                       LocationAccessKind::FieldIndex(_, field_index) => {
                           // Field access - set the field
                           let field_target = current_register.move_to_field(*field_index);
                           self.builder.add_mov_for_assignment(
                               &field_target,
                               &value_to_assign,
                               node,
                               &format!("assign to field {}", field_target.ty()),
                           );

                           // If this field is inside an object from a collection, copy the object back
                           if let (Some(idx), Some(args)) = (collection_access_index, &collection_args)
                           {
                               // We need the container and address of the object that contains the field
                               // Get the container by going back in the chain to before the collection access
                               let mut container = self
                                   .variable_registers
                                   .get(
                                       &location_expression
                                           .starting_variable
                                           .unique_id_within_function,
                                   )
                                   .unwrap()
                                   .clone();

                               for i in 0..idx {
                                   match &location_expression.access_chain[i].kind {
                                       LocationAccessKind::FieldIndex(_, field_idx) => {
                                           container = container.move_to_field(*field_idx);
                                       }
                                       _ => {} // Skip other access types (shouldn't happen before collection)
                                   }
                               }

                               // Copy the modified object back to the collection
                               self.emit_collection_set(
                                   node,
                                   &container,
                                   args,
                                   &current_register,
                               );
                           }
                       }
                   }
               } else {
                   // Direct variable assignment (no access chain)
                   self.builder.add_mov_for_assignment(
                       &current_register,
                       &value_to_assign,
                       node,
                       "direct variable assignment",
                   );
               }
           }


    */

    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_lvalue_chain(
        &mut self,
        location_expression: &SingleLocationExpression,
    ) -> AssignmentTarget {
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
                    let ty = x.underlying();
                    let offset_item = ty.get_field_offset(*field_index).unwrap();

                    last_memory_offset = offset_item.offset;
                }
                LocationAccessKind::IntrinsicCallMut(
                    _intrinsic_function,
                    arguments_to_the_intrinsic,
                ) => {
                    // Fetching from vector, map, etc. are done using intrinsic calls
                    let ctx = self.temp_space_for_type(&access.ty, "intrinsic call mut"); // TODO: should be more generic pointer
                    self.emit_collection_get(
                        node,
                        &current_register,
                        arguments_to_the_intrinsic,
                        &ctx,
                    );

                    current_register = ctx.target_register().clone();
                    last_memory_offset = MemoryOffset(0);
                }
            }
        }

        AssignmentTarget::MemoryLocation(current_register, last_memory_offset)
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
                    ctx.target_register(),
                    self_collection,
                    &key_address,
                    node,
                    "collection get (vec)",
                );
            }
            BasicTypeKind::InternalMapPointer(_, _) => {
                self.builder.add_map_fetch(
                    ctx.target_register(),
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
