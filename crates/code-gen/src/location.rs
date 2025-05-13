/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::DetailedLocation;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::reg_pool::TempRegister;
use source_map_node::Node;
use swamp_semantic::{
    Expression, LocationAccessKind, MutRefOrImmutableExpression, SingleLocationExpression,
};
use swamp_types::Type;
use swamp_vm_types::MemoryOffset;
use swamp_vm_types::types::{BasicTypeKind, TypedRegister, VmType, unknown_type};

impl CodeBuilder<'_> {
    pub(crate) fn emit_for_access_or_location(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        context: &Context,
    ) -> TypedRegister {
        self.emit_expression_location_mut_ref_or_immutable(mut_or_immutable_expression, context)
    }

    pub(crate) fn emit_mut_or_immute(
        &mut self,
        target_reg: &TypedRegister,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_materialize(target_reg, found_expression, ctx);
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                let location = self.emit_lvalue_chain(location_expression, ctx);
                //TODO: move location to target_reg // self.emit_ptr_reg_from_detailed_location()
            }
        }
    }

    /*
    pub(crate) fn emit_argument(
        &mut self,
        target_reg: &TypedRegister,
        argument: &MutRefOrImmutableExpression,
        ctx: &Context,
        comment: &str,
    ) {
        match &argument {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_expression_materialize(target_reg, found_expression, ctx);
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                self.emit_absolute_pointer(target_reg, location_expression, ctx, comment);
            }
        }
    }

     */

    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_lvalue_chain(
        &mut self,
        location_expression: &SingleLocationExpression,
        ctx: &Context,
    ) -> DetailedLocation {
        let mut start_reg = self
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

        let mut current_location = DetailedLocation::Register { reg: start_reg };
        let mut temp_regs = Vec::new();

        // Loop over the consecutive accesses until we find the actual frame relative address (TypedRegister)
        for access in location_expression.access_chain.iter().take(accesses_count) {
            match &access.kind {
                LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                    let ty = current_location.vm_type().underlying();
                    let offset_item = ty.get_field_offset(*field_index).unwrap();

                    current_location = current_location.add_offset(
                        offset_item.offset,
                        VmType::new_unknown_placement(offset_item.ty.clone()),
                    );
                }
                LocationAccessKind::IntrinsicSubscript(
                    _intrinsic_function,
                    arguments_to_the_intrinsic,
                ) => {
                    let layout_item_type = layout_type(&access.ty);
                    let get_item_target_reg = self.temp_registers.allocate(
                        VmType::new_unknown_placement(layout_item_type),
                        "intrinsic subscript",
                    );
                    let (collection_reg, maybe_temp_collection_reg) = self
                        .emit_ptr_reg_from_detailed_location(
                            current_location,
                            &access.node,
                            "lvalue chain",
                        );

                    // Fetching from vector, map, etc. are done using intrinsic calls

                    self.emit_collection_get(
                        node,
                        &collection_reg,
                        arguments_to_the_intrinsic,
                        get_item_target_reg.register(),
                        ctx,
                    );

                    if let Some(save_temp_reg) = maybe_temp_collection_reg {
                        temp_regs.push(save_temp_reg);
                    }

                    current_location = DetailedLocation::Register {
                        reg: get_item_target_reg.register,
                    }
                }
            }
        }

        self.temp_registers.free_multiple(temp_regs);

        current_location
    }

    fn emit_collection_set(
        &mut self,
        node: &Node,
        self_collection: &TypedRegister,
        key_or_index: &[Expression],
        element_to_set: &TypedRegister,
        ctx: &Context,
    ) {
        let key_address = self.emit_rvalue(&key_or_index[0], ctx);
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
        target_register: &TypedRegister,
        ctx: &Context,
    ) {
        // TODO: Fix this
        let key_address = self.emit_rvalue(&key_or_index[0], ctx);

        match &self_collection.underlying().kind {
            BasicTypeKind::InternalStringPointer => {
                todo!()
            }
            BasicTypeKind::InternalVecPointer(expected_item_type) => {
                assert!(key_address.ty().is_int());
                //assert_eq!(expected_item_type.total_size, ctx.target_size());
                /*
                self.builder.add_vec_get(
                    self_collection.register(),
                    self_collection,
                    &key_address,
                    node,
                    "collection get (vec)",
                );

                 */
            }
            BasicTypeKind::InternalMapPointer(_, value_type) => {
                /*
                self.builder.add_map_fetch(
                    ctx.register(),
                    self_collection,
                    &key_address,
                    node,
                    "collection get (map)",
                );

                 */
            }
            _ => panic!("unknown collection"),
        }
    }
}
