/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::DetailedLocation;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use source_map_node::Node;
use swamp_semantic::{
    Expression, LocationAccessKind, MutRefOrImmutableExpression, SingleLocationExpression,
};
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, BoundsCheck, TypedRegister, VmType, int_type, u16_type,
};
use swamp_vm_types::{MemoryOffset, VEC_HEADER_COUNT_OFFSET, VEC_HEADER_PAYLOAD_OFFSET};

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

    pub fn subscript_helper_from_location_to_location(
        &mut self,
        detailed_location_to_slice: DetailedLocation,
        element_basic_type: &BasicType,
        int_expr: &Expression,
        bounds_check: BoundsCheck,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> DetailedLocation {
        let ptr_to_slice_reg = self.emit_ptr_reg_from_detailed_location(
            &detailed_location_to_slice,
            node,
            &format!(
                "{comment} (get the the base pointer to the start of where to subscript from)"
            ),
        );
        //let basic_slice_type = layout_type(&Type::Slice(Box::new(*slice_type.element.clone()), slice_type.fixed_size));
        let new_base_pointer_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(element_basic_type.clone()),
            &format!("{comment} (new_base_pointer reg temp)"),
        );

        self.subscript_helper(
            new_base_pointer_reg.register(),
            &ptr_to_slice_reg,
            &element_basic_type,
            bounds_check,
            int_expr,
            node,
            comment,
            ctx,
        );

        // We continue the chain from the calculated pointer
        DetailedLocation::Memory {
            ty: new_base_pointer_reg.register.ty.clone(),
            base_ptr_reg: new_base_pointer_reg.register,
            offset: MemoryOffset(0),
        }
    }

    pub fn subscript_helper(
        &mut self,
        target_reg: &TypedRegister,
        ptr_to_slice_reg: &TypedRegister,
        element_basic_type: &BasicType,
        bounds_check: BoundsCheck,
        int_expr: &Expression,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) {
        let index_int_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(int_type()),
            "subscript unsigned int",
        );
        self.emit_expression_materialize(index_int_reg.register(), int_expr, ctx);

        let reg_to_use_for_upper_bound = match bounds_check {
            BoundsCheck::KnownSizeAtCompileTime(max_length) => {
                let element_capacity_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(int_type()),
                    "element capacity",
                );
                self.builder.add_mov_16_immediate_value(
                    element_capacity_reg.register(),
                    max_length,
                    node,
                    "set the capacity",
                );

                element_capacity_reg.register
            }
            BoundsCheck::RegisterWithMaxCount(reg) => reg,
        };

        // Bounds check it
        self.builder.add_ge_u32(
            index_int_reg.register(),
            &reg_to_use_for_upper_bound,
            node,
            &format!("check if it is >= capacity {comment}"),
        );
        let patch = self
            .builder
            .add_jmp_if_not_true_placeholder(node, "jump over panic if within bounds");
        self.builder.add_trap(5, node, "out of bounds trap");
        self.builder.patch_jump_here(patch);

        //let element_basic_type = layout_type(&slice_type.element);

        let element_size_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(int_type()),
            &format!("reg for immediate element size: {comment}"),
        );
        self.builder.add_mov_32_immediate_value(
            element_size_reg.register(),
            element_basic_type.total_size.0 as u32,
            node,
            &format!("element_size: {comment}"),
        );

        let offset_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(int_type()), "temp for offset");
        self.builder.add_mul_i32(
            offset_reg.register(),
            index_int_reg.register(),
            element_size_reg.register(),
            node,
            &format!("offset = index * element_size ({comment})"),
        );

        self.builder.add_add_u32(
            target_reg,
            &ptr_to_slice_reg,
            offset_reg.register(),
            node,
            &format!("result = base_ptr + element_size * index ({comment})"),
        );
    }

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
                LocationAccessKind::Subscript(slice_type, int_expr) => {
                    let element_gen_type = layout_type(&slice_type.element);
                    current_location = self.subscript_helper_from_location_to_location(
                        current_location,
                        &element_gen_type,
                        int_expr,
                        BoundsCheck::KnownSizeAtCompileTime(slice_type.fixed_size as u16),
                        &int_expr.node,
                        "subscript",
                        ctx,
                    );
                }
                LocationAccessKind::SubscriptVec(element_type, int_expression) => {
                    current_location = self.vec_subscript_helper(
                        &current_location,
                        element_type,
                        int_expression,
                        ctx,
                    )
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
                    let collection_reg = self.emit_ptr_reg_from_detailed_location(
                        &current_location,
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

                    current_location = DetailedLocation::Register {
                        reg: get_item_target_reg.register,
                    }
                }
            }
        }

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
        let key_address = self.emit_simple_rvalue(&key_or_index[0], ctx);
        match &self_collection.ty().kind {
            BasicTypeKind::InternalStringPointer => {
                todo!()
            }
            BasicTypeKind::InternalVecView(expected_type) => {
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
        let key_address = self.emit_simple_rvalue(&key_or_index[0], ctx);

        match &self_collection.underlying().kind {
            BasicTypeKind::InternalStringPointer => {
                todo!()
            }
            BasicTypeKind::InternalVecView(expected_item_type) => {
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
