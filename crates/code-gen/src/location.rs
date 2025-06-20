/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{ArgumentExpression, Expression};
use swamp_vm_types::types::{
    BasicTypeKind, BasicTypeRef, BoundsCheck, Destination, RValueOrLValue, TypedRegister, VmType,
    b8_type, int_type,
};
use swamp_vm_types::{MemoryLocation, MemoryOffset};

impl CodeBuilder<'_> {
    pub(crate) fn emit_for_access_or_location(
        &mut self,
        mut_or_immutable_expression: &ArgumentExpression,
        context: &Context,
    ) -> RValueOrLValue {
        self.emit_argument_expression(mut_or_immutable_expression, context)
    }

    pub(crate) fn emit_argument_expression_binding(
        &mut self,
        target_reg: &TypedRegister,
        argument_expression: &ArgumentExpression,
        ctx: &Context,
    ) {
        match &argument_expression {
            ArgumentExpression::Expression(found_expression) => {
                if found_expression.ty.is_primitive() {
                    self.emit_expression_into_register(
                        target_reg,
                        found_expression,
                        "emit mut or immute",
                        ctx,
                    );
                }
            }
            ArgumentExpression::BorrowMutableReference(location_expression) => {
                let location = self.emit_lvalue_address(location_expression, ctx);
                //TODO: move location to target_reg // self.emit_ptr_reg_from_detailed_location()
            }
        }
    }
    pub(crate) fn emit_argument_expression(
        &mut self,
        mut_or_immutable_expression: &ArgumentExpression,
        ctx: &Context,
    ) -> RValueOrLValue {
        match &mut_or_immutable_expression {
            ArgumentExpression::Expression(found_expression) => {
                RValueOrLValue::Scalar(self.emit_scalar_rvalue(found_expression, ctx))
            }
            ArgumentExpression::BorrowMutableReference(location_expression) => {
                RValueOrLValue::Memory(self.emit_lvalue_address(location_expression, ctx))
            }
        }
    }

    pub fn subscript_helper_from_location_to_location(
        &mut self,
        detailed_location_to_slice: Destination,
        element_basic_type: &BasicTypeRef,
        int_expr: &Expression,
        bounds_check: BoundsCheck,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> Destination {
        let ptr_to_slice_reg = self.emit_compute_effective_address_to_register(
            &detailed_location_to_slice,
            node,
            &format!(
                "{comment} (get the the base pointer to the start of where to subscript from)"
            ),
        );
        //let basic_slice_type = layout_type(&TypeKind::Slice(Box::new(*slice_type.element.clone()), slice_type.fixed_size));
        let new_base_pointer_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(element_basic_type.clone()),
            &format!("{comment} (new_base_pointer reg temp)"),
        );

        self.subscript_helper(
            new_base_pointer_reg.register(),
            &ptr_to_slice_reg,
            element_basic_type,
            bounds_check,
            int_expr,
            node,
            comment,
            ctx,
        );

        // We continue the chain from the calculated pointer
        Destination::Memory(MemoryLocation {
            ty: new_base_pointer_reg.register.ty.clone(),
            base_ptr_reg: new_base_pointer_reg.register,
            offset: MemoryOffset(0),
        })
    }

    #[allow(clippy::too_many_arguments)]
    pub fn subscript_helper(
        &mut self,
        target_reg: &TypedRegister,
        ptr_to_slice_reg: &TypedRegister,
        element_basic_type: &BasicTypeRef,
        bounds_check: BoundsCheck,
        int_expr: &Expression,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) {
        let index_int_reg = self.emit_scalar_rvalue(int_expr, ctx);

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

        let bool_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(b8_type()),
            "truth from bounds check",
        );
        // Bounds check it
        self.builder.add_ge_u32(
            &bool_reg.register,
            &index_int_reg,
            &reg_to_use_for_upper_bound,
            node,
            &format!("check if it is >= len {comment}"),
        );
        let patch = self.builder.add_jmp_if_not_true_placeholder(
            &bool_reg.register,
            node,
            "jump over trap if within bounds",
        );
        self.builder.add_trap(5, node, "out of bounds trap");
        self.builder.patch_jump_here(patch);

        //let element_basic_type = layout_type(&slice_type.element);

        let element_size_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(int_type()),
            &format!("reg for immediate element size: {comment}"),
        );
        self.builder.add_mov_32_immediate_value(
            element_size_reg.register(),
            element_basic_type.total_size.0,
            node,
            &format!("element_size: {comment}"),
        );

        let offset_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(int_type()), "temp for offset");
        self.builder.add_mul_i32(
            offset_reg.register(),
            &index_int_reg,
            element_size_reg.register(),
            node,
            &format!("offset = index * element_size ({comment})"),
        );

        self.builder.add_add_u32(
            target_reg,
            ptr_to_slice_reg,
            offset_reg.register(),
            node,
            &format!("result = base_ptr + element_size * index ({comment})"),
        );
    }

    fn emit_collection_set(
        &mut self,
        node: &Node,
        self_collection: &TypedRegister,
        key_or_index: &[Expression],
        element_to_set: &TypedRegister,
        ctx: &Context,
    ) {
        let key_address = self.emit_scalar_rvalue(&key_or_index[0], ctx);
        match &self_collection.ty().kind {
            BasicTypeKind::InternalStringPointer => {
                todo!()
            }
            BasicTypeKind::DynamicLengthVecView(expected_type) => {}
            BasicTypeKind::DynamicLengthMapView(_, _) => {}
            _ => panic!("unknown collection"),
        }
    }

    pub(crate) fn emit_collection_get(
        &mut self,
        node: &Node,
        self_collection: &TypedRegister,
        key_or_index: &[Expression],
        target_register: &TypedRegister,
        ctx: &Context,
    ) {
        // TODO: Fix this
        let key_address = self.emit_scalar_rvalue(&key_or_index[0], ctx);

        match &self_collection.underlying().kind {
            BasicTypeKind::InternalStringPointer => {
                todo!()
            }
            BasicTypeKind::DynamicLengthVecView(expected_item_type) => {
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
            BasicTypeKind::DynamicLengthMapView(_, value_type) => {
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
