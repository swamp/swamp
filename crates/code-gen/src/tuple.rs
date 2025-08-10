/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{Expression, VariableRef};
use swamp_types::TypeRef;
use swamp_vm_isa::MemoryOffset;
use swamp_vm_types::types::{BasicTypeKind, VmType};
use swamp_vm_types::{AggregateMemoryLocation, MemoryLocation};

impl CodeBuilder<'_> {
    pub(crate) fn emit_tuple_literal_into_memory(
        &mut self,
        aggregate_lvalue_location: &AggregateMemoryLocation,
        tuple_type_ref: &TypeRef,
        expressions: &[Expression],
        ctx: &Context,
        node: &Node,
    ) {
        let gen_tuple_type_ref = self.state.layout_cache.layout(tuple_type_ref);
        let BasicTypeKind::Tuple(gen_tuple_type) = &gen_tuple_type_ref.kind else {
            panic!("something is wrong");
        };

        // TODO: Bring this back. //assert_eq!(gen_tuple_placed.total_size, target_reg.size());
        // TODO: Bring this back. //assert_eq!(gen_tuple_type.fields.len(), expressions.len());

        for (offset_item, expr) in gen_tuple_type.fields.iter().zip(expressions) {
            let target_memory_location_for_tuple_item =
                aggregate_lvalue_location.offset(offset_item.offset, offset_item.ty.clone());

            // We don't need to initialize memory here since the caller (enum_variant) has already done it
            // or the expression itself will handle initialization

            self.emit_expression_into_target_memory(
                &target_memory_location_for_tuple_item.location,
                expr,
                &format!("emit tuple item {}", offset_item.name),
                ctx,
            );
        }
    }

    pub(crate) fn emit_tuple_destructuring(
        &mut self,
        target_variables: &[VariableRef],
        tuple_type_ref: &TypeRef,
        source_tuple_expression: &Expression,
        context: &Context,
    ) {
        let tuple_base_pointer_reg = self.emit_scalar_rvalue(source_tuple_expression, context);

        let gen_tuple_type_ref = self.state.layout_cache.layout(tuple_type_ref);
        let BasicTypeKind::Tuple(tuple_type) = &gen_tuple_type_ref.kind else {
            panic!("something is wrong");
        };
        // TODO: Bring this back//assert_eq!(tuple_type.total_size.0, tuple_base_pointer_reg.size().0);

        for (tuple_index, target_variable) in target_variables.iter().enumerate() {
            if target_variable.is_unused {} else {
                let frame_placed_target_variable_register =
                    self.get_variable_register(target_variable).clone();

                //                assert_eq!(frame_placed_target_variable.size().0, offset_item.size.0);

                let field_offset_item = &tuple_type.fields[tuple_index];

                let source_memory_location = MemoryLocation {
                    base_ptr_reg: tuple_base_pointer_reg.clone(),
                    offset: field_offset_item.offset,
                    ty: VmType::new_unknown_placement(field_offset_item.ty.clone()),
                };

                //let source_location = Destination::new_location(source_memory_location);

                if frame_placed_target_variable_register.ty.is_scalar() {
                    self.emit_load_value_from_memory_source(
                        &frame_placed_target_variable_register,
                        &source_memory_location,
                        &target_variable.name,
                        "load from memory into variable register",
                    );
                } else {
                    let target_memory_location = MemoryLocation {
                        ty: frame_placed_target_variable_register.ty.clone(),
                        base_ptr_reg: frame_placed_target_variable_register,
                        offset: MemoryOffset(0),
                    };
                    let source_memory_location = MemoryLocation {
                        base_ptr_reg: tuple_base_pointer_reg.clone(),
                        offset: field_offset_item.offset,
                        ty: VmType::new_unknown_placement(field_offset_item.ty.clone()),
                    };
                    self.emit_block_copy_with_size_from_location(
                        &target_memory_location,
                        &source_memory_location,
                        &target_variable.name,
                        "copy from tuple field to destination variable",
                    );
                }
            }
        }
    }
}
