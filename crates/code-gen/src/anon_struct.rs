/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::FlagState;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{AnonymousStructLiteral, Expression};
use swamp_types::{TypeKind, TypeRef};
use swamp_vm_types::AggregateMemoryLocation;
use swamp_vm_types::types::BasicTypeKind;

impl CodeBuilder<'_> {
    pub(crate) fn emit_anonymous_struct_into_memory(
        &mut self,
        aggregate_lvalue_memory_location: &AggregateMemoryLocation,
        anon_struct_type: &TypeRef,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        node: &Node,
        base_context: &Context,
    ) {
        self.emit_struct_literal_into_memory_location(
            aggregate_lvalue_memory_location,
            anon_struct_type,
            source_order_expressions,
            node,
            "store struct into memory",
            base_context,
        );
    }

    // TODO: Bring this back // assert_eq!(target_reg.size().0, gen_source_struct_type.total_size.0);
    /* TODO: Bring this back
    assert_eq!(
        source_order_expressions.len(),
        gen_source_struct_type.fields.len()
    );
    */
    fn emit_struct_literal_into_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        struct_type_ref: &TypeRef,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> FlagState {
        let hwm = self.temp_registers.save_mark();
        let struct_type = self.state.layout_cache.layout(struct_type_ref);

        let BasicTypeKind::Struct(gen_source_struct_type) = &struct_type.kind else {
            panic!("must be named struct type or anon struct literal")
        };

        // We need to initialize collections in the struct before filling them
        self.emit_initialize_collections_in_type(
            lvalue_location,
            &struct_type,
            node,
            &format!("{comment} - initialize collections in struct"),
        );

        for (offset_item, (field_index, _node, source_expression)) in gen_source_struct_type
            .fields
            .iter()
            .zip(source_order_expressions)
        {
            let real_offset_item = struct_type.get_field_offset(*field_index).unwrap();
            let safe_output_aggregate_location_with_offset =
                lvalue_location.offset(real_offset_item.offset, real_offset_item.ty.clone());

            // Now emit the expression into the target memory
            self.emit_expression_into_target_memory(
                &safe_output_aggregate_location_with_offset.location,
                source_expression,
                &format!(
                    "store expression (using safe output reg) into struct field {}:{}",
                    field_index, offset_item.name
                ),
                ctx,
            );
        }

        self.temp_registers.restore_to_mark(hwm);

        FlagState::default()
    }

    pub(crate) fn emit_anonymous_struct_literal_into_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        anon_struct_literal: &AnonymousStructLiteral,
        ty: &TypeRef,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> FlagState {
        let anon_struct_type = match &*ty.kind {
            TypeKind::NamedStruct(named_struct) => named_struct.anon_struct_type.clone(),
            TypeKind::AnonymousStruct(_anon_struct_type) => ty.clone(),
            _ => panic!("internal error with struct literal"),
        };

        self.emit_struct_literal_into_memory_location(
            lvalue_location,
            &anon_struct_type,
            &anon_struct_literal.source_order_expressions,
            node,
            comment,
            ctx,
        )
    }
}
