/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::FlagState;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_struct_type;
use source_map_node::Node;
use swamp_semantic::{AnonymousStructLiteral, Expression};
use swamp_types::{AnonymousStructType, Type};
use swamp_vm_types::types::{BasicType, BasicTypeKind, VmType, u16_type};
use swamp_vm_types::{
    AggregateMemoryLocation, COLLECTION_CAPACITY_OFFSET, COLLECTION_ELEMENT_COUNT_OFFSET, CountU16,
    MemoryLocation,
};

impl CodeBuilder<'_> {
    pub(crate) fn emit_anonymous_struct_into_memory(
        &mut self,
        aggregate_lvalue_memory_location: &AggregateMemoryLocation,
        anon_struct_type: &AnonymousStructType,
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

    fn emit_struct_literal_into_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        struct_type_ref: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> FlagState {
        let gen_source_struct_type = layout_struct_type(struct_type_ref, "");

        // TODO: Bring this back // assert_eq!(target_reg.size().0, gen_source_struct_type.total_size.0);
        /* TODO: Bring this back
        assert_eq!(
            source_order_expressions.len(),
            gen_source_struct_type.fields.len()
        );
        */

        let struct_type = BasicType {
            total_size: gen_source_struct_type.total_size,
            max_alignment: gen_source_struct_type.max_alignment,
            kind: BasicTypeKind::Struct(gen_source_struct_type.clone()),
        };

        for (offset_item, (field_index, _node, source_expression)) in gen_source_struct_type
            .fields
            .iter()
            .zip(source_order_expressions)
        {
            let real_offset_item = struct_type.get_field_offset(*field_index).unwrap();
            let modified_lvalue_location =
                lvalue_location.offset(real_offset_item.offset, real_offset_item.ty.clone());

            self.emit_initialize_target_memory_first_time(
                &modified_lvalue_location.location,
                node,
                &format!("{comment} - initialize struct literal field"),
            );

            self.emit_expression_into_target_memory(
                &modified_lvalue_location.location,
                source_expression,
                &format!(
                    "store expression into struct field {}:{}",
                    field_index, offset_item.name
                ),
                ctx,
            );
        }

        FlagState::default()
    }

    pub(crate) fn emit_anonymous_struct_literal_into_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        anon_struct_literal: &AnonymousStructLiteral,
        ty: &Type,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> FlagState {
        let anon_struct_type = match ty {
            Type::NamedStruct(named_struct) => named_struct.anon_struct_type.clone(),
            Type::AnonymousStruct(anon_struct_type) => anon_struct_type.clone(),
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

    pub(crate) fn emit_initialize_target_memory_first_time(
        &mut self,
        lvalue_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        match &lvalue_location.ty.underlying().kind {
            BasicTypeKind::SparseStorage(element_type, capacity) => {
                self.builder.add_sparse_init(
                    &lvalue_location.pointer_location().unwrap().ptr_reg,
                    element_type.total_size,
                    *capacity as u16,
                    node,
                    comment,
                );
            }
            BasicTypeKind::GridStorage(element_type, width, height) => {
                self.builder.add_grid_init(
                    &lvalue_location.pointer_location().unwrap().ptr_reg,
                    element_type.total_size,
                    *width as u16,
                    *height as u16,
                    node,
                    comment,
                );
            }
            BasicTypeKind::MapStorage {
                capacity,
                logical_limit,
                tuple_type,
                
                ..
            } => {
                let hwm = self.temp_registers.save_mark();

                let init_capacity_reg = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u16_type()),
                    &format!("{comment} - init map capacity reg"),
                );

                self.builder.add_mov_16_immediate_value(
                    init_capacity_reg.register(),
                    capacity.0,
                    node,
                    &format!("{comment} -set init map capacity value to {lvalue_location}"),
                );
                self.builder.add_st16_using_ptr_with_offset(
                    &lvalue_location.unsafe_add_offset(COLLECTION_CAPACITY_OFFSET),
                    init_capacity_reg.register(),
                    node,
                    &format!("{comment} - store capacity"),
                );
                self.temp_registers.restore_to_mark(hwm);

                let unaligned_key_size = tuple_type.fields[0].ty.total_size;
                let unaligned_value_size = tuple_type.fields[1].ty.total_size;

                let map_pointer_location = self
                    .emit_compute_effective_address_from_location_to_register(
                        lvalue_location,
                        node,
                        "find location of map to init",
                    );

                self.builder.add_map_init_set_capacity(
                    &map_pointer_location,
                    CountU16(*logical_limit as u16),
                    unaligned_key_size,
                    unaligned_value_size,
                    tuple_type.max_alignment,
                    node,
                    "initialize map (capacity, key_size, total_key_and_value_size)",
                );
            }
            _ => {
                if let Some(capacity) = lvalue_location.ty.underlying().get_collection_capacity() {
                    let hwm = self.temp_registers.save_mark();

                    let init_capacity_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(u16_type()),
                        &format!("{comment} - init vec slice capacity reg"),
                    );

                    self.builder.add_mov_16_immediate_value(
                        init_capacity_reg.register(),
                        capacity.0,
                        node,
                        &format!("{comment} -set init capacity value to {lvalue_location}"),
                    );
                    self.builder.add_st16_using_ptr_with_offset(
                        &lvalue_location.unsafe_add_offset(COLLECTION_CAPACITY_OFFSET),
                        init_capacity_reg.register(),
                        node,
                        &format!("{comment} - store capacity"),
                    );

                    // if it is a fixed static array, we have to set the element_count
                    // it never changes at runtime
                    if lvalue_location.ty.element_count_always_same_as_capacity() {
                        self.builder.add_st16_using_ptr_with_offset(
                            &lvalue_location.unsafe_add_offset(COLLECTION_ELEMENT_COUNT_OFFSET),
                            init_capacity_reg.register(),
                            node,
                            &format!("{comment} - store element_count to same as capacity"),
                        );
                    }

                    self.temp_registers.restore_to_mark(hwm);
                } else {
                    // if there is no collection capacity
                }
            }
        }
    }
}
