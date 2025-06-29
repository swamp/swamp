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
use swamp_vm_types::types::{BasicTypeKind, Destination, VmType, u16_type, u32_type};
use swamp_vm_types::{
    AggregateMemoryLocation, COLLECTION_CAPACITY_OFFSET, COLLECTION_ELEMENT_COUNT_OFFSET, CountU16,
    MemoryLocation,
};

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
        let struct_type = self.state.layout_cache.layout(struct_type_ref);

        let base_ptr_register = self.temp_registers.allocate(
            VmType::new_contained_in_register(u32_type()),
            "temporary so it isn't clobbered during filling in the structure",
        );
        let output_destination = Destination::Memory(lvalue_location.location.clone());

        self.emit_compute_effective_address_to_target_register(
            base_ptr_register.register(),
            &output_destination,
            node,
            "flatten the pointer before, to minimize offset",
        );
        let output_aggregate_location = AggregateMemoryLocation {
            location: MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                base_ptr_register.register,
            ),
        };

        /*
        let struct_type = BasicType {
            total_size: gen_source_struct_type.total_size,
            max_alignment: gen_source_struct_type.max_alignment,
            kind: BasicTypeKind::Struct(gen_source_struct_type.clone()),
        };

         */

        let BasicTypeKind::Struct(gen_source_struct_type) = &struct_type.kind else {
            panic!("must be named struct type or anon struct literal")
        };

        for (offset_item, (field_index, _node, source_expression)) in gen_source_struct_type
            .fields
            .iter()
            .zip(source_order_expressions)
        {
            let real_offset_item = struct_type.get_field_offset(*field_index).unwrap();
            let modified_lvalue_location = output_aggregate_location
                .offset(real_offset_item.offset, real_offset_item.ty.clone());

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

        // After we are done, let's copy back the register

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
            TypeKind::AnonymousStruct(anon_struct_type) => ty.clone(),
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

    /// Initialize memory for a given type at the specified location
    ///
    /// This function is responsible for initializing memory for various types:
    /// - For collections (Vec, Map, etc.), it sets the capacity field based on the type definition
    /// - For complex types like tuples and structs, it initializes the base structure
    ///
    /// Note: For nested collections (e.g., Vec inside tuple or struct), this function only
    /// initializes the outer container. The caller must explicitly initialize nested collections
    /// by calculating the appropriate offset and calling this function again for the inner type.
    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_initialize_target_memory_first_time(
        &mut self,
        lvalue_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        match &lvalue_location.ty.basic_type().kind {
            BasicTypeKind::SparseStorage(element_type, capacity) => {
                let absolute_pointer = self
                    .emit_compute_effective_address_from_location_to_register(
                        lvalue_location,
                        node,
                        "load effective address for sparse",
                    );
                self.builder.add_sparse_init(
                    &absolute_pointer.ptr_reg,
                    element_type.total_size,
                    *capacity as u16,
                    node,
                    comment,
                );
            }
            BasicTypeKind::GridStorage(element_type, width, height) => {
                let hwm = self.temp_registers.save_mark();

                let init_element_size = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u16_type()),
                    &format!("{comment} - init grid init_element_size"),
                );

                self.builder.add_mov_32_immediate_value(
                    init_element_size.register(),
                    element_type.total_size.0,
                    node,
                    &format!("{comment} -set grid element size {lvalue_location}"),
                );
                self.builder.add_grid_init(
                    &lvalue_location.pointer_location().unwrap().ptr_reg,
                    &init_element_size.register,
                    *width as u16,
                    *height as u16,
                    node,
                    comment,
                );

                self.temp_registers.restore_to_mark(hwm);
            }
            BasicTypeKind::MapStorage {
                capacity,
                logical_limit,
                key_type,
                value_type,
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

                let unaligned_key_size = key_type.total_size;
                let key_alignment = key_type.max_alignment;
                let unaligned_value_size = value_type.total_size;
                let value_alignment = value_type.max_alignment;

                let init_key_size = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u32_type()),
                    &format!("{comment} - init map key_size reg"),
                );
                self.builder.add_mov_32_immediate_value(
                    init_key_size.register(),
                    unaligned_key_size.0,
                    node,
                    &format!("{comment} -set init key_size value to {lvalue_location}"),
                );

                let init_value_size = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u32_type()),
                    &format!("{comment} - init map value_size reg"),
                );
                self.builder.add_mov_32_immediate_value(
                    init_value_size.register(),
                    unaligned_value_size.0,
                    node,
                    &format!("{comment} -set init value_size value to {lvalue_location}"),
                );

                let map_pointer_location = self
                    .emit_compute_effective_address_from_location_to_register(
                        lvalue_location,
                        node,
                        "find location of map to init",
                    );

                self.builder.add_map_init_set_capacity(
                    &map_pointer_location,
                    CountU16(*logical_limit as u16),
                    init_key_size.register(),
                    key_alignment,
                    init_value_size.register(),
                    value_alignment,
                    node,
                    "initialize map (capacity, key_size, total_key_and_value_size)",
                );

                self.temp_registers.restore_to_mark(hwm);
            }
            _ => {
                if let Some(capacity) = lvalue_location.ty.basic_type().get_collection_capacity() {
                    println!(
                        "DEBUG: emit_initialize_target_memory_first_time - Found collection capacity {} for type {:?}",
                        capacity.0,
                        lvalue_location.ty.basic_type().kind
                    );
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
                    println!(
                        "DEBUG: emit_initialize_target_memory_first_time - NO collection capacity found for type {:?}",
                        lvalue_location.ty.basic_type().kind
                    );
                    // if there is no collection capacity
                }
            }
        }
    }
}
