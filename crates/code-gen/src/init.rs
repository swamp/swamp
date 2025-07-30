/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use source_map_node::Node;
use swamp_vm_types::types::{u16_type, u32_type, BasicTypeKind, BasicTypeRef, VmType};
use swamp_vm_types::{AggregateMemoryLocation, CountU16, MemoryLocation};

impl CodeBuilder<'_> {
    /// Helper function to initialize collections in any type (struct, tuple, optional, etc.)
    pub(crate) fn emit_initialize_collections_in_type(
        &mut self,
        location: &AggregateMemoryLocation,
        type_ref: &BasicTypeRef,
        node: &Node,
        comment: &str,
    ) {
        match &type_ref.kind {
            // Direct collection types that need initialization
            // TODO: Should be in a helper
            BasicTypeKind::MapStorage { .. }
            | BasicTypeKind::StringStorage { .. }
            | BasicTypeKind::VecStorage(..)
            | BasicTypeKind::GridStorage(..)
            | BasicTypeKind::SparseStorage(..)
            | BasicTypeKind::StackStorage(..)
            | BasicTypeKind::QueueStorage(..)
            | BasicTypeKind::FixedCapacityArray(..) => {
                // Initialize the collection
                self.emit_initialize_collection_metadata(
                    &location.location,
                    node,
                    &format!("{comment} - emit_initialize_collections_in_type"),
                );
            }

            // Container types that might contain collections
            BasicTypeKind::Struct(_) => {
                self.initialize_struct_collection_fields(location, type_ref, node, comment);
            }

            BasicTypeKind::Tuple(tuple_info) => {
                for (i, tuple_field) in tuple_info.fields.iter().enumerate() {
                    let tuple_field_location =
                        location.offset(tuple_field.offset, tuple_field.ty.clone());
                    self.emit_initialize_collections_in_type(
                        &tuple_field_location,
                        &tuple_field.ty,
                        node,
                        &format!("{comment} - emit_initialize_collections_in_type tuple field {i}"),
                    );
                }
            }

            // For tagged unions (including Optional types)
            BasicTypeKind::TaggedUnion(union_info) => {
                // We can't initialize collections inside unions at struct creation time
                // because we don't know which variant will be active
            }

            _ => {}
        }
    }

    /// Helper function to recursively initialize collection fields in a struct
    fn initialize_struct_collection_fields(
        &mut self,
        struct_location: &AggregateMemoryLocation,
        struct_type: &BasicTypeRef,
        node: &Node,
        comment: &str,
    ) {
        match &struct_type.kind {
            BasicTypeKind::Struct(struct_info) => {
                // Iterate through all fields in the struct
                for field in &struct_info.fields {
                    // Calculate the field's memory location
                    let field_location = struct_location.offset(field.offset, field.ty.clone());

                    // Recursively initialize any collections in this field
                    self.emit_initialize_collections_in_type(
                        &field_location,
                        &field.ty,
                        node,
                        &format!("{comment} - field {}", field.name),
                    );
                }
            }
            // Handle other container types that might contain collections
            BasicTypeKind::Tuple(tuple_info) => {
                for (i, tuple_field) in tuple_info.fields.iter().enumerate() {
                    let tuple_field_location =
                        struct_location.offset(tuple_field.offset, tuple_field.ty.clone());
                    self.emit_initialize_collections_in_type(
                        &tuple_field_location,
                        &tuple_field.ty,
                        node,
                        &format!("{comment} - tuple field {i}"),
                    );
                }
            }
            BasicTypeKind::TaggedUnion(union_info) => {
                // For tagged unions (including Optional types), we can't initialize collections
                // since we don't know which variant is active yet
            }
            _ => {}
        }
    }

    /// Initialize memory for any type that might contain collections
    ///
    /// This is the main public interface for memory initialization. It handles:
    /// - Direct collections: Initializes the collection metadata (capacity, etc.)
    /// - Container types: Recursively initializes any collections they contain
    /// - Scalar types: Does nothing (no initialization needed)
    ///
    /// Use this function whenever you need to ensure that memory is properly initialized
    /// for any type that might contain collections, either directly or nested.
    pub fn emit_initialize_memory_for_any_type(
        &mut self,
        memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let basic_type = memory_location.ty.basic_type();

        // Handle direct collections
        if basic_type.is_collection() {
            self.emit_initialize_collection_metadata(memory_location, node, comment);
            return;
        }

        // Handle container types that might contain collections
        let aggregate_location = AggregateMemoryLocation {
            location: memory_location.clone(),
        };

        self.emit_initialize_collections_in_type(&aggregate_location, basic_type, node, comment);
    }

    /// Initialize memory for a collection type at the specified location
    ///
    /// This function is responsible for initializing the metadata for collection types:
    /// - For collections (Vec, Map, etc.), it sets the capacity field based on the type definition
    /// - Does NOT handle nested collections - use `emit_initialize_collections_in_type` for that
    ///
    /// This is a specialized function for direct collection initialization only.
    pub fn emit_initialize_collection_metadata(
        &mut self,
        memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let basic_type = memory_location.ty.basic_type();
        if !basic_type.is_collection() {
            return; // Not a collection, nothing to initialize
        }

        match &basic_type.kind {
            BasicTypeKind::FixedCapacityArray(element_type, capacity) => {
                let absolute_pointer = self
                    .emit_compute_effective_address_from_location_to_register(
                        memory_location,
                        node,
                        &format!("{comment} - compute address for vec init when pointer_location() is None"),
                    );

                self.builder
                    .add_fixed_capacity_array_init_fill_capacity_and_len(
                        &absolute_pointer,
                        *capacity as u16,
                        &element_type.total_size,
                        node,
                        &format!("{comment} - initialize fixed capacity array init"),
                    );
            }
            BasicTypeKind::SparseStorage(element_type, capacity) => {
                let absolute_pointer = self
                    .emit_compute_effective_address_from_location_to_register(
                        memory_location,
                        node,
                        &format!("{comment} - load effective address for sparse"),
                    );
                self.builder.add_sparse_init(
                    &absolute_pointer,
                    element_type.total_size,
                    *capacity as u16,
                    node,
                    comment,
                );
            }
            BasicTypeKind::GridStorage(element_type, width, height) => {
                let hwm = self.temp_registers.save_mark();

                let absolute_pointer = self
                    .emit_compute_effective_address_from_location_to_register(
                        memory_location,
                        node,
                        "compute address for grid init when pointer_location() is None",
                    );

                let init_element_size = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u16_type()),
                    &format!("{comment} - init grid init_element_size"),
                );

                self.builder.add_mov_32_immediate_value(
                    init_element_size.register(),
                    element_type.total_size.0,
                    node,
                    &format!("{comment} -set grid element size {memory_location}"),
                );

                self.builder.add_grid_init(
                    &absolute_pointer.ptr_reg,
                    &init_element_size.register,
                    *width as u16,
                    *height as u16,
                    node,
                    comment,
                );

                self.temp_registers.restore_to_mark(hwm);
            }
            BasicTypeKind::MapStorage {
                logical_limit,
                key_type,
                value_type,
                ..
            } => {
                let hwm = self.temp_registers.save_mark();

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
                    &format!("{comment} -set init key_size value to {memory_location}"),
                );

                let init_value_size = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u32_type()),
                    &format!("{comment} - init map value_size reg"),
                );
                self.builder.add_mov_32_immediate_value(
                    init_value_size.register(),
                    unaligned_value_size.0,
                    node,
                    &format!("{comment} -set init value_size value to {memory_location}"),
                );

                let map_pointer_location = self
                    .emit_compute_effective_address_from_location_to_register(
                        memory_location,
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
                    &format!(
                        "{comment} - initialize map (capacity, key_size, total_key_and_value_size)"
                    ),
                );

                self.temp_registers.restore_to_mark(hwm);
            }
            BasicTypeKind::VecStorage(element_type, capacity)
            | BasicTypeKind::StackStorage(element_type, capacity)
            | BasicTypeKind::StringStorage {
                element_type,
                capacity,
                ..
            }
            | BasicTypeKind::QueueStorage(element_type, capacity) => {
                let absolute_pointer = self
                    .emit_compute_effective_address_from_location_to_register(
                        memory_location,
                        node,
                        "compute address for vec init when pointer_location() is None",
                    );
                self.builder.add_vec_init_set_capacity(
                    &absolute_pointer,
                    CountU16(*capacity as u16),
                    &element_type.total_size,
                    node,
                    "initialize vec like (vec, stack, queue)",
                );
            }

            _ => {}
        }
    }
}
