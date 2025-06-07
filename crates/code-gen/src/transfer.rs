/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::DetailedLocationResolved;
use crate::code_bld::CodeBuilder;
use source_map_node::Node;
use swamp_vm_types::types::{Destination, TypedRegister, VmType, u16_type};
use swamp_vm_types::{
    COLLECTION_CAPACITY_OFFSET, COLLECTION_ELEMENT_COUNT_OFFSET, MemoryLocation, MemoryOffset,
    MemorySize,
};
use tracing::error;

impl CodeBuilder<'_> {
    // Load -------------------------------------------------------

    /// Transfers a **value** from a given `Destination` (either a register or a memory location)
    /// into a specified `target_reg`.
    ///
    /// This function acts as a primary entry point for ensuring a value resides in a register.
    /// If the `source` is a `Register`, it generates a register-to-register copy if the
    /// source and target are not already the same. If the `source` is `Memory`, it delegates
    /// the load operation to `emit_load_value_from_memory_source`.
    pub(crate) fn emit_transfer_value_to_register(
        &mut self,
        target_reg: &TypedRegister,
        source: &Destination,
        node: &Node,
        comment: &str,
    ) {
        match source {
            Destination::Register(source_reg) => {
                if target_reg.index != source_reg.index {
                    self.emit_copy_register(target_reg, source_reg, node, comment);
                }
            }
            Destination::Memory(memory_location) => {
                self.emit_load_value_from_memory_source(target_reg, memory_location, node, comment);
            }
            Destination::Unit => panic!("Cannot load from Unit destination"),
        }
    }

    /// Loads a **value** from a `MemoryLocation` into a `target_reg`.
    ///
    /// This function serves as a helper for `emit_transfer_value_to_register` when the
    /// source is memory. It distinguishes between loading scalar values and
    /// loading pointer-like values (e.g., references) that are themselves stored within
    /// registers. For scalar loads, it utilizes `emit_load_scalar_from_memory_offset_instruction`.
    pub(crate) fn emit_load_value_from_memory_source(
        &mut self,
        target_reg: &TypedRegister,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let source_type = source_memory_location.vm_type();
        if source_type.is_aggregate() {
            if target_reg.ty().is_mutable_reference() {
                self.emit_load_scalar_from_memory_offset_instruction(
                    target_reg,
                    source_memory_location,
                    node,
                    &format!("emit_load_from_memory: ptr to ptr (mutable reference). {comment}"),
                );
            } else {
                if source_memory_location.as_direct_register().is_none() {
                    error!(
                        ?target_reg,
                        ?source_memory_location,
                        "emit_load_value_from_memory_source failed for aggregate that is not a mutable reference"
                    );
                }
                self.builder.add_mov_reg(
                    target_reg,
                    source_memory_location.as_direct_register().unwrap(),
                    node,
                    "emit_load_from_memory: copy pointer reg to reg",
                );
            }
        } else {
            self.emit_load_scalar_from_memory_offset_instruction(
                target_reg,
                source_memory_location,
                node,
                &format!("emit primitive value. ptr to primitive reg {comment}"),
            );
        }
    }

    /// Loads a scalar value or calculates the effective address for an aggregate type.
    ///
    /// It is basically a "bind" of a register to a location, and is used (exclusively?)
    /// in "pattern matching", like `match` arms.
    ///
    /// For scalar types: Loads the actual value into the target register
    /// For aggregate types: Calculates and stores the effective address in the target register
    pub(crate) fn emit_load_or_calculate_address_from_memory(
        &mut self,
        target_reg: &TypedRegister,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let source_type = source_memory_location.vm_type();
        if source_type.is_aggregate() {
            self.emit_compute_effective_address_to_target_register(
                target_reg,
                &Destination::Memory(source_memory_location.clone()),
                node,
                comment,
            );
        } else {
            // For scalars, load the actual value
            self.emit_load_scalar_from_memory_offset_instruction(
                target_reg,
                source_memory_location,
                node,
                &format!("load scalar value {comment}"),
            );
        }
    }

    /// Ensures that a value from a given `Destination` is available in a register.
    ///
    /// In compiler design, "materialization" refers to the process of bringing a value from an
    /// abstract or indirect representation (like a variable name, or a value stored in memory)
    /// into a concrete, directly usable form, which is typically a CPU register.
    ///
    /// If the `location` is already a `Register`, it's returned directly. If the `location`
    /// is `Memory`, a temporary register is allocated, and the primitive value is loaded
    /// into it using `emit_load_value_from_memory_source`, ensuring it's ready for
    /// register-based operations.
    pub(crate) fn emit_materialize_value_to_register(
        &mut self,
        location: &Destination,
        node: &Node,
        comment: &str,
    ) -> DetailedLocationResolved {
        match location {
            Destination::Register(reg) => DetailedLocationResolved::Register(reg.clone()),
            Destination::Memory(memory_location) => {
                let temp_reg_target = self.temp_registers.allocate(
                    memory_location.ty.clone(),
                    "emit load primitive from location",
                );
                self.emit_load_value_from_memory_source(
                    temp_reg_target.register(),
                    memory_location,
                    node,
                    &format!("load primitive from detailed location {comment}"),
                );
                DetailedLocationResolved::TempRegister(temp_reg_target)
            }
            Destination::Unit => {
                panic!("")
            }
        }
    }

    // Store -------------------------------------------------------
    pub fn emit_check_that_source_capacity_is_less_or_equal_to_target_capacity(
        &mut self,
        destination_memory_location: &MemoryLocation,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        // First check if the destination capacity is greater or equal to the source location so we don't overwrite too much
        // Then always preserve the capacity. We should fill in the elements count and other things, but never overwrite capacity.

        let destination_capacity_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "destination capacity",
        );
        self.builder.add_ld16_from_pointer_from_memory_location(
            destination_capacity_reg.register(),
            &destination_memory_location.unsafe_add_offset(COLLECTION_CAPACITY_OFFSET),
            node,
            &format!("{comment} - load capacity for destination"),
        );

        let source_length_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "source capacity",
        );
        self.builder.add_ld16_from_pointer_from_memory_location(
            source_length_reg.register(),
            &source_memory_location.unsafe_add_offset(COLLECTION_CAPACITY_OFFSET),
            node,
            &format!("{comment} - load capacity for source"),
        );

        self.builder.add_trap_if_lt(
            destination_capacity_reg.register(),
            source_length_reg.register(),
            node,
            &format!("{comment} - verify that we are within bounds"),
        );

        source_length_reg.register
    }

    pub fn emit_check_that_element_count_is_less_or_equal_to_capacity(
        &mut self,
        destination_memory_location: &MemoryLocation,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        // First check if the destination capacity is greater or equal to the source location so we don't overwrite too much
        // Then always preserve the capacity. We should fill in the elements count and other things, but never overwrite capacity.

        let destination_capacity_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "destination capacity",
        );
        self.builder.add_ld16_from_pointer_from_memory_location(
            destination_capacity_reg.register(),
            &destination_memory_location.unsafe_add_offset(COLLECTION_CAPACITY_OFFSET),
            node,
            &format!("{comment} - load capacity for destination"),
        );

        let source_length_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "source capacity",
        );
        self.builder.add_ld16_from_pointer_from_memory_location(
            source_length_reg.register(),
            &source_memory_location.unsafe_add_offset(COLLECTION_ELEMENT_COUNT_OFFSET),
            node,
            &format!("{comment} - load source element_count"),
        );

        self.builder.add_trap_if_lt(
            destination_capacity_reg.register(),
            source_length_reg.register(),
            node,
            &format!("{comment} - verify that we are within bounds"),
        );

        source_length_reg.register
    }

    pub fn emit_check_that_known_len_is_less_or_equal_to_capacity(
        &mut self,
        destination_memory_location: &MemoryLocation,
        len: usize,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        // First check if the destination capacity is greater or equal to the source location so we don't overwrite too much
        // Then always preserve the capacity. We should fill in the elements count and other things, but never overwrite capacity.

        let destination_capacity_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "destination capacity",
        );
        self.builder.add_ld16_from_pointer_from_memory_location(
            destination_capacity_reg.register(),
            &destination_memory_location.unsafe_add_offset(COLLECTION_CAPACITY_OFFSET),
            node,
            &format!("{comment} - load capacity for destination"),
        );

        let source_length_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "source capacity",
        );

        self.builder.add_mov_16_immediate_value(
            source_length_reg.register(),
            len as u16,
            node,
            "known length size",
        );

        self.builder.add_trap_if_lt(
            destination_capacity_reg.register(),
            source_length_reg.register(),
            node,
            &format!("{comment} - verify that we are within bounds"),
        );

        source_length_reg.register
    }
    pub(crate) fn emit_copy_vec_like_value_helper(
        &mut self,
        destination_memory_location: &MemoryLocation,
        source_memory_location: &MemoryLocation,
        bucket_size: MemorySize,
        collection_header_size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let hwm = self.temp_registers.save_mark();
        let source_length_reg = self.emit_check_that_element_count_is_less_or_equal_to_capacity(
            destination_memory_location,
            source_memory_location,
            node,
            &format!("{comment} - check `destination capacity` >= `source element_count`"),
        );
        let skip_capacity = MemoryOffset(2);
        let destination_tail = destination_memory_location.unsafe_add_offset(skip_capacity);
        let source_tail = source_memory_location.unsafe_add_offset(skip_capacity);

        // Compute bytes = (header_bytes - 2) + (element_count * element_size)
        let src_count_in_bytes_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "calculate byte length",
        );
        let element_size_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "element_size",
        );
        self.builder.add_mov_16_immediate_value(
            element_size_reg.register(),
            bucket_size.0,
            node,
            "load in element size",
        );

        self.builder.add_mul_i32(
            src_count_in_bytes_reg.register(),
            &source_length_reg,
            element_size_reg.register(),
            node,
            "element_count * element_size",
        );

        let header_tail_size = collection_header_size.0 - skip_capacity.0;
        self.builder.add_add_u32_imm(
            src_count_in_bytes_reg.register(),
            src_count_in_bytes_reg.register(),
            u32::from(header_tail_size),
            node,
            "(element_count*element_size) + collection header size",
        );

        self.builder.add_block_copy_with_offset_with_variable_size(
            &destination_tail,
            &source_tail,
            src_count_in_bytes_reg.register(),
            node,
            &format!("{comment} - copy whole collection except capacity"),
        );

        self.temp_registers.restore_to_mark(hwm);
    }

    // In the case of a Map like we must copy the whole capacity
    // since elements can be stored in any of the buckets
    pub(crate) fn emit_copy_map_like_value_helper(
        &mut self,
        destination_memory_location: &MemoryLocation,
        source_memory_location: &MemoryLocation,
        bucket_size: MemorySize,
        collection_header_size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let hwm = self.temp_registers.save_mark();
        let source_capacity_reg = self
            .emit_check_that_source_capacity_is_less_or_equal_to_target_capacity(
                destination_memory_location,
                source_memory_location,
                node,
                &format!("{comment} - check source capacity is less or equal than target capacity"),
            );
        let skip_capacity = MemoryOffset(2);

        // Compute bytes = (header_bytes - 2) + (source_capacity * bucket size)
        let src_count_in_bytes_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u16_type()),
            "calculate byte length",
        );
        let bucket_size_reg = self
            .temp_registers
            .allocate(VmType::new_contained_in_register(u16_type()), "bucket_size");
        self.builder.add_mov_16_immediate_value(
            bucket_size_reg.register(),
            bucket_size.0,
            node,
            "set bucket_size to reg",
        );

        self.builder.add_mul_i32(
            src_count_in_bytes_reg.register(),
            &source_capacity_reg,
            bucket_size_reg.register(),
            node,
            "count * bucket_size",
        );

        let header_tail_size = collection_header_size.0 - skip_capacity.0;
        self.builder.add_add_u32_imm(
            src_count_in_bytes_reg.register(),
            src_count_in_bytes_reg.register(),
            u32::from(header_tail_size),
            node,
            "(count*element_size) + collection header size",
        );

        let destination_tail = destination_memory_location.unsafe_add_offset(skip_capacity);
        let source_tail = source_memory_location.unsafe_add_offset(skip_capacity);

        self.builder.add_block_copy_with_offset_with_variable_size(
            &destination_tail,
            &source_tail,
            src_count_in_bytes_reg.register(),
            node,
            &format!("{comment} - copy whole buckets except capacity"),
        );

        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) fn emit_copy_aggregate_value_helper(
        &mut self,
        destination_memory_location: &MemoryLocation,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let ty = &source_memory_location.ty;
        if ty.is_collection_like() {
            let element_size = source_memory_location.ty.basic_type.bucket_size().unwrap();
            let header_size = source_memory_location.ty.basic_type.header_size().unwrap();
            if ty.basic_type.is_vec_like() {
                self.emit_copy_vec_like_value_helper(
                    destination_memory_location,
                    source_memory_location,
                    element_size,
                    header_size,
                    node,
                    comment,
                );
            } else {
                self.emit_copy_map_like_value_helper(
                    destination_memory_location,
                    source_memory_location,
                    element_size,
                    header_size,
                    node,
                    comment,
                );
            }
        } else {
            self.builder.add_block_copy_with_offset(
                destination_memory_location,
                source_memory_location,
                node,
                &format!("block copy {comment} to memory pointed by register {destination_memory_location} <- {source_memory_location}"),
            );
        }
    }

    /// Stores a **value** from a `value_source` (either a register or memory) to a
    /// target memory location specified by `output_destination`.
    ///
    /// This function handles storing both **scalar** and **aggregate** types. For scalars,
    /// it delegates to `emit_store_scalar_to_memory_offset_instruction`. For larger
    /// aggregate types (like structs, tagged unions or arrays), it performs a block copy.
    /// If the `value_source` is also memory, it first loads the value into a temporary
    /// register using `emit_load_value_from_memory_source` before storing.
    pub(crate) fn emit_store_value_to_memory_destination(
        &mut self,
        output_destination: &Destination,
        value_source: &Destination,
        node: &Node,
        comment: &str,
    ) {
        let output_mem_loc = output_destination.grab_memory_location(); // Assuming this is always a MemoryLocation

        match value_source {
            Destination::Register(value_reg) => {
                if value_reg.ty.is_scalar() {
                    self.emit_store_scalar_to_memory_offset_instruction(
                        output_mem_loc,
                        value_reg,
                        node,
                        &format!("store {comment} to memory pointed by register {output_destination} <- {value_reg}"),
                    );
                } else {
                    let source_memory_location =
                        MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                            value_reg.clone(),
                        );
                    self.emit_copy_aggregate_value_helper(
                        &output_destination.memory_location_or_pointer_reg(),
                        &source_memory_location,
                        node,
                        "copy aggregate",
                    );
                }
            }
            Destination::Memory(source_mem_loc) => {
                let temp_reg = self
                    .temp_registers
                    .allocate(source_mem_loc.ty.clone(), "temp_for_memory_to_memory_store");

                self.emit_load_value_from_memory_source(
                    temp_reg.register(),
                    source_mem_loc,
                    node,
                    &format!("load {comment} from memory for store"),
                );

                if source_mem_loc.ty.is_scalar() {
                    self.emit_store_scalar_to_memory_offset_instruction(
                        output_mem_loc,
                        temp_reg.register(),
                        node,
                        &format!("store {comment} from temp to memory pointed by register"),
                    );
                } else {
                    self.emit_copy_aggregate_value_helper(
                        output_destination.grab_memory_location(),
                        source_mem_loc,
                        node,
                        "copy aggregate",
                    );
                }
            }
            Destination::Unit => panic!("Cannot store from Unit source"),
        }
    }
}
