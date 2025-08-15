/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::DetailedLocationResolved;
use crate::code_bld::CodeBuilder;
use source_map_node::Node;
use swamp_vm_isa::COLLECTION_CAPACITY_OFFSET;
use swamp_vm_types::MemoryLocation;
use swamp_vm_types::types::{Place, TypedRegister, VmType, u16_type};

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
        source: &Place,
        node: &Node,
        comment: &str,
    ) {
        match source {
            Place::Register(source_reg) => {
                if target_reg.index != source_reg.index {
                    self.emit_copy_register(target_reg, source_reg, node, comment);
                }
            }
            Place::Memory(memory_location) => {
                self.emit_load_value_from_memory_source(target_reg, memory_location, node, comment);
            }
            Place::Discard => panic!("Cannot load from Unit destination"),
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
        if !source_type.is_reg_copy() {
            // We want the pointer since it is an aggregate. So either just copy the register
            // or flatten the pointer
            let source_loc = Place::Memory(source_memory_location.clone());

            self.emit_compute_effective_address_to_target_register(
                target_reg,
                &source_loc,
                node,
                "copy aggregate pointer to target register",
            );
        } else {
            self.emit_load_scalar_from_memory_offset_instruction(
                target_reg,
                source_memory_location,
                node,
                &format!("emit primitive value. ptr to primitive reg {comment}"),
            );
        }
    }

    /// Loads a scalar value or computes the absolute pointer for an aggregate type.
    ///
    /// This function handles the two main cases for intrinsic function arguments:
    ///
    /// - **Scalar values**: Ensures the actual primitive value is loaded into a register
    /// - **Aggregate types**: Computes and returns the absolute memory address pointer
    ///
    /// This is specifically designed for intrinsic calls where:
    /// - Scalar intrinsics need the actual value (not a pointer to the value)
    /// - Aggregate intrinsics need a flattened absolute pointer for operations
    ///
    /// # Arguments
    /// * `destination` - The source location (register, memory, or unit)
    /// * `node` - Source location for debugging
    /// * `comment` - Description for generated assembly comments
    ///
    /// # Returns
    /// * `Some(TypedRegister)` - Register containing the scalar value or aggregate pointer
    /// * `None` - If the destination is Unit
    pub(crate) fn emit_load_scalar_or_absolute_aggregate_pointer(
        &mut self,
        destination: &Place,
        node: &Node,
        comment: &str,
    ) -> Option<TypedRegister> {
        if matches!(destination, Place::Discard) {
            None
        } else {
            let vm_type = destination.vm_type().unwrap();

            if vm_type.is_scalar() {
                // Scalar case
                match destination {
                    Place::Register(reg) => {
                        // a) if it is a register, we are done: use that register
                        Some(reg.clone())
                    }
                    Place::Memory(memory_location) => {
                        // b) if it is a memory location: load_scalar_from_memory_location
                        let scalar_temp = self.temp_registers.allocate(
                            memory_location.ty.clone(),
                            &format!("load scalar from memory for intrinsic: {comment}"),
                        );

                        self.emit_load_scalar_from_memory_offset_instruction(
                            scalar_temp.register(),
                            memory_location,
                            node,
                            &format!("load scalar value from memory for intrinsic: {comment}"),
                        );

                        Some(scalar_temp.register)
                    }
                    Place::Discard => unreachable!(), // Already handled above
                }
            } else {
                // Aggregate case: we only need to flatten it if needed
                Some(self.emit_compute_effective_address_to_register(
                    destination,
                    node,
                    &format!("flatten aggregate for intrinsic: {comment}"),
                ))
            }
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
                &Place::Memory(source_memory_location.clone()),
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
        location: &Place,
        node: &Node,
        comment: &str,
    ) -> DetailedLocationResolved {
        match location {
            Place::Register(reg) => DetailedLocationResolved::Register(reg.clone()),
            Place::Memory(memory_location) => {
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
            Place::Discard => {
                panic!("")
            }
        }
    }

    // Store -------------------------------------------------------

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
        node: &Node,
        comment: &str,
    ) {
        let destination_pointer = self.emit_compute_effective_address_from_location_to_register(
            destination_memory_location,
            node,
            "get the destination vec",
        );
        let source_pointer = self.emit_compute_effective_address_from_location_to_register(
            source_memory_location,
            node,
            "get vector source address",
        );

        self.builder.add_vec_copy(
            &destination_pointer,
            &source_pointer,
            node,
            "copy over, but leave the capacity on the destination",
        );
    }

    /// Copies the data for a map-like collection using open addressing.
    ///
    /// This implementation safely handles maps with different capacities by:
    /// 1. Verifying that the source `element_count` doesn't exceed target capacity
    /// 2. Re-inserting each element into the target map individually
    ///
    /// Rather than performing a direct bucket array copy (which would be incorrect when
    /// capacities differ), this approach ensures each element is inserted at the correct
    /// index in the target map. Since element positions are determined by `hash % capacity`,
    /// this recalculation is essential when source and target capacities differ.
    ///
    /// Performance note: When source and target have identical capacities, a more
    /// efficient direct copy could be used, but this implementation prioritizes
    /// correctness across all scenarios.
    pub(crate) fn emit_copy_map_like_value_helper(
        &mut self,
        destination_memory_location: &MemoryLocation,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let destination_ptr_location = self
            .emit_compute_effective_address_from_location_to_register(
                destination_memory_location,
                node,
                comment,
            );
        let source_ptr_location = self.emit_compute_effective_address_from_location_to_register(
            source_memory_location,
            node,
            comment,
        );
        self.builder.add_map_overwrite(
            &destination_ptr_location,
            &source_ptr_location,
            node,
            comment,
        );
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
            if ty.basic_type.is_vec_like() {
                if let (Some(_element_size), Some(_header_size)) = (
                    source_memory_location
                        .ty
                        .basic_type
                        .bucket_size_for_vec_like(),
                    source_memory_location
                        .ty
                        .basic_type
                        .header_size_for_vec_like(),
                ) {
                    self.emit_copy_vec_like_value_helper(
                        destination_memory_location,
                        source_memory_location,
                        node,
                        comment,
                    );
                } else {
                    // Fallback to block copy for vec-like types that don't have proper size methods
                    self.emit_block_copy_with_size_from_location(
                        destination_memory_location,
                        source_memory_location,
                        node,
                        &format!("block copy {comment} (vec-like fallback) to memory pointed by register {destination_memory_location} <- {source_memory_location}"),
                    );
                }
            } else {
                self.emit_compute_effective_address_from_location_to_register(
                    destination_memory_location,
                    node,
                    comment,
                );
                self.emit_copy_map_like_value_helper(
                    destination_memory_location,
                    source_memory_location,
                    node,
                    comment,
                );
            }
        } else {
            self.emit_block_copy_with_size_from_location(
                destination_memory_location,
                source_memory_location,
                node,
                &format!("block copy {comment} to memory pointed by register {destination_memory_location} <- {source_memory_location}"),
            );
        }
    }

    pub fn emit_copy_value_between_destinations(
        &mut self,
        output_destination: &Place,
        value_source: &Place,
        node: &Node,
        comment: &str,
    ) {
        match output_destination {
            Place::Register(reg) => {
                self.emit_transfer_value_to_register(reg, value_source, node, comment);
            }
            Place::Memory(_) => {
                self.emit_store_value_to_memory_destination(
                    output_destination,
                    value_source,
                    node,
                    comment,
                );
            }
            Place::Discard => {
                panic!("Cannot copy to Unit destination")
            }
        }
    }

    pub(crate) fn emit_copy_value_from_memory_location(
        &mut self,
        destination: &Place,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        if let Some(_mem_loc) = destination.memory_location() {
            let source_loc = Place::Memory(source_memory_location.clone());
            self.emit_store_value_to_memory_destination(destination, &source_loc, node, comment);
        } else if let Some(output_target_reg) = destination.register() {
            self.emit_load_value_from_memory_source(
                output_target_reg,
                source_memory_location,
                node,
                comment,
            );
        } else {
            panic!("it was unit, not supported");
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
        output_destination: &Place,
        value_source: &Place,
        node: &Node,
        comment: &str,
    ) {
        let output_mem_loc = output_destination.grab_memory_location(); // Assuming this is always a MemoryLocation

        match value_source {
            Place::Register(value_reg) => {
                if value_reg.ty.is_reg_copy() {
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
                        output_destination.grab_memory_location(),
                        &source_memory_location,
                        node,
                        "copy aggregate",
                    );
                }
            }
            Place::Memory(source_mem_loc) => {
                let temp_reg = self
                    .temp_registers
                    .allocate(source_mem_loc.ty.clone(), "temp_for_memory_to_memory_store");

                self.emit_load_value_from_memory_source(
                    temp_reg.register(),
                    source_mem_loc,
                    node,
                    &format!("load {comment} from memory for store"),
                );

                if source_mem_loc.ty.is_reg_copy() {
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
            Place::Discard => panic!("Cannot store from Unit source"),
        }
    }
}
