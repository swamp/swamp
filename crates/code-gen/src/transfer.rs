use crate::DetailedLocationResolved;
use crate::code_bld::CodeBuilder;
use source_map_node::Node;
use swamp_vm_types::MemoryLocation;
use swamp_vm_types::types::{Destination, TypedRegister};
use tracing::{error, info};

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
            // For aggregates, calculate the effective address using our existing helper
            let effective_addr = self.emit_compute_effective_address_to_register(
                &Destination::Memory(source_memory_location.clone()),
                node,
                &format!("calculate address for aggregate {comment}"),
            );
            // Copy the calculated address to our target register
            self.builder.add_mov_reg(
                target_reg,
                &effective_addr,
                node,
                "copy calculated address to target",
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
                    // This implies value_reg holds an aggregate type (represented by a pointer)
                    // Existing block copy for aggregate types
                    let source_memory_location =
                        MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                            value_reg.clone(),
                        );
                    self.builder.add_block_copy_with_offset(
                        output_mem_loc,
                        &source_memory_location,
                        node,
                        &format!("block copy {comment} to memory pointed by register {output_destination} <- {value_reg}"),
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
                    // This implies temp_reg holds an aggregate type (represented by a pointer)
                    let source_memory_location =
                        MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                            temp_reg.register,
                        );

                    self.builder.add_block_copy_with_offset(
                        output_mem_loc,
                        &source_memory_location,
                        node,
                        &format!("block copy '{comment}' from temp to memory pointed by register"),
                    );
                }
            }
            Destination::Unit => panic!("Cannot store from Unit source"),
        }
    }
}
