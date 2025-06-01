use crate::DetailedLocationResolved;
use crate::code_bld::CodeBuilder;
use source_map_node::Node;
use swamp_vm_types::types::{BasicTypeKind, Destination, TypedRegister, VmType};
use swamp_vm_types::{HeapMemoryAddress, MemoryLocation};

impl CodeBuilder<'_> {
    /// Loads a scalar from an absolute memory address
    /// Only used for constants
    pub(crate) fn emit_load_primitive_from_absolute_memory_address(
        &mut self,
        target_reg: &TypedRegister,
        source_offset: HeapMemoryAddress,
        type_at_offset: &VmType,
        node: &Node,
        comment: &str,
    ) {
        match type_at_offset.basic_type.kind {
            BasicTypeKind::Empty => {
                // No need to copy, it has zero size
            }
            BasicTypeKind::U8 | BasicTypeKind::B8 => {
                self.builder.add_ld8_from_absolute_memory_address(
                    target_reg,
                    &source_offset,
                    node,
                    &format!("{comment} - load u8 primitive from memory"),
                );
            }
            BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                self.builder.add_ld32_from_absolute_memory_address(
                    target_reg,
                    &source_offset,
                    node,
                    &format!("{comment} - load u32 primitive from memory"),
                );
            }
            _ => panic!("this is not a primitive {type_at_offset:?}"),
        }
    }

    pub(crate) fn emit_load_value_into_register(
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
                self.emit_load_from_memory_internal(target_reg, memory_location, node, comment);
            }
            Destination::Unit => panic!("Cannot load from Unit destination"),
        }
    }

    pub(crate) fn emit_store_to_pointer_target(
        &mut self,
        output_destination: &Destination,
        value_source: &Destination,
        node: &Node,
        comment: &str,
    ) {
        match value_source {
            Destination::Register(value_reg) => match value_reg.ty.basic_type.kind {
                BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                    self.builder.add_st32_using_ptr_with_offset(
                        output_destination.grab_memory_location(),
                        value_reg,
                        node,
                        &format!("store {comment} to memory pointed by register {output_destination} <- {value_reg}"),
                    );
                }
                BasicTypeKind::U8 | BasicTypeKind::B8 => {
                    self.builder.add_st8_using_ptr_with_offset(
                        output_destination.grab_memory_location(),
                        value_reg,
                        node,
                        &format!("store byte {comment} to memory pointed by register {output_destination} <- {value_reg}"),
                    );
                }
                _ => {
                    let source_memory_location =
                        MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                            value_reg.clone(),
                        );
                    self.builder.add_block_copy_with_offset(
                        output_destination.grab_memory_location(),
                        &source_memory_location,
                        node,
                        &format!("block copy {comment} to memory pointed by register {output_destination} <- {value_reg}"),
                    );
                }
            },
            Destination::Memory(source_mem_loc) => {
                let temp_reg = self
                    .temp_registers
                    .allocate(source_mem_loc.ty.clone(), "temp_for_memory_to_memory_store");

                self.emit_load_from_memory_internal(
                    temp_reg.register(),
                    source_mem_loc,
                    node,
                    &format!("load {comment} from memory for store"),
                );

                match source_mem_loc.ty.basic_type.kind {
                    BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                        self.builder.add_st32_using_ptr_with_offset(
                            output_destination.grab_memory_location(),
                            temp_reg.register(),
                            node,
                            &format!("store {comment} from temp to memory pointed by register"),
                        );
                    }
                    _ => {
                        let source_memory_location =
                            MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                                temp_reg.register,
                            );
                        self.builder.add_block_copy_with_offset(
                            output_destination.grab_memory_location(),
                            &source_memory_location,
                            node,
                            &format!(
                                "block copy '{comment}' from temp to memory pointed by register"
                            ),
                        );
                    }
                }
            }
            Destination::Unit => panic!("Cannot store from Unit source"),
        }
    }

    /*
    pub(crate) fn emit_store_value_from_register(
        &mut self,
        target_mem_loc: &MemoryLocation,
        source_reg: &TypedRegister, // This register *holds the value to be stored*
        node: &Node,
        comment: &str,
    ) {
        let value_ty = &source_reg.ty;
        if value_ty.is_represented_as_pointer_inside_register() {
            // Complex type (like a struct or VecStorage) represented as a pointer in the register.
            // Need to block copy the data it points to.
            // This is the `blkcpy` case for struct assignment.
            self.builder.add_block_copy_with_offset(
                target_mem_loc,                 // Destination MemoryLocation
                source_reg,                     // Source Register (holds pointer to data)
                MemoryOffset(0), // Offset from source_reg is 0 (it points to the start of the data)
                value_ty.basic_type.total_size, // Size of the data to copy
                node,
                &format!("block copy {comment} from register {source_reg} to memory"),
            );
        } else {
            self.emit_store_scalar_to_memory_location_helper(target_mem_loc, source_reg, node, comment)
        }
    }


     */

    pub(crate) fn emit_load_from_memory_internal(
        &mut self,
        target_reg: &TypedRegister,
        source_memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let source_type = source_memory_location.vm_type();
        if source_type.is_represented_as_pointer_inside_register() {
            if target_reg.ty().is_mutable_reference() {
                self.emit_load_scalar_from_memory_location_helper(
                    target_reg,
                    source_memory_location,
                    node,
                    &format!("emit_load_from_memory: ptr to ptr (mutable reference). {comment}"),
                );
            } else {
                self.builder.add_mov_reg(
                    target_reg,
                    source_memory_location.as_direct_register().unwrap(),
                    node,
                    "emit_load_from_memory: copy pointer reg to reg",
                );

                /*
                let size = target_reg.size();
                self.builder.add_block_copy_with_offset(
                    target_reg,
                    MemoryOffset(0),
                    base_ptr_reg,
                    source_offset,
                    size,
                    node,
                    &format!("block copy {comment}"),
                );

                 */
            }
        } else {
            self.emit_load_scalar_from_memory_location_helper(
                target_reg,
                source_memory_location,
                node,
                &format!("emit primitive value. ptr to primitive reg {comment}"),
            );
        }
    }

    pub(crate) fn emit_load_primitive_from_detailed_location_if_needed(
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
                self.emit_load_from_memory_internal(
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

    /// Loads a scalar from Memory Location
    ///
    /// This is a helper and usually not called directly
    pub(crate) fn emit_load_scalar_from_memory_location_helper(
        &mut self,
        target: &TypedRegister,
        source_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        // Choose the appropriate load instruction based on the target register's type
        match target.underlying().kind {
            BasicTypeKind::Fixed32
            | BasicTypeKind::U32
            | BasicTypeKind::S32
            | BasicTypeKind::InternalStringPointer => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    target,
                    &source_location.base_ptr_reg,
                    source_location.offset,
                    node,
                    &format!("{comment} (load int)"),
                );
            }
            BasicTypeKind::B8 | BasicTypeKind::U8 => {
                self.builder.add_ld8_from_pointer_with_offset_u16(
                    target,
                    &source_location.base_ptr_reg,
                    source_location.offset,
                    node,
                    &format!("{comment} (load bool)"),
                );
            }
            _ => panic!(
                "Unsupported primitive type in add_load_primitive: {:?}",
                target.ty
            ),
        }
    }

    /// Stores a scalar to a memory location
    ///
    /// This is a helper and usually not called directly
    pub(crate) fn emit_store_scalar_to_memory_location_helper(
        &mut self,
        memory_location: &MemoryLocation,
        primitive_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        match primitive_reg.ty.basic_type.kind {
            BasicTypeKind::B8 | BasicTypeKind::U8 => {
                self.builder.add_st8_using_ptr_with_offset(
                    memory_location,
                    primitive_reg,
                    node,
                    comment,
                );
            }
            BasicTypeKind::U32
            | BasicTypeKind::S32
            | BasicTypeKind::Fixed32
            | BasicTypeKind::InternalStringPointer => {
                self.builder.add_st32_using_ptr_with_offset(
                    memory_location,
                    primitive_reg,
                    node,
                    comment,
                );
            }

            _ => panic!("must be primitive"),
        }
    }

    pub(crate) fn emit_ptr_reg_from_detailed_location(
        &mut self,
        location: &Destination,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        match location {
            Destination::Register(reg) => reg.clone(),
            Destination::Memory(memory_location) => {
                //let hwm = self.temp_registers.save_mark();

                if memory_location.offset.0 == 0 {
                    memory_location.base_ptr_reg.clone()
                } else {
                    let final_ptr_target_reg = self.temp_registers.allocate(
                        memory_location.ty.clone(),
                        &format!("{comment} - final_ptr_target_reg"),
                    );

                    self.builder.add_add_u32_imm(
                        final_ptr_target_reg.register(),
                        &memory_location.base_ptr_reg,
                        u32::from(memory_location.offset.0),
                        node,
                        &format!("{comment} (add to resolved new base_ptr)"),
                    );
                    final_ptr_target_reg.register().clone()
                }
            }
            Destination::Unit => {
                panic!("not sure")
            }
        }
    }
}
