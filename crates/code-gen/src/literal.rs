/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use std::mem::size_of;
use swamp_vm_isa::{HeapMemoryAddress, VEC_HEADER_MAGIC_CODE, VecHeader};
use swamp_vm_types::types::{BasicTypeKind, Destination, VmType, string_type};

impl CodeBuilder<'_> {
    pub(crate) fn emit_string_literal(
        &mut self,
        destination: &Destination,
        node: &Node,
        string: &str,
        ctx: &Context,
    ) {
        let string_bytes = string.as_bytes();
        let string_byte_count = string_bytes.len();
        let string_cap_bytes = string_byte_count;

        // Create a combined buffer for the header and string data
        let total_size = size_of::<VecHeader>() + string_byte_count;
        let mut combined_buffer = Vec::with_capacity(total_size);

        // Create the string header
        let string_header = VecHeader {
            capacity: string_cap_bytes as u16,
            element_count: string_byte_count as u16,
            element_size: 1, // size of byte
            padding: VEC_HEADER_MAGIC_CODE,
        };

        // Convert string header to bytes (little-endian)
        // Create a properly aligned header with little-endian byte order
        let le_header = VecHeader {
            capacity: string_header.capacity.to_le(),
            element_count: string_header.element_count.to_le(),
            element_size: string_header.element_size.to_le(),
            padding: string_header.padding.to_le(),
        };

        // Convert the entire struct to bytes
        let header_bytes = unsafe {
            std::slice::from_raw_parts((&raw const le_header).cast::<u8>(), size_of::<VecHeader>())
        };

        // Add header and string data to the combined buffer
        combined_buffer.extend_from_slice(header_bytes);
        combined_buffer.extend_from_slice(string_bytes);
        if string_cap_bytes > string_byte_count {
            // this will fill the remaining slot(s) so that the blob truly has
            // cap_bytes of storage.
            combined_buffer.resize(total_size, 0);
        }

        // Allocate the combined buffer in constant memory
        let string_header_in_heap_ptr = HeapMemoryAddress(
            self.state
                .constants_manager
                .allocate_byte_array(&combined_buffer)
                .addr()
                .0,
        );

        match destination {
            Destination::Unit => {
                panic!("can not write string to unit")
            }
            Destination::Register(target_register) => {
                self.builder.add_mov_32_immediate_value(
                    target_register,
                    string_header_in_heap_ptr.0,
                    node,
                    &format!("constant string '{string}'"),
                );
            }
            Destination::Memory(memory_location) => {
                // Special case: if destination is StringStorage, copy the string data directly
                if matches!(
                    memory_location.ty.basic_type.kind,
                    BasicTypeKind::StringStorage {
                        element_type: _,
                        char: _,
                        capacity: _
                    }
                ) {
                    // Create source memory location pointing to the constant string
                    let source_pointer_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(string_type()),
                        "source pointer for string literal vec copy",
                    );
                    self.builder.add_mov_32_immediate_value(
                        source_pointer_reg.register(),
                        string_header_in_heap_ptr.0,
                        node,
                        &format!("load source pointer for string literal '{string}'"),
                    );

                    // Create a memory location for the source StringView
                    let source_memory_location =
                        swamp_vm_types::MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                            source_pointer_reg.register,
                        );

                    // Perform vec-like copy using the proper helper
                    self.emit_copy_vec_like_value_helper(
                        memory_location,
                        &source_memory_location,
                        node,
                        &format!("copy const string literal '{string}' to StringStorage"),
                    );
                } else {
                    // Normal case: just store the pointer for StringView destinations
                    let temp_string_literal_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(string_type()),
                        "temporary for string literal",
                    );
                    self.builder.add_mov_32_immediate_value(
                        temp_string_literal_reg.register(),
                        string_header_in_heap_ptr.0,
                        node,
                        &format!("set const string literal '{string}'"),
                    );
                    self.builder.add_st32_using_ptr_with_offset(
                        memory_location,
                        temp_string_literal_reg.register(),
                        node,
                        &format!("copy const string literal '{string}' to memory location"),
                    );
                }
            }
        }
    }
}
