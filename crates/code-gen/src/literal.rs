/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use std::mem::size_of;
use swamp_vm_types::types::{Destination, VmType, string_type};
use swamp_vm_types::{HeapMemoryAddress, STRING_SECRET, StringHeader};

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

        // Create a combined buffer for the header and string data
        let total_size = size_of::<StringHeader>() + string_byte_count;
        let mut combined_buffer = Vec::with_capacity(total_size);

        // Create the string header
        let string_header = StringHeader {
            capacity: string_byte_count as u16,
            byte_count: string_byte_count as u16,
            padding: STRING_SECRET,
        };

        // Convert string header to bytes (little-endian)
        let mut header_bytes = [0u8; size_of::<StringHeader>()];
        header_bytes[0..2].copy_from_slice(&string_header.capacity.to_le_bytes());
        header_bytes[2..4].copy_from_slice(&string_header.byte_count.to_le_bytes());
        header_bytes[4..6].copy_from_slice(&string_header.padding.to_le_bytes());

        // Add header and string data to the combined buffer
        combined_buffer.extend_from_slice(&header_bytes);
        combined_buffer.extend_from_slice(string_bytes);

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
                let temp_string_literal_reg = self.temp_registers.allocate(
                    VmType::new_contained_in_register(string_type()),
                    "temporary for string literal",
                );
                self.builder.add_mov_32_immediate_value(
                    temp_string_literal_reg.register(),
                    string_header_in_heap_ptr.0,
                    node,
                    "string literal",
                );
                self.builder.add_st32_using_ptr_with_offset(
                    memory_location,
                    temp_string_literal_reg.register(),
                    node,
                    "copy string pointer literal into destination memory",
                );
            }
        }
    }
}
