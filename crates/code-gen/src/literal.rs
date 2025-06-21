/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_vm_types::types::{Destination, VmType, string_type};
use swamp_vm_types::{HeapMemoryAddress, StringHeader};

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

        let data_ptr = self
            .state
            .constants_manager
            .allocate_byte_array(string_bytes);

        let string_header = StringHeader {
            heap_offset: data_ptr.addr().0,
            byte_count: string_byte_count as u32,
        };

        // Convert string header to bytes (little-endian)
        let mut header_bytes = [0u8; 8];
        header_bytes[0..4].copy_from_slice(&string_header.heap_offset.to_le_bytes());
        header_bytes[4..8].copy_from_slice(&string_header.byte_count.to_le_bytes());

        let string_header_in_heap_ptr = HeapMemoryAddress(
            self.state
                .constants_manager
                .allocate_byte_array(&header_bytes)
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
