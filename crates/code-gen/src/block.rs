/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use source_map_node::Node;
use swamp_vm_types::MemoryLocation;

impl CodeBuilder<'_> {
    pub fn emit_block_copy_with_size_from_location(
        &mut self,
        destination_location: &MemoryLocation,
        source_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let dest_pointer_location = self.emit_compute_effective_address_from_location_to_register(
            destination_location,
            node,
            "flatten destination location for block copy",
        );
        let source_pointer_location = self
            .emit_compute_effective_address_from_location_to_register(
                source_location,
                node,
                "flatten source location for block copy",
            );
        self.builder.add_block_copy_with_immediate_size(
            &dest_pointer_location,
            &source_pointer_location,
            source_location.ty.basic_type.total_size,
            node,
            comment,
        );
    }
}
