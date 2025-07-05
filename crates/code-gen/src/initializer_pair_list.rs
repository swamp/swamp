/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_vm_types::PointerLocation;
use swamp_vm_types::types::{BasicTypeKind, Destination};

impl CodeBuilder<'_> {
    pub fn emit_map_like_init_from_initialization_pair_list(
        &mut self,
        output_destination: &Destination,
        elements: &[(Expression, Expression)],
        node: &Node,
        ctx: &Context,
    ) {
        match &output_destination.ty().kind {
            BasicTypeKind::MapStorage {
                logical_limit,
                key_type,
                value_type,
                ..
            } => {
                assert!(elements.len() <= *logical_limit, "too many initializers");
                let target_map_header_ptr_reg = self.emit_compute_effective_address_to_register(
                    output_destination,
                    node,
                    "map header init",
                );
                let pointer_target = PointerLocation {
                    ptr_reg: target_map_header_ptr_reg,
                };

                //let adjusted_capacity = capacity.next_power_of_two();
                self.emit_map_storage_init_from_initializer_pair_list(
                    &pointer_target,
                    elements,
                    key_type,
                    value_type,
                    *logical_limit,
                    node,
                    "emit_container_init_from_initialization_pair_list",
                    ctx,
                );
            }

            _ => panic!(
                "unknown initializer pair list type:{}",
                output_destination.ty()
            ),
        }
    }
}
