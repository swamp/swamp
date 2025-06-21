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
    /*
    const fn emit_initializer_pair_list_into_target_lvalue_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        gen_tuple_type: &TupleType,
        expressions: &[(Expression, Expression)],
        node: &Node,
        ctx: &Context,
    ) {
        // We assume that the target_reg holds a starting pointer where we can put the slice
        let element_size = gen_tuple_type.total_size.0;

        let hwm = self.temp_registers.save_mark();

        let temp_r

        for (index, (key_expr, value_expr)) in expressions.iter().enumerate() {


            self.emit_map_storage_init_from_initializer_pair_list()


        }

        self.temp_registers.restore_to_mark(hwm);
    }

     */

    pub fn emit_container_init_from_initialization_pair_list(
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
                assert!(
                    elements.len() <= *logical_limit,
                    "too many initializers. should have been caught by analyzer"
                );
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
                    &key_type,
                    value_type,
                    *logical_limit,
                    node,
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
