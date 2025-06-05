/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_vm_types::types::{BasicType, BasicTypeKind, Destination};
use swamp_vm_types::{AggregateMemoryLocation, MemoryOffset, PointerLocation};

impl CodeBuilder<'_> {
    pub(crate) fn emit_initializer_list_into_target_lvalue_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        element_gen_type: &BasicType,
        expressions: &[Expression],
        ctx: &Context,
    ) {
        // We assume that the target_reg holds a starting pointer where we can put the slice
        let element_size = element_gen_type.total_size.0;

        let hwm = self.temp_registers.save_mark();

        for (index, expr) in expressions.iter().enumerate() {
            let offset_to_element = index as u16 * element_size;
            let slice_element_location = lvalue_location
                .offset(MemoryOffset(offset_to_element), element_gen_type.clone())
                .location;
            self.emit_expression_into_target_memory(
                &slice_element_location,
                expr,
                &format!("store slice element {index} of type {element_gen_type} into memory"),
                ctx,
            );
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) fn emit_container_init_from_initialization_list(
        &mut self,
        output_destination: &Destination,
        elements: &[Expression],
        node: &Node,
        ctx: &Context,
    ) {
        match &output_destination.ty().underlying().kind {
            BasicTypeKind::VecStorage(element_type, capacity) => {
                let absolute_ptr_reg = self.emit_compute_effective_address_to_register(
                    output_destination,
                    node,
                    "absolute vec storage target",
                );
                let pointer_location = PointerLocation {
                    ptr_reg: absolute_ptr_reg,
                };
                self.emit_vec_storage_init(
                    &pointer_location,
                    elements,
                    element_type,
                    *capacity,
                    output_destination.ty(),
                    node,
                    ctx,
                );
            }

            BasicTypeKind::FixedCapacityArray(element_type, capacity) => {
                let absolute_ptr_reg = self.emit_compute_effective_address_to_register(
                    output_destination,
                    node,
                    "absolute vec storage target",
                );
                let pointer_location = PointerLocation {
                    ptr_reg: absolute_ptr_reg,
                };
                self.emit_fixed_storage_array_init(
                    &pointer_location,
                    elements,
                    element_type,
                    *capacity,
                    output_destination.ty(),
                    node,
                    ctx,
                );
            }
            _ => panic!("what is this {}", output_destination.ty()),
        }
    }
}
