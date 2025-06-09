/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_vm_types::types::{BasicType, Destination, VmType};
use swamp_vm_types::{
    AggregateMemoryLocation, COLLECTION_ELEMENT_COUNT_OFFSET, MemoryLocation, MemoryOffset,
    VEC_HEADER_PAYLOAD_OFFSET,
};

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

    pub(crate) fn emit_collection_init_from_initialization_list(
        &mut self,
        output_destination: &Destination,
        elements: &[Expression],
        node: &Node,
        ctx: &Context,
    ) {
        let output_memory_location = output_destination.grab_memory_location();
        let element_type = output_memory_location
            .ty
            .basic_type
            .element()
            .expect(&format!(
                "could not find element for {:?}",
                output_memory_location.ty.basic_type
            ));
        self.emit_initialize_target_memory_first_time(
            output_memory_location,
            node,
            "initialize collection for the first time",
        );
        let elements_base_ptr_reg = AggregateMemoryLocation {
            location: MemoryLocation {
                base_ptr_reg: output_memory_location.base_ptr_reg.clone(),
                offset: MemoryOffset(0),
                ty: VmType::new_unknown_placement(element_type.clone()),
            },
        };

        // Make sure that the destination can hold all the elements, and initialize length
        let length_reg = self.emit_check_that_known_len_is_less_or_equal_to_capacity(
            output_memory_location,
            elements.len(),
            node,
            "check initializer elements can fix",
        );

        if !output_memory_location
            .ty
            .element_count_always_same_as_capacity()
        {
            let len_location =
                output_memory_location.unsafe_add_offset(COLLECTION_ELEMENT_COUNT_OFFSET);
            self.builder.add_st16_using_ptr_with_offset(
                &len_location,
                &length_reg,
                node,
                "initialize element_count so initializer list doesn't have to",
            );
        }

        let bucket_start_memory_location =
            output_memory_location.unsafe_add_offset(VEC_HEADER_PAYLOAD_OFFSET);

        let bucket_start_memory_location_reg = self
            .emit_compute_effective_address_from_location_to_register(
                &bucket_start_memory_location,
                node,
                "absolute vec storage target",
            );

        let bucket_start_aggregate_location = AggregateMemoryLocation {
            location: MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                bucket_start_memory_location_reg,
            ),
        };

        self.emit_initializer_list_into_target_lvalue_memory_location(
            &bucket_start_aggregate_location,
            element_type,
            elements,
            ctx,
        );
        /*
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

         */
    }
}
