/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_vm_isa::{MemoryOffset, COLLECTION_ELEMENT_COUNT_OFFSET, VEC_HEADER_PAYLOAD_OFFSET};
use swamp_vm_types::types::{BasicTypeRef, Destination};
use swamp_vm_types::{
    AggregateMemoryLocation, MemoryLocation, PointerLocation,
};

impl CodeBuilder<'_> {
    pub(crate) fn emit_initializer_list_into_target_vec_like_memory_location(
        &mut self,
        transient_location: &PointerLocation, // Bucket start
        element_gen_type: &BasicTypeRef,
        expressions: &[Expression],
        node: &Node,
        ctx: &Context,
    ) {
        // We assume that the target_reg holds a starting pointer where we can put the slice
        let element_size = element_gen_type.total_size.0;

        let hwm = self.temp_registers.save_mark();

        // The reason we need a safe temp reg is that each emit_expression_into_target_memory() can clobber
        // any register when calling functions
        let lvalue_location_temp_reg = self.temp_registers.allocate(
            transient_location.ptr_reg.ty.clone(),
            "must make a copy of the destination register for initializer list",
        );
        self.builder.add_mov_reg(
            lvalue_location_temp_reg.register(),
            &transient_location.ptr_reg,
            node,
            "copy to initializer destination to safe place",
        );

        let lvalue_location = AggregateMemoryLocation::new(
            MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                lvalue_location_temp_reg.register,
            ),
        );

        for (index, expr) in expressions.iter().enumerate() {
            let offset_to_element = index as u32 * element_size;
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

        // When we have emitted every expression in the list, we can give up on our safe temporary register
        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) fn emit_vec_like_collection_init_from_initialization_list(
        &mut self,
        output_destination: &Destination,
        elements: &[Expression],
        node: &Node,
        ctx: &Context,
    ) {
        let hwm = self.temp_registers.save_mark();

        let output_memory_location_reg =
            self.emit_compute_effective_address_to_register(output_destination, node, "set start");
        let output_memory_location =
            MemoryLocation::new_copy_over_whole_type_with_zero_offset(output_memory_location_reg);

        let element_type = output_memory_location
            .ty
            .basic_type
            .element()
            .unwrap_or_else(|| {
                panic!(
                    "could not find element for {:?}",
                    output_memory_location.ty.basic_type
                )
            });

        // Make sure that the destination can hold all the elements, and initialize length
        let length_reg = self.emit_check_that_known_len_is_less_or_equal_to_capacity(
            &output_memory_location,
            elements.len(),
            node,
            "check initializer elements can fit",
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

        self.emit_initializer_list_into_target_vec_like_memory_location(
            &bucket_start_memory_location_reg,
            &element_type,
            elements,
            node,
            ctx,
        );

        self.temp_registers.restore_to_mark(hwm);
    }
}
