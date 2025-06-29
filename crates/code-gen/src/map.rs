/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{Expression, MapType};
use swamp_vm_types::types::{BasicTypeRef, Destination, VmType, u32_type};
use swamp_vm_types::{CountU16, MemoryLocation, PointerLocation};

impl CodeBuilder<'_> {
    /// Emits Swamp VM opcodes to calculate the memory address of an element within a map.
    pub fn map_subscript_helper(
        &mut self,
        map_header_location: &Destination,
        map_type: &MapType,
        key_expression: &Expression,
        should_create_if_needed: bool,
        ctx: &Context,
    ) -> Destination {
        let map_header_ptr_reg = self.emit_compute_effective_address_to_register(
            map_header_location,
            &key_expression.node,
            "get map header absolute pointer",
        );

        let pointer_location = PointerLocation::new(map_header_ptr_reg);

        // We have to get the key materialized in a temporary storage, so the map can calculate the hash for it.
        let key_temp_storage_reg =
            self.emit_aggregate_pointer_or_pointer_to_scalar_memory(key_expression, ctx);

        let gen_value_type = self.state.layout_cache.layout(&map_type.value);
        let map_entry_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(gen_value_type),
            "map entry temp",
        );

        if should_create_if_needed {
            self.builder.add_map_get_or_reserve_entry_location(
                map_entry_reg.register(),
                &pointer_location,
                &key_temp_storage_reg,
                &key_expression.node,
                "lookup the entry for this key in the map",
            );
        } else {
            self.builder.add_map_get_entry_location(
                map_entry_reg.register(),
                &pointer_location,
                &key_temp_storage_reg,
                &key_expression.node,
                "lookup the entry for this key in the map",
            );
        }

        // a map subscript is always a memory location
        Destination::Memory(MemoryLocation::new_copy_over_whole_type_with_zero_offset(
            map_entry_reg.register,
        ))
    }

    pub(crate) fn emit_map_storage_init_from_initializer_pair_list(
        &mut self,
        target_map_header_ptr_reg: &PointerLocation, // Points to MapStorage
        initializer_pair_list_expressions: &[(Expression, Expression)],
        key_type: &BasicTypeRef,
        value_type: &BasicTypeRef,
        logical_limit: usize,
        node: &Node,
        ctx: &Context,
    ) {
        let hwm = self.temp_registers.save_mark();
        //assert!(is_power_of_two(logical_limit));

        let len = initializer_pair_list_expressions.len();
        //        let aligned_key_size = key_value_tuple_type.aligned_size_of_field(0);
        //      let aligned_value_size = key_value_tuple_type.aligned_size_of_field(1);
        let unaligned_key_size = key_type.total_size;
        let key_alignment = key_type.max_alignment;
        let unaligned_value_size = value_type.total_size;
        let value_alignment = value_type.max_alignment;

        let comment = "";
        let init_key_size = self.temp_registers.allocate(
            VmType::new_contained_in_register(u32_type()),
            &format!("{comment} - init map key_size reg"),
        );
        self.builder.add_mov_32_immediate_value(
            init_key_size.register(),
            unaligned_key_size.0,
            node,
            &format!("{comment} -set init key_size value to"),
        );

        let init_value_size = self.temp_registers.allocate(
            VmType::new_contained_in_register(u32_type()),
            &format!("{comment} - init map value_size reg"),
        );
        self.builder.add_mov_32_immediate_value(
            init_value_size.register(),
            unaligned_value_size.0,
            node,
            &format!("{comment} -set init value_size value to "),
        );

        debug_assert!(logical_limit >= len, "checked");
        if logical_limit > 0 || len > 0 {
            self.builder.add_map_init_set_capacity(
                target_map_header_ptr_reg,
                CountU16(logical_limit as u16),
                init_key_size.register(),
                key_alignment,
                init_value_size.register(),
                value_alignment,
                node,
                "initialize map (capacity, key_size, total_key_and_value_size)",
            );
        }

        let key_frame_location = self.allocate_frame_space_and_return_destination_to_it(
            &u32_type(),
            node,
            "key temporary storage",
        );
        let key_frame_place = key_frame_location
            .vm_type()
            .unwrap()
            .frame_placed_type()
            .unwrap();

        let value_target_register = self.temp_registers.allocate(
            VmType::new_unknown_placement(value_type.clone()),
            "key temp",
        );

        for (key_expr, value_expr) in initializer_pair_list_expressions {
            let initializer_pair_node = &key_expr.node;

            if key_frame_location.ty().total_size.0 > 1 {
                self.builder.add_frame_memory_clear(
                    key_frame_place.region(),
                    initializer_pair_node,
                    "clear key area each time, to make sure hash is calculated correctly",
                );
            }

            self.emit_expression_into_target_memory(
                key_frame_location.grab_memory_location(),
                key_expr,
                "store key to memory",
                ctx,
            );

            self.builder.add_map_get_or_reserve_entry_location(
                value_target_register.register(),
                target_map_header_ptr_reg,
                &key_frame_location.grab_memory_location().base_ptr_reg,
                initializer_pair_node,
                "find existing or create a map entry to write into",
            );

            self.emit_expression_into_target_memory(
                &MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                    value_target_register.register().clone(),
                ),
                value_expr,
                "put value into map entry value section",
                ctx,
            );
        }

        self.temp_registers.restore_to_mark(hwm);
    }
}
