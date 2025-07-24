/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_types::TypeRef;
use swamp_vm_types::types::{Destination, VmType};
use swamp_vm_types::{MemoryLocation, PointerLocation};

impl CodeBuilder<'_> {
    pub(crate) fn grid_subscript_helper(
        &mut self,
        grid_header_location: &Destination,
        analyzed_element_type: &TypeRef,
        x_expr: &Expression,
        y_expr: &Expression,
        ctx: &Context,
    ) -> Destination {
        let pointer_location = self.grid_subscript_helper_helper(
            grid_header_location,
            analyzed_element_type,
            x_expr,
            y_expr,
            ctx,
        );
        // Fix: Dereference the pointer like map subscript does, instead of returning pointer to pointer
        Destination::Memory(MemoryLocation::new_copy_over_whole_type_with_zero_offset(
            pointer_location.ptr_reg,
        ))
    }

    /// Emits Swamp VM opcodes to calculate the memory address of an element within an array.
    ///
    /// The function targets a vec-like structured with a `u16` length prefix followed by
    /// contiguous element data in memory. It uses the provided `int_expression` as the
    /// index for the lookup. Calls the `subscript_helper_from_location_to_location` to
    /// emit the bounds checking.
    pub fn vec_subscript_helper(
        &mut self,
        vec_header_location: &Destination,
        analyzed_element_type: &TypeRef,
        int_expr: &Expression,
        ctx: &Context,
    ) -> Destination {
        let pointer_location = self.vec_subscript_helper_helper(
            vec_header_location,
            analyzed_element_type,
            int_expr,
            ctx,
        );
        Destination::Memory(pointer_location.memory_location())
    }

    pub fn emit_vec_subscript_range_helper(
        &mut self,
        destination_memory_location: &Destination,
        source_memory_location: &Destination,
        range_expr: &Expression,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) {
        let destination_pointer = self.emit_compute_effective_address_to_register(
            destination_memory_location,
            node,
            "get the destination vec",
        );
        let destination_pointer_loc = PointerLocation::new(destination_pointer);
        let source_pointer = self.emit_compute_effective_address_to_register(
            source_memory_location,
            node,
            "get vector source address to take range from",
        );
        let source_pointer_loc = PointerLocation::new(source_pointer);

        let range_header_register = self.emit_scalar_rvalue(range_expr, ctx);

        self.builder.add_vec_copy_range(
            &destination_pointer_loc,
            &source_pointer_loc,
            &range_header_register,
            node,
            "copy range, but leave the capacity on the destination",
        );
    }

    pub fn grid_subscript_helper_helper(
        &mut self,
        grid_header_location: &Destination,
        analyzed_element_type: &TypeRef,
        x_int_expr: &Expression,
        y_int_expr: &Expression,
        ctx: &Context,
    ) -> PointerLocation {
        let gen_element_type = self.state.layout_cache.layout(analyzed_element_type);
        let x_int_reg = self.emit_scalar_rvalue(x_int_expr, ctx);
        let y_int_reg = self.emit_scalar_rvalue(y_int_expr, ctx);
        let node = &x_int_expr.node;

        let grid_header_ptr_reg = self.emit_compute_effective_address_to_register(
            grid_header_location,
            node,
            "get grid header absolute pointer",
        );

        let grid_header_ptr_location = PointerLocation {
            ptr_reg: grid_header_ptr_reg,
        };

        let absolute_pointer_to_element = self.temp_registers.allocate(
            VmType::new_contained_in_register(gen_element_type.clone()),
            "temporary target",
        );
        self.builder.add_grid_get_entry_addr(
            absolute_pointer_to_element.register(),
            &grid_header_ptr_location,
            &x_int_reg,
            &y_int_reg,
            gen_element_type.total_size,
            node,
            "lookup grid subscript",
        );

        PointerLocation {
            ptr_reg: absolute_pointer_to_element.register,
        }
    }

    pub fn vec_subscript_helper_helper(
        &mut self,
        vec_header_location: &Destination,
        analyzed_element_type: &TypeRef,
        int_expr: &Expression,
        ctx: &Context,
    ) -> PointerLocation {
        let gen_element_type = self.state.layout_cache.layout(analyzed_element_type);
        let index_int_reg = self.emit_scalar_rvalue(int_expr, ctx);
        let node = &int_expr.node;

        let vec_header_ptr_reg = self.emit_compute_effective_address_to_register(
            vec_header_location,
            node,
            "get vec header absolute pointer",
        );

        let absolute_pointer_to_element = self.temp_registers.allocate(
            VmType::new_contained_in_register(gen_element_type.clone()),
            "temporary target",
        );
        self.builder.add_vec_subscript(
            absolute_pointer_to_element.register(),
            &vec_header_ptr_reg,
            &index_int_reg,
            gen_element_type.total_size,
            node,
            "lookup veclike subscript",
        );

        PointerLocation {
            ptr_reg: absolute_pointer_to_element.register,
        }
    }
}
