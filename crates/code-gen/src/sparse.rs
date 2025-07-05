/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_types::TypeRef;
use swamp_vm_types::types::{Destination, TypedRegister, VmType, pointer_type};
use swamp_vm_types::{AggregateMemoryLocation, MemoryLocation, MemoryOffset, PointerLocation};

impl CodeBuilder<'_> {
    pub fn sparse_subscript_helper(
        &mut self,
        vec_header_location: &Destination,
        analyzed_element_type: &TypeRef,
        int_expr: &Expression,
        ctx: &Context,
    ) -> Destination {
        let pointer_location = self.sparse_subscript_helper_helper(
            vec_header_location,
            analyzed_element_type,
            int_expr,
            ctx,
        );
        Destination::Memory(pointer_location.memory_location())
    }

    pub(crate) fn emit_sparse_add(
        &mut self,
        target: &TypedRegister,
        self_addr: &PointerLocation,
        element_expr: &Expression,
        node: &Node,
        ctx: &Context,
    ) {
        let element_gen_type = self.state.layout_cache.layout(&element_expr.ty);

        let temp_element_ptr = self.temp_registers.allocate(
            VmType::new_contained_in_register(pointer_type()),
            "pointer to new element",
        );

        self.builder.add_sparse_add_give_entry_address(
            temp_element_ptr.register(),
            target, // Should be the Int target
            self_addr,
            element_gen_type.total_size,
            node,
            "add space and get pointer to new element",
        );

        let location = AggregateMemoryLocation {
            location: MemoryLocation {
                base_ptr_reg: temp_element_ptr.register,
                offset: MemoryOffset(0),
                ty: VmType::new_unknown_placement(element_gen_type.clone()),
            },
        };

        // Initialize the allocated space
        if element_gen_type.is_aggregate() {
            self.emit_initialize_memory_for_any_type(
                &location.location,
                node,
                "initialize sparse set element memory",
            );
        }

        self.emit_expression_into_target_memory(
            &location.location,
            element_expr,
            "sparse add, put expression into entry memory location",
            ctx,
        );
    }

    pub(crate) fn emit_sparse_remove(
        &mut self,
        self_addr: &PointerLocation,
        element_expr: &Expression,
        node: &Node,
        ctx: &Context,
    ) {
        let int_reg = self.emit_scalar_rvalue(element_expr, ctx);
        self.builder
            .add_sparse_remove(self_addr, &int_reg, node, "");
    }

    pub(crate) fn emit_sparse_is_alive(
        &mut self,
        dest_bool_reg: &TypedRegister,
        self_addr: &PointerLocation,
        element_expr: &Expression,
        node: &Node,
        ctx: &Context,
    ) {
        let int_reg = self.emit_scalar_rvalue(element_expr, ctx);
        self.builder
            .add_sparse_is_alive(dest_bool_reg, self_addr, &int_reg, node, "");
    }
    pub fn sparse_subscript_helper_helper(
        &mut self,
        sparse_header_location: &Destination,
        analyzed_element_type: &TypeRef,
        int_expr: &Expression,
        ctx: &Context,
    ) -> PointerLocation {
        let gen_element_type = self.state.layout_cache.layout(analyzed_element_type);
        let index_int_reg = self.emit_scalar_rvalue(int_expr, ctx);
        let node = &int_expr.node;

        let sparse_header_ptr_reg = self.emit_compute_effective_address_to_register(
            sparse_header_location,
            node,
            "get sparse header absolute pointer",
        );

        let absolute_pointer_to_element = self.temp_registers.allocate(
            VmType::new_contained_in_register(gen_element_type.clone()),
            "temporary target",
        );

        let pointer_location = PointerLocation {
            ptr_reg: sparse_header_ptr_reg,
        };

        self.builder.add_sparse_get_entry_addr(
            absolute_pointer_to_element.register(),
            &pointer_location,
            &index_int_reg,
            gen_element_type.total_size,
            node,
            "lookup sparse subscript",
        );

        PointerLocation {
            ptr_reg: absolute_pointer_to_element.register,
        }
    }
}
