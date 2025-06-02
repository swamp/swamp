use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use source_map_node::Node;
use swamp_semantic::{ArgumentExpression, Expression};
use swamp_types::Type;
use swamp_vm_types::types::{BasicType, BasicTypeKind, Destination, VmType, u32_type, vec_type};
use swamp_vm_types::{AggregateMemoryLocation, MemoryLocation, MemoryOffset, PointerLocation};
use tracing::info;

impl CodeBuilder<'_> {
    /// Emits Swamp VM opcodes to calculate the memory address of an element within an array.
    ///
    /// The function targets a vec-like structured with a `u16` length prefix followed by
    /// contiguous element data in memory. It uses the provided `int_expression` as the
    /// index for the lookup. Calls the `subscript_helper_from_location_to_location` to
    /// emit the bounds checking.
    pub fn vec_subscript_helper(
        &mut self,
        vec_header_location: &Destination,
        analyzed_element_type: &Type,
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

    pub fn vec_subscript_helper_helper(
        &mut self,
        vec_header_location: &Destination,
        analyzed_element_type: &Type,
        int_expr: &Expression,
        ctx: &Context,
    ) -> PointerLocation {
        let gen_element_type = layout_type(analyzed_element_type);
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

    /*
         let element_basic_type = layout_type(analyzed_element_type);
       let vec_count_reg = self
           .temp_registers
           .allocate(VmType::new_unknown_placement(u16_type()), "vec count");
       let vec_header_ptr_reg = self.emit_ptr_reg_from_detailed_location(
           current_location,
           &int_expression.node,
           "get vec header absolute pointer",
       );
       self.builder.add_ld16_from_pointer_with_offset_u16(
           vec_count_reg.register(),
           &vec_header_ptr_reg,
           VEC_HEADER_COUNT_OFFSET,
           &int_expression.node,
           "load vec count for bounds check",
       );
       let payload_location = current_location.add_offset(
           VEC_HEADER_PAYLOAD_OFFSET,
           VmType::new_unknown_placement(element_basic_type.clone()),
       );
       self.subscript_helper_from_location_to_location(
           payload_location,
           &element_basic_type,
           int_expression,
           BoundsCheck::RegisterWithMaxCount(vec_count_reg.register),
           &int_expression.node,
           &format!("rvalue {analyzed_element_type}"),
           ctx,
       )
    */

    fn emit_intrinsic_vec_create(&self, arguments: &Vec<ArgumentExpression>) {
        for arg in arguments {
            info!(?arg, "argument");
        }
    }

    fn emit_intrinsic_vec_init_capacity_set_payload_addr(
        &mut self,
        pointer_lvalue_location: &PointerLocation,
        arguments: &[ArgumentExpression],
        node: &Node,
        ctx: &Context,
    ) {
        if let ArgumentExpression::Expression(found_expr) = &arguments[0] {
            let hwm = self.temp_registers.save_mark();
            let element_base_ptr_reg = self.temp_registers.allocate(
                VmType::new_unknown_placement(vec_type()),
                "element base ptr",
            );
            let BasicTypeKind::VecStorage(element_type, fixed_size_capacity) =
                &pointer_lvalue_location.ptr_reg.ty.basic_type.kind
            else {
                panic!("mut have storage");
            };

            self.builder.add_vec_init_fill_capacity_and_element_addr(
                pointer_lvalue_location,
                &element_base_ptr_reg.register,
                *fixed_size_capacity as u16,
                0,
                node,
                "init vec",
            );

            let memory = self.emit_scalar_rvalue(found_expr, ctx);
            self.temp_registers.restore_to_mark(hwm);
        } else {
            panic!("vec_from_slice");
        }
    }

    // When initializing a VecStorage with an initialization list
    pub(crate) fn emit_fixed_storage_array_init(
        &mut self,
        vec_storage_lvalue_memory_location: &PointerLocation, // Points to VecStorage
        slice_literal: &[Expression],
        element_type: &BasicType,
        capacity: usize,
        debug_vec_storage_type: &BasicType,
        node: &Node,
        ctx: &Context,
    ) {
        let elements_base_ptr_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(u32_type()),
            &format!("{debug_vec_storage_type}::elements"),
        );

        let len = slice_literal.len();
        debug_assert!(capacity >= len);
        if capacity > 0 || len > 0 {
            self.builder
                .add_fixed_capacity_array_init_fill_capacity_and_get_element_addr(
                    vec_storage_lvalue_memory_location,
                    elements_base_ptr_reg.register(),
                    capacity as u16,
                    node,
                    "initialize vec from initialization list",
                );
        } else {
            info!("skipping, no capacity or no length");
        }

        let elements_base_ptr_reg = AggregateMemoryLocation {
            location: MemoryLocation {
                base_ptr_reg: elements_base_ptr_reg.register,
                offset: MemoryOffset(0),
                ty: VmType::new_unknown_placement(element_type.clone()),
            },
        };

        self.emit_initializer_list_into_target_lvalue_memory_location(
            &elements_base_ptr_reg,
            element_type,
            slice_literal,
            ctx,
        );
    }

    // When initializing a VecStorage with an initialization list
    pub(crate) fn emit_vec_storage_init(
        &mut self,
        vec_storage_lvalue_memory_location: &PointerLocation, // Points to VecStorage
        slice_literal: &[Expression],
        element_type: &BasicType,
        capacity: usize,
        debug_vec_storage_type: &BasicType,
        node: &Node,
        ctx: &Context,
    ) {
        let elements_base_ptr_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(u32_type()),
            &format!("{debug_vec_storage_type}::elements"),
        );

        let len = slice_literal.len();
        debug_assert!(capacity >= len);
        if capacity > 0 || len > 0 {
            self.builder.add_vec_init_fill_capacity_and_element_addr(
                vec_storage_lvalue_memory_location,
                elements_base_ptr_reg.register(),
                capacity as u16,
                len as u16,
                node,
                "initialize vec from slice",
            );
        } else {
            info!("skipping, no capacity or no length");
        }

        let elements_base_ptr_reg = AggregateMemoryLocation {
            location: MemoryLocation {
                base_ptr_reg: elements_base_ptr_reg.register,
                offset: MemoryOffset(0),
                ty: VmType::new_unknown_placement(element_type.clone()),
            },
        };

        self.emit_initializer_list_into_target_lvalue_memory_location(
            &elements_base_ptr_reg,
            element_type,
            slice_literal,
            ctx,
        );
    }
}
