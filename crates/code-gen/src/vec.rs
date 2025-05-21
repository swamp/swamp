use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use source_map_node::Node;
use swamp_semantic::{Expression, MutRefOrImmutableExpression};
use swamp_types::Type;
use swamp_vm_types::types::{
    BasicTypeKind, BoundsCheck, OutputDestination, VmType, u16_type, vec_type,
};
use swamp_vm_types::{PointerLocation, VEC_HEADER_COUNT_OFFSET, VEC_HEADER_PAYLOAD_OFFSET};
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
        current_location: &OutputDestination,
        analyzed_element_type: &Type,
        int_expression: &Expression,
        ctx: &Context,
    ) -> OutputDestination {
        let element_basic_type = layout_type(&analyzed_element_type);
        let vec_count_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(u16_type()), "vec count");
        let vec_header_ptr_reg = self.emit_ptr_reg_from_detailed_location(
            &current_location,
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
    }

    fn emit_intrinsic_vec_create(&self, arguments: &Vec<MutRefOrImmutableExpression>) {
        for arg in arguments {
            info!(?arg, "argument");
        }
    }

    fn emit_intrinsic_vec_init_capacity_set_payload_addr(
        &mut self,
        pointer_lvalue_location: &PointerLocation,
        arguments: &[MutRefOrImmutableExpression],
        node: &Node,
        ctx: &Context,
    ) {
        if let MutRefOrImmutableExpression::Expression(found_expr) = &arguments[0] {
            let hwm = self.temp_registers.save_mark();
            let element_base_ptr_reg = self.temp_registers.allocate(
                VmType::new_unknown_placement(vec_type()),
                "element base ptr",
            );
            let BasicTypeKind::InternalVecStorage(element_type, fixed_size_capacity) =
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
}
