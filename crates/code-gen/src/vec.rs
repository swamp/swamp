use crate::DetailedLocation;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use swamp_semantic::Expression;
use swamp_types::Type;
use swamp_vm_types::types::{BoundsCheck, VmType, u16_type};
use swamp_vm_types::{VEC_HEADER_COUNT_OFFSET, VEC_HEADER_PAYLOAD_OFFSET};

impl CodeBuilder<'_> {
    /// Emits Swamp VM opcodes to calculate the memory address of an element within an array.
    ///
    /// The function targets a vec-like structured with a `u16` length prefix followed by
    /// contiguous element data in memory. It uses the provided `int_expression` as the
    /// index for the lookup. Calls the `subscript_helper_from_location_to_location` to
    /// emit the bounds checking.
    pub fn vec_subscript_helper(
        &mut self,
        current_location: &DetailedLocation,
        analyzed_element_type: &Type,
        int_expression: &Expression,
        ctx: &Context,
    ) -> DetailedLocation {
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
}
