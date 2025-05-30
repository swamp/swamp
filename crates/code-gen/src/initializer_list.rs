use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::Expression;
use swamp_vm_types::types::BasicType;
use swamp_vm_types::{AggregateMemoryLocation, MemoryOffset};

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
}
