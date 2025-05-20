use crate::DetailedLocation;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::Expression;
use swamp_types::Type;

impl CodeBuilder<'_> {
    /// Emits Swamp VM opcodes to calculate the memory address of an element within a map.
    pub fn map_subscript_helper(
        &mut self,
        current_location: &DetailedLocation,
        analyzed_element_type: &Type,
        value_type: &Type,
        int_expression: &Expression,
        ctx: &Context,
    ) -> DetailedLocation {
        todo!()
    }
}
