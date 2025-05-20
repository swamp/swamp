use crate::DetailedLocation;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::Expression;
use swamp_types::Type;
use swamp_vm_types::types::{VmType, int_type};

impl CodeBuilder<'_> {
    /// Emits Swamp VM opcodes to calculate the memory address of an element within a map.
    pub fn map_subscript_helper(
        &mut self,
        map_header_location: &DetailedLocation,
        analyzed_key_type: &Type,
        key_expression: &Expression,
        ctx: &Context,
    ) -> DetailedLocation {
        let map_header_ptr_reg = self.emit_ptr_reg_from_detailed_location(
            &map_header_location,
            &key_expression.node,
            "get map header absolute pointer",
        );

        // We have to get the key materialized in a temporary storage, so the map can calculate the hash for it.
        let key_temp_storage_reg = self.temp_space_for_type(analyzed_key_type, "key temp");
        self.emit_expression_materialize(key_temp_storage_reg.register(), key_expression, ctx);

        let map_entry_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(int_type()), "map entry temp");

        self.builder.add_map_get_entry_location(
            map_entry_reg.register(),
            &map_header_ptr_reg,
            key_temp_storage_reg.register(),
            &key_expression.node,
            "lookup the entry for this key in the map",
        );

        DetailedLocation::Register {
            reg: map_entry_reg.register,
        }
    }
}
