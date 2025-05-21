use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_types::Type;
use swamp_vm_types::PointerLocation;
use swamp_vm_types::types::{Destination, TupleType, VmType, int_type, u32_type};

impl CodeBuilder<'_> {
    /// Emits Swamp VM opcodes to calculate the memory address of an element within a map.
    pub fn map_subscript_helper(
        &mut self,
        map_header_location: &Destination,
        analyzed_key_type: &Type,
        key_expression: &Expression,
        ctx: &Context,
    ) -> Destination {
        let map_header_ptr_reg = self.emit_ptr_reg_from_detailed_location(
            map_header_location,
            &key_expression.node,
            "get map header absolute pointer",
        );

        let gen_key_type = layout_type(analyzed_key_type);

        // We have to get the key materialized in a temporary storage, so the map can calculate the hash for it.
        let key_temp_storage_reg = self.allocate_frame_space_and_assign_register(
            &gen_key_type,
            &key_expression.node,
            "key storage region",
        );
        self.emit_expression(
            &key_temp_storage_reg,
            key_expression,
            //"map subscript",
            ctx,
        );

        let map_entry_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(int_type()), "map entry temp");

        self.builder.add_map_get_entry_location(
            map_entry_reg.register(),
            &map_header_ptr_reg,
            key_temp_storage_reg.grab_register(),
            &key_expression.node,
            "lookup the entry for this key in the map",
        );

        Destination::new_reg(map_entry_reg.register)
    }

    pub(crate) fn emit_map_storage_init_from_slice_pair_literal(
        &mut self,
        target_map_header_ptr_reg: &PointerLocation, // Points to MapStorage
        slice_pair_literal: &[(Expression, Expression)],
        key_value_tuple_type: &TupleType,
        capacity: usize,
        node: &Node,
        ctx: &Context,
    ) {
        let hwm = self.temp_registers.save_mark();

        let len = slice_pair_literal.len();
        debug_assert!(capacity >= len);
        if capacity > 0 || len > 0 {
            self.builder.add_map_init_set_capacity(
                target_map_header_ptr_reg,
                capacity as u16,
                key_value_tuple_type.fields[0].ty.total_size,
                key_value_tuple_type.total_size,
                node,
                "initialize map",
            );
        }

        let aggregate_location =
            self.allocate_frame_space_and_assign_register(&u32_type(), node, "key storage");

        let element_target_temp_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(u32_type()), "key temp");

        for (key_expr, value_expr) in slice_pair_literal {
            //self.emit_expression_materialize(&key_storage_register, key_expr, ctx);
            self.emit_expression_into_target_memory(
                aggregate_location.grab_memory_location(),
                value_expr,
                "store expression to memory if needed",
                ctx,
            );

            self.builder.add_map_get_or_reserve_entry_location(
                element_target_temp_reg.register(),
                target_map_header_ptr_reg,
                &aggregate_location.grab_memory_location().base_ptr_reg,
                node,
                "find existing or create a map entry to write into",
            );
        }

        self.temp_registers.restore_to_mark(hwm);
    }
}
