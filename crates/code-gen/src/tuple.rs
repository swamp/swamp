use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_tuple_items;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_types::Type;
use swamp_vm_types::AggregateMemoryLocation;
use swamp_vm_types::types::VmType;

impl CodeBuilder<'_> {
    pub(crate) fn emit_tuple_literal_into_memory(
        &mut self,
        aggregate_lvalue_location: &AggregateMemoryLocation,
        types: &[Type],
        expressions: &[Expression],
        ctx: &Context,
        node: &Node,
    ) {
        let gen_tuple_type = layout_tuple_items(types);
        /*
        let gen_tuple_placed = BasicType {
            total_size: gen_tuple_type.total_size,
            max_alignment: gen_tuple_type.max_alignment,
            kind: BasicTypeKind::Tuple(gen_tuple_type.clone()),
        };

        let frame_placed_tuple = self.frame_allocator.allocate_type(gen_tuple_placed);

        self.builder.add_frame_memory_clear(
            frame_placed_tuple.region(),
            node,
            "clear memory for tuple",
        );

        self.builder.add_lea(
            target_reg,
            frame_placed_tuple.addr(),
            node,
            &format!(
                "store the pointer to the frame allocated tuple in reg size:{} ty:{}",
                frame_placed_tuple.size(),
                frame_placed_tuple.ty()
            ),
        );
            */

        // TODO: Bring this back. //assert_eq!(gen_tuple_placed.total_size, target_reg.size());
        // TODO: Bring this back. //assert_eq!(gen_tuple_type.fields.len(), expressions.len());

        for (offset_item, expr) in gen_tuple_type.fields.iter().zip(expressions) {
            let hwm = self.temp_registers.save_mark();

            let temp_materialize_reg = self.temp_registers.allocate(
                VmType::new_unknown_placement(offset_item.ty.clone()),
                "emit_materialize for tuple element",
            );
            //let materialized_item_in_tuple_reg = temp_materialize_reg.register();

            self.emit_expression_into_target_memory(
                &aggregate_lvalue_location.location,
                &expr,
                &format!("emit tuple item {}", offset_item.name),
                ctx,
            );
            self.temp_registers.restore_to_mark(hwm);
        }
    }
}
