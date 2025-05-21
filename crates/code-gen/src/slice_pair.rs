use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_types::Type;

impl CodeBuilder<'_> {
    fn emit_slice_pair_literal(
        &mut self,
        slice_type: &Type,
        expressions: &[(Expression, Expression)],
        node: &Node,
        ctx: &Context,
    ) {
        /*
        let Type::SlicePair(key_type, value_type) = slice_type else {
            panic!("should have been slice pair type")
        };

        assert!(key_type.is_concrete());
        assert!(value_type.is_concrete());

        assert_eq!(ctx.target_size(), SLICE_PAIR_HEADER_SIZE);

        //let constructed_tuple = Type::Tuple(vec![*key_type.clone(), *value_type.clone()]);

        let key_layout = layout_type(key_type);
        let value_layout = layout_type(value_type);

        //info!(?key_layout, ?value_layout, "layouts");

        let pair_size = key_layout.total_size.0 + value_layout.total_size.0; // Alignment is not relevant, since we will only access them using byte chunks.
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(pair_size * element_count);

        let heap_ptr_header_addr = ctx
            .register()
            .move_with_offset(SLICE_PTR_OFFSET, pointer_type());

        self.builder.add_alloc(
            &heap_ptr_header_addr,
            total_slice_size,
            node,
            "allocate slice pair",
        );

        let temp_key_ctx = self.temp_allocator.reserve_ctx(key_type);
        let temp_value_ctx = self.temp_allocator.reserve_ctx(value_type);

        for (index, (key_expr, value_expr)) in expressions.iter().enumerate() {
            self.emit_expression_materialize(key_expr, &temp_key_ctx);
            let key_offset = HeapMemoryOffset((index as u32) * pair_size as u32);
            self.builder.add_stx_for_assignment(
                &heap_ptr_header_addr,
                key_offset,
                temp_key_ctx.target(),
                node,
                "copy slice pair key element",
            );

            self.emit_expression_materialize(value_expr, &temp_value_ctx);
            let value_offset = HeapMemoryOffset((index as u32) * pair_size as u32).add(
                HeapMemorySize(key_layout.total_size.0 as u32),
                MemoryAlignment::U8,
            );
            self.builder.add_stx_for_assignment(
                &heap_ptr_header_addr,
                value_offset,
                temp_value_ctx.target(),
                node,
                "copy slice pair value element",
            );
        }

        self.builder.add_slice_pair_from_heap(
            ctx.register(),
            heap_ptr_header_addr.addr(),
            &key_layout,
            &value_layout,
            element_count,
            node,
            "creating slice pair",
        );
        /*

        SlicePairInfo {
            addr: TempFrameMemoryAddress(start_frame_address_to_transfer),
            key_size: key_layout.size,
            value_size: value_layout.size,
            element_count: CountU16(element_count),
            element_size,
        }

         */

         */
    }
}
