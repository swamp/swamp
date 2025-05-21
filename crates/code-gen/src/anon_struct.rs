use crate::GeneratedExpressionResult;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_struct_type;
use source_map_node::Node;
use swamp_semantic::{AnonymousStructLiteral, Expression};
use swamp_types::{AnonymousStructType, Type};
use swamp_vm_types::AggregateMemoryLocation;
use swamp_vm_types::types::{BasicType, BasicTypeKind};

impl CodeBuilder<'_> {
    pub(crate) fn emit_anonymous_struct_into_memory(
        &mut self,
        aggregate_lvalue_memory_location: &AggregateMemoryLocation,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        node: &Node,
        base_context: &Context,
    ) {
        self.emit_struct_literal_into_memory_location(
            aggregate_lvalue_memory_location,
            anon_struct_type,
            source_order_expressions,
            node,
            "store struct into memory",
            base_context,
        );
    }

    fn emit_struct_literal_into_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        struct_type_ref: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let gen_source_struct_type = layout_struct_type(struct_type_ref, "");

        // TODO: Bring this back // assert_eq!(target_reg.size().0, gen_source_struct_type.total_size.0);
        /* TODO: Bring this back
        assert_eq!(
            source_order_expressions.len(),
            gen_source_struct_type.fields.len()
        );
        */

        let struct_type = BasicType {
            total_size: gen_source_struct_type.total_size,
            max_alignment: gen_source_struct_type.max_alignment,
            kind: BasicTypeKind::Struct(gen_source_struct_type.clone()),
        };

        for (offset_item, (field_index, _node, source_expression)) in gen_source_struct_type
            .fields
            .iter()
            .zip(source_order_expressions)
        {
            let real_offset_item = struct_type.get_field_offset(*field_index).unwrap();
            let modified_lvalue_location =
                lvalue_location.offset(real_offset_item.offset, real_offset_item.ty.clone());

            self.emit_expression_into_target_memory(
                &modified_lvalue_location.location,
                source_expression,
                &format!(
                    "store expression into struct field {}:{}",
                    field_index, offset_item.name
                ),
                ctx,
            );
        }

        GeneratedExpressionResult::default()
    }

    pub(crate) fn emit_anonymous_struct_literal_into_memory_location(
        &mut self,
        lvalue_location: &AggregateMemoryLocation,
        anon_struct_literal: &AnonymousStructLiteral,
        ty: &Type,
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let anon_struct_type = match ty {
            Type::NamedStruct(named_struct) => named_struct.anon_struct_type.clone(),
            Type::AnonymousStruct(anon_struct_type) => anon_struct_type.clone(),
            _ => panic!("internal error with struct literal"),
        };

        self.emit_struct_literal_into_memory_location(
            lvalue_location,
            &anon_struct_type,
            &anon_struct_literal.source_order_expressions,
            node,
            comment,
            ctx,
        )
    }
}
