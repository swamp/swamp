use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_enum_into_tagged_union;
use source_map_node::Node;
use swamp_semantic::{EnumLiteralData, Expression, ExpressionKind, Literal};
use swamp_types::{EnumType, EnumVariantType};
use swamp_vm_types::types::{BasicTypeKind, TypedRegister, VmType, u8_type};
use swamp_vm_types::{AggregateMemoryLocation, ScalarMemoryLocation};

impl CodeBuilder<'_> {
    pub fn emit_aggregate_rvalue_to_lvalue(
        &mut self,
        location: &AggregateMemoryLocation,
        expr: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let node = &expr.node;
        match &expr.kind {
            ExpressionKind::Block(expressions) => {
                self.emit_block_to_aggregate_location(location, expressions, ctx);
            }
            ExpressionKind::AnonymousStructLiteral(anon_struct) => {
                self.emit_anonymous_struct_literal_into_memory_location(
                    location,
                    anon_struct,
                    &expr.ty,
                    node,
                    comment,
                    ctx,
                );
            }
            ExpressionKind::Option(maybe_option) => {
                self.emit_option_expression(location, node, maybe_option.as_deref(), ctx)
            }

            ExpressionKind::Literal(basic_literal) => match basic_literal {
                Literal::EnumVariantLiteral(enum_type, enum_variant, enum_variant_payload) => self
                    .emit_enum_variant_to_memory_location(
                        location,
                        enum_type,
                        enum_variant,
                        enum_variant_payload,
                        node,
                        ctx,
                    ),
                Literal::TupleLiteral(types, expressions) => {
                    self.emit_tuple_literal_into_memory(location, types, expressions, ctx, node);
                }
                Literal::Slice(_, _) => todo!(),
                Literal::SlicePair(_, _) => todo!(),
                _ => todo!(),
            },

            _ => {
                panic!("unknown aggregate to write {:?}", expr.kind)
            }
        }
    }

    pub fn emit_block_to_aggregate_location(
        &mut self,
        aggregate_memory_location: &AggregateMemoryLocation,
        expressions: &[Expression],
        ctx: &Context,
    ) {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                //i nfo!("this is others in block");
                self.emit_statement(expr, ctx);
            }
            if last.ty.is_unit() {
                self.emit_statement(last, ctx);
            } else {
                //            info!(?last.ty, ?target_reg.ty, "this is the last in the block!");
                self.emit_aggregate_rvalue_to_lvalue(
                    &aggregate_memory_location,
                    last,
                    "emit last in block as aggregate to location",
                    ctx,
                );
            }
        } else {
            // empty blocks are allowed for side effects
        }
    }

    pub fn emit_enum_variant_to_memory_location(
        &mut self,
        target_memory_location: &AggregateMemoryLocation,
        enum_type: &EnumType,
        a: &EnumVariantType,
        b: &EnumLiteralData,
        node: &Node,
        ctx: &Context,
    ) {
        let variant_index = a.common().container_index as usize;
        let variants = enum_type
            .variants
            .values()
            .map(|x| x.clone())
            .collect::<Vec<_>>();
        let layout_enum = layout_enum_into_tagged_union(&enum_type.assigned_name, &variants);
        let layout_variant = layout_enum.get_variant_by_index(variant_index);

        let hwm = self.temp_registers.save_mark();

        let temp_payload_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(layout_variant.ty.clone()),
            "variant literal payload",
        );

        self.builder.add_mov8_immediate(
            temp_payload_reg.register(),
            variant_index as u8,
            node,
            &format!("enum variant {} tag", a.common().assigned_name),
        );

        let tag_memory_location = target_memory_location.offset(layout_enum.tag_offset, u8_type());
        self.builder.add_st8_using_ptr_with_offset(
            &tag_memory_location.location,
            temp_payload_reg.register(),
            &node,
            "put enum tag in place",
        );

        let payload_memory_location =
            target_memory_location.offset(layout_enum.payload_offset, u8_type());
        match b {
            EnumLiteralData::Nothing => {}
            EnumLiteralData::Tuple(expressions) => {
                let EnumVariantType::Tuple(tuple_type) = a else {
                    panic!();
                };
                self.emit_tuple_literal_into_memory(
                    &payload_memory_location,
                    &tuple_type.fields_in_order,
                    expressions,
                    ctx,
                    node,
                );
            }
            EnumLiteralData::Struct(sorted_expressions) => {
                let EnumVariantType::Struct(variant_struct_type) = a else {
                    panic!()
                };

                self.emit_anonymous_struct_into_memory(
                    &payload_memory_location,
                    &variant_struct_type.anon_struct,
                    sorted_expressions,
                    node,
                    ctx,
                );
            }
        }

        /*
           self.store_register_contents_to_memory(
               node,
               target_reg,
               layout_enum.payload_offset,
               temp_payload_reg.register(),
               "copy enum payload into target",
           );

        */

        self.temp_registers.restore_to_mark(hwm);
    }

    pub fn emit_scalar_rvalue_to_lvalue(
        &mut self,
        scalar_location: &ScalarMemoryLocation,
        expr: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        //debug_assert!(expr.ty.is_scalar());

        let scalar_register = self.emit_scalar_rvalue(expr, ctx);
        self.store_scalar_to_memory(
            scalar_location,
            scalar_register,
            &expr.node,
            "store scalar into memory location",
        );
    }

    pub(crate) fn store_scalar_to_memory(
        &mut self,
        scalar_lvalue_location: &ScalarMemoryLocation,
        source_scalar_reg: TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        match source_scalar_reg.ty.basic_type.kind {
            BasicTypeKind::U8 | BasicTypeKind::B8 => {
                self.builder.add_st8_using_ptr_with_offset(
                    &scalar_lvalue_location.location,
                    &source_scalar_reg,
                    node,
                    comment,
                );
            }
            BasicTypeKind::U16 => {
                self.builder.add_st16_using_ptr_with_offset(
                    &scalar_lvalue_location.location,
                    &source_scalar_reg,
                    node,
                    comment,
                );
            }
            BasicTypeKind::S32
            | BasicTypeKind::Fixed32
            | BasicTypeKind::U32
            | BasicTypeKind::InternalStringPointer => {
                // Strings are scalars for now
                self.builder.add_st32_using_ptr_with_offset(
                    &scalar_lvalue_location.location,
                    &source_scalar_reg,
                    node,
                    comment,
                );
            }
            _ => panic!("this is not a primitive {}", source_scalar_reg.ty),
        }
    }
}
