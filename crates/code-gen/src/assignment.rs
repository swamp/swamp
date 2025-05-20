use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{Expression, ExpressionKind, Literal};
use swamp_vm_types::types::{BasicTypeKind, TypedRegister};
use swamp_vm_types::{AggregateMemoryLocation, ScalarMemoryLocation};

impl CodeBuilder<'_> {
    pub fn emit_aggregate_rvalue_to_lvalue(
        &mut self,
        location: &AggregateMemoryLocation,
        expr: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        match &expr.kind {
            ExpressionKind::AnonymousStructLiteral(anon_struct) => {
                self.emit_anonymous_struct_literal_into_memory_location(
                    location,
                    anon_struct,
                    &expr.ty,
                    &expr.node,
                    comment,
                    ctx,
                );
            }
            ExpressionKind::Option(maybe_option) => {
                self.emit_option_expression(location, &expr.node, maybe_option.as_deref(), ctx)
            }

            ExpressionKind::Literal(basic_literal) => match basic_literal {
                Literal::EnumVariantLiteral(_, _, _) => {}
                Literal::TupleLiteral(_, _) => {}
                Literal::Slice(_, _) => {}
                Literal::SlicePair(_, _) => {}
                _ => todo!(),
            },

            _ => {}
        }
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
