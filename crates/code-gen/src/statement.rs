use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::{Collection, Transformer};
use source_map_node::Node;
use swamp_semantic::{
    ArgumentExpression, BooleanExpression, Expression, ExpressionKind, ForPattern, Iterable,
};
use swamp_types::Type;
use swamp_vm_types::types::{Destination, TypedRegister};

impl CodeBuilder<'_> {
    pub fn emit_statement(&mut self, expr: &Expression, ctx: &Context) {
        debug_assert!(matches!(expr.ty, Type::Unit));
        let output_destination = Destination::new_unit();
        self.emit_expression(&output_destination, expr, ctx);
    }

    pub(crate) fn emit_for_loop(
        &mut self,
        destination: &Destination,
        node: &Node,
        for_pattern: &ForPattern,
        iterable: &Iterable,
        lambda_non_capturing_expr: &Expression,
        ctx: &Context,
    ) {
        // Add check if the collection is empty, to skip everything

        // get some kind of iteration pointer

        // check if it has reached its end

        let collection_type = &iterable.resolved_expression.ty();
        let hwm = self.temp_registers.save_mark();

        let collection_reg = self.emit_argument_expression(&iterable.resolved_expression, ctx);
        match collection_type {
            Type::Range(anon_struct_type) => {
                self.emit_for_loop_lambda(
                    destination,
                    node,
                    Collection::Range,
                    collection_reg.grab_rvalue(),
                    collection_type,
                    for_pattern,
                    lambda_non_capturing_expr,
                    ctx,
                );
            }
            Type::SliceView(element_type) => {
                self.emit_for_loop_lambda(
                    destination,
                    node,
                    Collection::Vec,
                    collection_reg.grab_rvalue(),
                    collection_type,
                    for_pattern,
                    lambda_non_capturing_expr,
                    ctx,
                );
            }
            Type::String => {
                todo!();
            }
            Type::NamedStruct(named_type) => {
                if named_type.is_vec() {
                } else if named_type.is_map() {
                    self.emit_for_loop_lambda(
                        destination,
                        node,
                        Collection::Map,
                        collection_reg.grab_rvalue(),
                        collection_type,
                        for_pattern,
                        lambda_non_capturing_expr,
                        ctx,
                    );
                } else if named_type.is_range() {
                } else if named_type.is_stack() {
                    /*
                    self.emit_for_loop_lambda(
                        node,
                        Collection::Stack,
                        &gen_collection,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )?

                     */
                } else if named_type.is_grid() {
                    /*
                    self.emit_for_loop_lambda(
                        node,
                        Collection::Grid,
                        &gen_collection,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )?

                     */
                } else {
                    panic!("can not iterate this collection");
                }
            }
            _ => {
                panic!("can not iterate this collection");
            }
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    fn emit_for_loop_lambda(
        &mut self,
        target_reg: &Destination,
        node: &Node,
        collection: Collection,
        source_collection: &TypedRegister,
        source_collection_type: &Type,
        for_pattern: &ForPattern,
        lambda_expr: &Expression,

        ctx: &Context,
    ) {
        let variables = match for_pattern {
            ForPattern::Single(a) => vec![a.clone()],
            ForPattern::Pair(a, b) => vec![a.clone(), b.clone()],
        };

        let fake_lambda_kind = ExpressionKind::Lambda(variables, Box::from(lambda_expr.clone()));
        let fake_lambda_expr = ArgumentExpression::Expression(Expression {
            ty: lambda_expr.ty.clone(),
            node: node.clone(),
            kind: fake_lambda_kind,
        });

        self.emit_iterate_over_collection_with_lambda(
            target_reg,
            node,
            collection,
            Transformer::For,
            source_collection,
            source_collection_type,
            &fake_lambda_expr,
            ctx,
        );
    }

    pub(crate) fn emit_while_loop(
        &mut self,
        condition: &BooleanExpression,
        expression: &Expression,
        ctx: &Context,
    ) {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        //assert_eq!(target_reg.size.0, 0);

        let ip_for_condition = self.builder.position();

        let jump_on_false_condition = self.emit_condition_context(condition, ctx);

        // Expression is only for side effects
        self.emit_statement(expression, ctx);

        // Always jump to the condition again to see if it is true
        self.builder
            .add_jmp(ip_for_condition, &expression.node, "jmp to while condition");

        self.builder.patch_jump_here(jump_on_false_condition);
    }
}
