/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::transformer::{Collection, Transformer};
use source_map_node::Node;
use swamp_semantic::{BooleanExpression, Expression, ForPattern, Iterable};
use swamp_types::TypeKind;
use swamp_types::TypeRef;
use swamp_vm_types::types::{Destination, TypedRegister};

impl CodeBuilder<'_> {
    pub fn emit_statement(&mut self, expr: &Expression, ctx: &Context) {
        debug_assert!(matches!(&*expr.ty.kind, TypeKind::Unit | TypeKind::Never));
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

        let collection_type = &iterable.resolved_expression.ty;
        let hwm = self.temp_registers.save_mark();

        // Check if any of the lambda variables are mutable
        // If so, we need persistent storage for the collection (not temporary)
        let variables_are_mutable = match for_pattern {
            ForPattern::Single(var) => var.is_mutable(),
            ForPattern::Pair(var1, var2) => var1.is_mutable() || var2.is_mutable(),
        };
        let allow_temporary = !variables_are_mutable;

        let collection_ptr_reg = self.emit_scalar_rvalue_or_pointer_to_temporary(
            &iterable.resolved_expression,
            ctx,
            allow_temporary,
        );
        let underlying_collection = collection_type;
        match &*underlying_collection.kind {
            TypeKind::Range(_range_struct_ref) => {
                self.emit_for_loop_lambda(
                    destination,
                    node,
                    Collection::Range,
                    &collection_ptr_reg,
                    collection_type,
                    for_pattern,
                    lambda_non_capturing_expr,
                    ctx,
                );
            }

            TypeKind::StackStorage(_element_type, _)
            | TypeKind::StackView(_element_type)
            | TypeKind::QueueStorage(_element_type, _)
            | TypeKind::QueueView(_element_type)
            | TypeKind::DynamicLengthVecView(_element_type)
            | TypeKind::VecStorage(_element_type, ..)
            | TypeKind::FixedCapacityAndLengthArray(_element_type, _)
            | TypeKind::SliceView(_element_type) => {
                self.emit_for_loop_lambda(
                    destination,
                    node,
                    Collection::Vec,
                    &collection_ptr_reg,
                    collection_type,
                    for_pattern,
                    lambda_non_capturing_expr,
                    ctx,
                );
            }
            TypeKind::SparseStorage(element_type, _) | TypeKind::SparseView(element_type) => {
                self.emit_for_loop_lambda(
                    destination,
                    node,
                    Collection::Sparse,
                    &collection_ptr_reg,
                    collection_type,
                    for_pattern,
                    lambda_non_capturing_expr,
                    ctx,
                );
            }
            TypeKind::DynamicLengthMapView(_key, _value)
            | TypeKind::MapStorage(_key, _value, ..) => {
                self.emit_for_loop_lambda(
                    destination,
                    node,
                    Collection::Map,
                    &collection_ptr_reg,
                    collection_type,
                    for_pattern,
                    lambda_non_capturing_expr,
                    ctx,
                );
            }
            TypeKind::StringView(..) => {
                self.emit_for_loop_lambda(
                    destination,
                    node,
                    Collection::String,
                    &collection_ptr_reg,
                    collection_type,
                    for_pattern,
                    lambda_non_capturing_expr,
                    ctx,
                );
            }

            _ => {
                panic!("can not iterate this collection {underlying_collection}");
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
        source_collection_type: &TypeRef,
        for_pattern: &ForPattern,
        lambda_expr: &Expression,
        ctx: &Context,
    ) {
        let variables = match for_pattern {
            ForPattern::Single(a) => vec![a.clone()],
            ForPattern::Pair(a, b) => vec![a.clone(), b.clone()],
        };

        self.emit_iterate_over_collection_with_lambda(
            target_reg,
            node,
            collection,
            Transformer::For,
            source_collection,
            (variables, lambda_expr),
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
