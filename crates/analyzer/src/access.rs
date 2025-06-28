/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::TypeContext;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{ArgumentExpression, Expression, ExpressionKind, Function};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    #[must_use]
    pub fn lookup_associated_function(
        &self,
        ty: &TypeRef,
        function_name: &str,
    ) -> Option<Function> {
        let x = self
            .shared
            .state
            .associated_impls
            .get_member_function(ty, function_name)
            .cloned();

        x.map(|found_func_ref| found_func_ref.as_ref().clone())
    }

    pub(crate) fn analyze_min_max_expr(
        &mut self,
        min_expr: &swamp_ast::Expression,
        max_expr: &swamp_ast::Expression,
    ) -> (Expression, Expression) {
        let int_type = self.shared.state.types.int();
        let context = TypeContext::new_argument(&int_type, false);

        let resolved_min = self.analyze_expression(min_expr, &context);
        let resolved_max = self.analyze_expression(max_expr, &context);

        (resolved_min, resolved_max)
    }

    /// # Errors
    ///
    /// #  Panics
    /// If core hasn't added the `Range` type
    pub fn analyze_range(
        &mut self,
        min_expr: &swamp_ast::Expression,
        max_expr: &swamp_ast::Expression,
        mode: &swamp_ast::RangeMode,
        ast_node: &swamp_ast::Node,
    ) -> Expression {
        let (min, max) = self.analyze_min_max_expr(min_expr, max_expr);

        let range_type_ref = self
            .shared
            .core_symbol_table
            .get_struct("Range")
            .unwrap()
            .clone();

        let range_type = self.shared.state.types.range(range_type_ref);

        let is_inclusive = matches!(mode, swamp_ast::RangeMode::Inclusive);

        let bool_expr_kind = ExpressionKind::BoolLiteral(is_inclusive);
        let bool_type = self.shared.state.types.bool();
        let bool_expr = self.create_expr(bool_expr_kind, bool_type, ast_node);

        let call_kind = ExpressionKind::IntrinsicCallEx(
            IntrinsicFunction::RangeInit,
            Vec::from(&[
                ArgumentExpression::Expression(min),
                ArgumentExpression::Expression(max),
                ArgumentExpression::Expression(bool_expr),
            ]), //ast_node,
                //&range_type,
        );

        self.create_expr(call_kind, range_type, ast_node)
    }
}
