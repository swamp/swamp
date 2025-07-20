/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod access;
mod attributes;
pub mod call;
pub mod constant;
pub mod context;
pub mod def;
pub mod literal;
pub mod operator;
pub mod pattern;
pub mod prelude;
pub mod shared;
mod structure;
mod to_string;
pub mod types;
pub mod variable;

use crate::call::MaybeBorrowMutRefExpression;
use crate::context::TypeContext;
use crate::shared::SharedState;
use crate::to_string::create_expr_resolved;
use crate::types::TypeAnalyzeContext;
use seq_map::SeqMap;
use source_map_cache::SourceMap;
use source_map_node::{FileId, Node, Span};
use std::mem::take;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use std::str::{FromStr, ParseBoolError};
use swamp_ast::{GenericParameter, QualifiedTypeIdentifier};
use swamp_modules::prelude::*;
use swamp_modules::symtbl::SymbolTableRef;
use swamp_semantic::prelude::*;
use swamp_semantic::{
    ArgumentExpression, BinaryOperatorKind, BlockScope, BlockScopeMode, FunctionScopeState,
    GridType, InternalMainExpression, LocationAccess, LocationAccessKind, MapType,
    MutableReferenceKind, NormalPattern, Postfix, PostfixKind, ScopeInfo, SingleLocationExpression,
    SliceViewType, SparseType, TargetAssignmentLocation, TypeWithMut, VariableType, VecType,
    WhenBinding,
};
use swamp_semantic::{StartOfChain, StartOfChainKind};
use swamp_types::prelude::*;
use swamp_types::{Type, TypeKind};
use tracing::error;
/*
           swamp_ast::Postfix::NoneCoalescingOperator(default_expr) => {
                   previous_was_optional_chaining = false;

                   let (unwrapped_type, optional_type) = if let TypeKind::Optional(unwrapped_type) =
                       &*tv.resolved_type.kind
                   {
                       // Standalone mode: tv.resolved_type is already optional
                       (unwrapped_type, tv.resolved_type.clone())
                   } else if uncertain {
                       // Uncertain mode: tv.resolved_type is unwrapped, need to create optional type
                       let optional_type = self.shared.state.types.optional(&tv.resolved_type);
                       (&tv.resolved_type, optional_type)
                   } else {
                       return self.create_err(ErrorKind::CanNotNoneCoalesce, &default_expr.node);
                   };

                   let unwrapped_type_context = TypeContext::new_argument(unwrapped_type, false);
                   let resolved_default_expr =
                       self.analyze_expression(default_expr, &unwrapped_type_context);
                   self.add_postfix(
                       &mut suffixes,
                       PostfixKind::NoneCoalescingOperator(resolved_default_expr, optional_type, uncertain),
                       (*unwrapped_type).clone(),
                       &default_expr.node,
                   );
                   tv.resolved_type = (*unwrapped_type).clone();
                   uncertain = false; // the chain is safe, because this will always solve None
               }
*/

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum AssignmentMode {
    OwnedValue,
    CopyBlittable,
    CopySharedPtr,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LocationSide {
    Lhs,
    Mutable,
    Rhs,
}

#[derive(Debug)]
pub enum StartOfChainBase {
    FunctionReference(Function),
    Variable(VariableRef),
}
#[derive(Debug)]
pub struct Program {
    pub state: ProgramState,
    pub modules: Modules,
    pub default_symbol_table: SymbolTable,
}

impl Default for Program {
    fn default() -> Self {
        Self::new(ProgramState::new(), Modules::new(), SymbolTable::new(&[]))
    }
}

impl Program {
    #[must_use]
    pub const fn new(
        state: ProgramState,
        modules: Modules,
        default_symbol_table: SymbolTable,
    ) -> Self {
        Self {
            state,
            modules,
            default_symbol_table,
        }
    }
}

#[must_use]
pub const fn convert_span(without: &swamp_ast::SpanWithoutFileId, file_id: FileId) -> Span {
    Span {
        file_id,
        offset: without.offset,
        length: without.length,
    }
}

pub struct Analyzer<'a> {
    pub shared: SharedState<'a>,
    scope: ScopeInfo,
    global: FunctionScopeState,
    module_path: Vec<String>,
}

impl<'a> Analyzer<'a> {
    pub fn new(
        state: &'a mut ProgramState,
        modules: &'a Modules,
        core_symbol_table: SymbolTableRef,
        source_map: &'a SourceMap,
        module_path: &[String],
        file_id: FileId,
    ) -> Self {
        let shared = SharedState {
            state,
            lookup_table: SymbolTable::new(&[]),
            definition_table: SymbolTable::new(module_path),
            modules,
            core_symbol_table,
            source_map,
            file_id,
        };


        let mut analyzer = Self {
            scope: ScopeInfo::default(),
            global: FunctionScopeState::new(),
            shared,
            module_path: module_path.to_vec(),
        };

        // Hack
        let unit_type = analyzer.shared.state.types.unit();
        analyzer.add_default_functions(&unit_type, &swamp_ast::Node::default());

        analyzer
    }

    // TODO: Not happy about this construct of a start function, should be a separate struct.
    fn start_function(&mut self) {
        self.global.block_scope_stack = take(&mut self.scope.active_scope.block_scope_stack);
        self.scope = ScopeInfo::default();
    }

    fn stop_function(&mut self) {
        self.scope.active_scope.block_scope_stack = take(&mut self.global.block_scope_stack);
    }

    fn analyze_if_expression(
        &mut self,
        condition: &swamp_ast::Expression,
        true_expression: &swamp_ast::Expression,
        maybe_false_expression: Option<&swamp_ast::Expression>,
        context: &TypeContext,
    ) -> Expression {
        let resolved_condition = self.analyze_bool_argument_expression(condition);

        let branch_context = context;

        let true_expr = self.analyze_expression(true_expression, branch_context);
        let resolved_true = Box::new(true_expr);

        let mut detected = context.expected_type.cloned();
        if detected.is_none() {
            detected = Some(resolved_true.ty.clone());
        }

        // Analyze the false branch if it exists
        let else_statements = if let Some(false_expression) = maybe_false_expression {
            let else_context =
                branch_context.with_expected_type(detected.as_ref(), context.has_lvalue_target);
            let else_expr = self.analyze_expression(false_expression, &else_context);
            if detected.is_none() {
                detected = Some(else_expr.ty.clone());
            }

            Some(Box::new(else_expr))
        } else {
            None
        };

        self.create_expr(
            ExpressionKind::If(resolved_condition, resolved_true, else_statements),
            detected.unwrap(),
            &condition.node,
        )
    }

    fn get_text(&self, ast_node: &swamp_ast::Node) -> &str {
        let span = Span {
            file_id: self.shared.file_id,
            offset: ast_node.span.offset,
            length: ast_node.span.length,
        };
        self.shared.source_map.get_span_source(
            self.shared.file_id,
            span.offset as usize,
            span.length as usize,
        )
    }

    fn get_text_resolved(&self, resolved_node: &Node) -> &str {
        let span = Span {
            file_id: self.shared.file_id,
            offset: resolved_node.span.offset,
            length: resolved_node.span.length,
        };
        self.shared.source_map.get_span_source(
            self.shared.file_id,
            span.offset as usize,
            span.length as usize,
        )
    }

    fn get_path(&self, ident: &swamp_ast::QualifiedTypeIdentifier) -> (Vec<String>, String) {
        let path = ident
            .module_path
            .as_ref()
            .map_or_else(Vec::new, |found_path| {
                let mut v = Vec::new();
                for p in &found_path.0 {
                    v.push(self.get_text(p).to_string());
                }
                v
            });
        (path, self.get_text(&ident.name.0).to_string())
    }

    fn analyze_return_type(&mut self, function: &swamp_ast::Function) -> TypeRef {
        let ast_return_type = match function {
            swamp_ast::Function::Internal(x) => &x.declaration.return_type,
            swamp_ast::Function::External(_, x) => &x.return_type,
        };

        match ast_return_type {
            None => self.shared.state.types.unit(),
            Some(x) => self.analyze_type(x, &TypeAnalyzeContext::default()),
        }
    }

    fn analyze_function_body_expression(
        &mut self,
        expression: &swamp_ast::Expression,
        return_type: &TypeRef,
    ) -> Expression {
        let context = TypeContext::new_function(return_type);

        self.analyze_expression(expression, &context)
    }

    fn analyze_maybe_type(&mut self, maybe_type: Option<&swamp_ast::Type>) -> TypeRef {
        match maybe_type {
            None => self.shared.state.types.unit(),
            Some(ast_type) => self.analyze_type(ast_type, &TypeAnalyzeContext::default()),
        }
    }

    fn analyze_for_pattern(
        &mut self,
        pattern: &swamp_ast::ForPattern,
        key_type: Option<&TypeRef>,
        value_type: &TypeRef,
    ) -> ForPattern {
        match pattern {
            swamp_ast::ForPattern::Single(var) => {
                let variable_ref = self.create_local_variable(
                    &var.identifier,
                    Option::from(&var.is_mut),
                    value_type,
                    false,
                );
                ForPattern::Single(variable_ref)
            }
            swamp_ast::ForPattern::Pair(first, second) => {
                let found_key = key_type.unwrap();
                let first_var_ref = self.create_local_variable(
                    &first.identifier,
                    Option::from(&first.is_mut),
                    found_key,
                    false,
                );
                let second_var_ref = self.create_local_variable(
                    &second.identifier,
                    Option::from(&second.is_mut),
                    value_type,
                    false,
                );
                ForPattern::Pair(first_var_ref, second_var_ref)
            }
        }
    }
    const MAX_PARAMETER_COUNT: usize = 6; // NOTE: Must be same or lower than the ABI allows (so it doesn't fail in codegen)
    fn analyze_parameters(
        &mut self,
        parameters: &Vec<swamp_ast::Parameter>,
    ) -> Vec<TypeForParameter> {
        let mut resolved_parameters = Vec::new();
        if parameters.len() > Self::MAX_PARAMETER_COUNT {
            self.add_err(
                ErrorKind::TooManyParameters {
                    encountered: parameters.len(),
                    allowed: Self::MAX_PARAMETER_COUNT,
                },
                &parameters[0].variable.name,
            );
            return vec![];
        }

        let allow_ephemeral = TypeAnalyzeContext::new_parameter_context();
        for parameter in parameters {
            let param_type = self.analyze_type(&parameter.param_type, &allow_ephemeral);
            if !param_type.allowed_as_parameter_type() {
                self.add_err(
                    ErrorKind::ParameterTypeCanNotBeStorage(param_type),
                    &parameter.variable.name,
                );
                continue;
            }
            resolved_parameters.push(TypeForParameter {
                name: self.get_text(&parameter.variable.name).to_string(),
                resolved_type: param_type,
                is_mutable: parameter.variable.is_mutable.is_some(),
                node: Some(ParameterNode {
                    is_mutable: self.to_node_option(Option::from(&parameter.variable.is_mutable)),
                    name: self.to_node(&parameter.variable.name),
                }),
            });
        }
        resolved_parameters
    }

    pub(crate) fn analyze_static_member_access(
        &mut self,
        named_type: &swamp_ast::QualifiedTypeIdentifier,
        member_name_node: &swamp_ast::Node,
    ) -> Option<Function> {
        if let Some(found_function) = self.special_static_member(named_type, member_name_node) {
            Some(found_function)
        } else {
            let some_type = self.analyze_named_type(named_type);

            let member_name = self.get_text(member_name_node);
            self.lookup_associated_function(&some_type, member_name)
        }
    }

    pub fn analyze_local_function_access(
        &mut self,
        qualified_func_name: &swamp_ast::QualifiedIdentifier,
    ) -> Option<Function> {
        let path = self.get_module_path(qualified_func_name.module_path.as_ref());
        let function_name = self.get_text(&qualified_func_name.name);

        if let Some(found_table) = self.shared.get_symbol_table(&path)
            && let Some(found_func) = found_table.get_function(function_name) {
            let (kind, signature) = match found_func {
                FuncDef::Internal(internal_fn) => (
                    Function::Internal(internal_fn.clone()),
                    &internal_fn.signature,
                ),
                FuncDef::External(external_fn) => (
                    Function::External(external_fn.clone()),
                    &external_fn.signature,
                ),
                // Can not have a reference to an intrinsic function
                FuncDef::Intrinsic(intrinsic_fn) => (
                    Function::Intrinsic(intrinsic_fn.clone()),
                    &intrinsic_fn.signature,
                ),
            };

            return Some(kind);
        }

        None
    }

    fn check_if_function_reference(
        &mut self,
        ast_expression: &swamp_ast::Expression,
    ) -> Option<Function> {
        match &ast_expression.kind {
            swamp_ast::ExpressionKind::StaticMemberFunctionReference(
                qualified_type_identifier,
                node,
            ) => self.analyze_static_member_access(qualified_type_identifier, node),
            swamp_ast::ExpressionKind::IdentifierReference(qualified_identifier) => {
                self.analyze_local_function_access(qualified_identifier)
            }
            _ => None,
        }
    }

    /// # Errors
    ///
    pub fn analyze_start_chain_expression_get_mutability(
        &mut self,
        ast_expression: &swamp_ast::Expression,
    ) -> Option<StartOfChainBase> {
        self.check_if_function_reference(ast_expression)
            .map_or_else(
                || {
                    if let swamp_ast::ExpressionKind::IdentifierReference(
                        found_qualified_identifier,
                    ) = &ast_expression.kind
                    {
                        self.try_find_variable(&found_qualified_identifier.name)
                            .map(StartOfChainBase::Variable)
                    } else {
                        None
                    }
                },
                |found_func_def| Some(StartOfChainBase::FunctionReference(found_func_def)),
            )
    }

    pub fn debug_expression(&self, expr: &swamp_ast::Expression, description: &str) {
        self.debug_line(expr.node.span.offset as usize, description);
    }

    pub fn debug_line(&self, offset: usize, description: &str) {
        let (line, col) = self
            .shared
            .source_map
            .get_span_location_utf8(self.shared.file_id, offset);

        let source_line = self
            .shared
            .source_map
            .get_source_line(self.shared.file_id, line);

        let source_path = self
            .shared
            .source_map
            .fetch_relative_filename(self.shared.file_id);
        let debug_line = format!("{source_path}:{line}:{col}> {}", source_line.unwrap());
        // info!(%debug_line, "source");
        // info!(?line, ?col, "source reference");
    }

    /// # Errors
    ///
    pub fn analyze_main_expression(
        &mut self,
        ast_expression: &swamp_ast::Expression,
    ) -> InternalMainExpression {
        self.start_function();

        let context = TypeContext::new_anything_argument(true); // function returns are per definition lvalue target for aggregates
        let analyzed_expr = self.analyze_expression(ast_expression, &context);
        let main_expr = InternalMainExpression {
            expression: analyzed_expr,
            scopes: self.scope.total_scopes.clone(),
            program_unique_id: self.shared.state.allocate_internal_function_id(),
        };

        self.stop_function();

        main_expr
    }

    fn analyze_maybe_ref_expression(
        &mut self,
        ast_expr: &swamp_ast::Expression,
    ) -> MaybeBorrowMutRefExpression {
        if let swamp_ast::ExpressionKind::UnaryOp(found_unary, ast_inner_expression) =
            &ast_expr.kind
            && let swamp_ast::UnaryOperator::BorrowMutRef(node) = found_unary {
            //let inner = self.analyze_expression(ast_inner_expression, context)?;
            let resolved_node = self.to_node(node);
            return MaybeBorrowMutRefExpression {
                ast_expression: *ast_inner_expression.clone(),
                has_borrow_mutable_reference: Some(resolved_node),
            };
        }

        MaybeBorrowMutRefExpression {
            ast_expression: ast_expr.clone(),
            has_borrow_mutable_reference: None,
        }
    }

    /// Check if an expression needs explicit storage and should return an error
    fn needs_storage_error(
        &self,
        expr: &Expression,
        is_function_call: bool,
        context: &TypeContext,
        ast_node: &swamp_ast::Node,
    ) -> bool {
        let is_constant = matches!(expr.kind, ExpressionKind::ConstantAccess(_));
        let is_variable = matches!(expr.kind, ExpressionKind::VariableAccess(_));

        // Return true if we need to create an error
        !context.has_lvalue_target
            && expr.ty.collection_view_that_needs_explicit_storage()
            && is_function_call
            && !is_constant
            && !is_variable
    }

    pub fn analyze_expression(
        &mut self,
        ast_expression: &swamp_ast::Expression,
        context: &TypeContext,
    ) -> Expression {
        //info!(?ast_expression, "analyze expression");
        //self.debug_expression(ast_expression, "analyze");
        let expr = self.analyze_expression_internal(ast_expression, context);

        if matches!(expr.kind, ExpressionKind::Error(_)) {
            return expr;
        }

        let encountered_type = expr.ty.clone();

        //info!(?expr, "analyze expression");
        let expr = if let Some(found_expected_type) = context.expected_type {
            let reduced_expected = found_expected_type;

            let reduced_encountered_type = encountered_type;

            if self
                .shared
                .state
                .types
                .compatible_with(reduced_expected, &reduced_encountered_type)
            {
                expr
            } else {
                self.types_did_not_match_try_late_coerce_expression(
                    expr,
                    reduced_expected,
                    &reduced_encountered_type,
                    &ast_expression.node,
                )
            }
        } else {
            expr
            //todo!()
            // TODO: self.coerce_unrestricted_type(&ast_expression.node, expr)?
        };

        // Only apply storage rule to function calls that return types needing explicit storage
        // Exclude literals, constants, and variables which already have storage or are constructed directly
        let is_function_call = Self::is_aggregate_function_call(ast_expression);

        if self.needs_storage_error(&expr, is_function_call, context, &ast_expression.node) {
            // We have no way to store it
            self.create_err(ErrorKind::NeedStorage, &ast_expression.node)
        } else {
            expr
        }
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression_internal(
        &mut self,
        ast_expression: &swamp_ast::Expression,
        context: &TypeContext,
    ) -> Expression {
        //info!(ty=%expression.ty, kind=?expression.kind,  "resolved expression");

        match &ast_expression.kind {
            // Lookups
            swamp_ast::ExpressionKind::PostfixChain(postfix_chain) => {
                self.analyze_postfix_chain(postfix_chain, context)
            }

            swamp_ast::ExpressionKind::VariableDefinition(
                variable,
                maybe_annotation,
                source_expression,
            ) => self.analyze_create_variable(
                variable,
                Option::from(maybe_annotation),
                source_expression,
            ),

            swamp_ast::ExpressionKind::VariableAssignment(variable, source_expression) => {
                self.analyze_variable_assignment(variable, source_expression)
            }

            swamp_ast::ExpressionKind::DestructuringAssignment(variables, expression) => {
                self.analyze_destructuring(&ast_expression.node, variables, expression)
            }

            swamp_ast::ExpressionKind::IdentifierReference(qualified_identifier) => {
                self.analyze_identifier(qualified_identifier)
            }

            swamp_ast::ExpressionKind::VariableReference(variable) => {
                self.analyze_variable_reference(&variable.name)
            }

            swamp_ast::ExpressionKind::ContextAccess => {
                todo!("context access is not done yet")
            }

            swamp_ast::ExpressionKind::StaticMemberFunctionReference(
                type_identifier,
                member_name,
            ) => {
                let debug_name = self.get_text(member_name);
                let type_name = self.get_text(&type_identifier.name.0);
                self.create_err(ErrorKind::CanNotHaveSeparateMemberFuncRef, member_name)
            }

            swamp_ast::ExpressionKind::ConstantReference(constant_identifier) => {
                self.analyze_constant_access(constant_identifier)
            }

            swamp_ast::ExpressionKind::Assignment(location, source) => {
                self.analyze_assignment(location, source)
            }

            swamp_ast::ExpressionKind::CompoundAssignment(target, op, source) => {
                self.analyze_assignment_compound(target, op, source)
            }

            // Operator
            swamp_ast::ExpressionKind::BinaryOp(resolved_a, operator, resolved_b) => {
                let Some((resolved_op, result_type)) =
                    self.analyze_binary_op(resolved_a, operator, resolved_b)
                else {
                    return self.create_err(ErrorKind::OperatorProblem, &operator.node);
                };

                self.create_expr(
                    ExpressionKind::BinaryOp(resolved_op),
                    result_type,
                    &ast_expression.node,
                )
            }

            swamp_ast::ExpressionKind::UnaryOp(operator, expression) => {
                if let swamp_ast::UnaryOperator::BorrowMutRef(_node) = operator {
                    let inner_expr =
                        self.analyze_to_location(expression, context, LocationSide::Rhs);
                    let ty = inner_expr.ty.clone();
                    self.create_expr(
                        ExpressionKind::BorrowMutRef(Box::from(inner_expr)),
                        ty,
                        &ast_expression.node,
                    )
                } else {
                    let Some((resolved_op, result_type)) =
                        self.analyze_unary_op(operator, expression)
                    else {
                        return self.create_err(ErrorKind::OperatorProblem, &ast_expression.node);
                    };
                    self.create_expr(
                        ExpressionKind::UnaryOp(resolved_op),
                        result_type,
                        &ast_expression.node,
                    )
                }
            }

            swamp_ast::ExpressionKind::Block(expressions) => {
                let (block, resulting_type) =
                    self.analyze_block(&ast_expression.node, context, expressions);
                self.create_expr(
                    ExpressionKind::Block(block),
                    resulting_type,
                    &ast_expression.node,
                )
            }

            swamp_ast::ExpressionKind::With(variable_bindings, expression) => {
                self.analyze_with_expr(context, variable_bindings, expression)
            }

            swamp_ast::ExpressionKind::When(variable_bindings, true_expr, else_expr) => {
                self.analyze_when_expr(context, variable_bindings, true_expr, else_expr.as_deref())
            }

            swamp_ast::ExpressionKind::InterpolatedString(string_parts) => {
                self.analyze_interpolated_string_lowering(&ast_expression.node, string_parts)
            }

            // Creation
            swamp_ast::ExpressionKind::NamedStructLiteral(struct_identifier, fields, has_rest) => {
                self.analyze_named_struct_literal(struct_identifier, fields, *has_rest, context)
            }

            swamp_ast::ExpressionKind::AnonymousStructLiteral(fields, rest_was_specified) => self
                .analyze_anonymous_struct_literal(
                    &ast_expression.node,
                    fields,
                    *rest_was_specified,
                    context,
                ),

            swamp_ast::ExpressionKind::ContextAccess => {
                todo!("lone dot not implemented yet")
            }

            swamp_ast::ExpressionKind::Range(min_value, max_value, range_mode) => {
                self.analyze_range(min_value, max_value, range_mode, &ast_expression.node)
            }

            swamp_ast::ExpressionKind::ForLoop(pattern, iterable_expression, statements) => {
                if pattern.is_key_variable_mut() {
                    return self.create_err(
                        ErrorKind::KeyVariableNotAllowedToBeMutable,
                        &ast_expression.node,
                    );
                }
                let resolved_iterator =
                    self.analyze_iterable(pattern.is_value_mut(), &iterable_expression.expression);

                self.push_block_scope("for_loop");
                let pattern = self.analyze_for_pattern(
                    pattern,
                    resolved_iterator.key_type.as_ref(),
                    &resolved_iterator.value_type,
                );
                let resolved_statements = self.analyze_expression(statements, context);
                self.pop_block_scope("for_loop");
                let resolved_type = resolved_statements.ty.clone();
                self.create_expr(
                    ExpressionKind::ForLoop(
                        pattern,
                        resolved_iterator,
                        Box::from(resolved_statements),
                    ),
                    resolved_type,
                    &ast_expression.node,
                )
            }

            swamp_ast::ExpressionKind::WhileLoop(expression, statements) => {
                let condition = self.analyze_bool_argument_expression(expression);
                //self.push_block_scope("while_loop");
                let resolved_statements = self.analyze_expression(statements, context);
                let resolved_type = resolved_statements.ty.clone();
                //self.pop_block_scope("while_loop");

                self.create_expr(
                    ExpressionKind::WhileLoop(condition, Box::from(resolved_statements)),
                    resolved_type,
                    &ast_expression.node,
                )
            }

            swamp_ast::ExpressionKind::If(expression, true_expression, maybe_false_expression) => {
                self.analyze_if_expression(
                    expression,
                    true_expression,
                    maybe_false_expression.as_deref(),
                    context,
                )
            }

            swamp_ast::ExpressionKind::Match(expression, arms) => {
                let (match_expr, return_type) = self.analyze_match(expression, context, arms);
                self.create_expr(
                    ExpressionKind::Match(match_expr),
                    return_type,
                    &ast_expression.node,
                )
            }
            swamp_ast::ExpressionKind::Guard(guard_expressions) => {
                self.analyze_guard(&ast_expression.node, context, guard_expressions)
            }

            swamp_ast::ExpressionKind::Lambda(variables, expression) => {
                self.analyze_lambda(&ast_expression.node, variables, expression, context)
            }
            swamp_ast::ExpressionKind::Error => {
                self.create_err(ErrorKind::UnexpectedType, &ast_expression.node)
            }
            swamp_ast::ExpressionKind::Literal(literal) => {
                self.analyze_complex_literal_to_expression(ast_expression, literal, context)
            } //self.create_err(ErrorKind::UnexpectedType, &ast_expression.node),
        }
    }

    fn get_struct_type(
        &mut self,
        qualified_type_identifier: &swamp_ast::QualifiedTypeIdentifier,
    ) -> NamedStructType {
        let maybe_struct_type = self.analyze_named_type(qualified_type_identifier);
        if let TypeKind::NamedStruct(struct_type) = &*maybe_struct_type.kind {
            struct_type.clone()
        } else {
            self.add_err(
                // For other TypeRef variants that are not Struct
                ErrorKind::UnknownStructTypeReference,
                &qualified_type_identifier.name.0,
            );

            NamedStructType {
                name: Default::default(),
                module_path: vec![],
                assigned_name: String::new(),
                anon_struct_type: self.types().unit(),
                instantiated_type_parameters: vec![],
            }
        }
    }

    pub(crate) fn analyze_named_type(
        &mut self,
        type_name_to_find: &swamp_ast::QualifiedTypeIdentifier,
    ) -> TypeRef {
        let (path, name) = self.get_path(type_name_to_find);
        let mut analyzed_type_parameters = Vec::new();

        if let Some(found) =
            self.analyze_special_named_type(&path, &name, &type_name_to_find.generic_params)
        {
            return found;
        }

        for analyzed_type in &type_name_to_find.generic_params {
            let ty = self.analyze_type(analyzed_type.get_type(), &TypeAnalyzeContext::default());

            analyzed_type_parameters.push(ty);
        }

        let symbol = {
            if let Some(symbol_table) = self.shared.get_symbol_table(&path) {
                if let Some(x) = symbol_table.get_symbol(&name) {
                    x
                } else {
                    self.add_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0);
                    return self.types().unit();
                }
            } else {
                self.add_err(ErrorKind::UnexpectedType, &type_name_to_find.name.0);
                return self.types().unit();
            }
        };

        if analyzed_type_parameters.is_empty() {
            match &symbol {
                Symbol::Type(base_type) => base_type.clone(),
                Symbol::Alias(alias_type) => alias_type.ty.clone(),
                _ => {
                    self.add_err(ErrorKind::UnexpectedType, &type_name_to_find.name.0);
                    self.types().unit()
                }
            }
        } else {
            panic!("unknown type")
        }
    }

    fn create_default_value_for_type(
        &mut self,
        node: &swamp_ast::Node,
        field_type: &TypeRef,
    ) -> Option<Expression> {
        let kind = match &*field_type.kind {
            TypeKind::Tuple(tuple_type_ref) => {
                let mut expressions = Vec::new();
                for resolved_type in tuple_type_ref {
                    let expr = self.create_default_value_for_type(node, resolved_type)?;
                    expressions.push(expr);
                }
                ExpressionKind::TupleLiteral(expressions)
            }

            TypeKind::NamedStruct(_struct_ref) => {
                // Only call default() if the type actually has a default implementation
                if self
                    .shared
                    .state
                    .associated_impls
                    .get_member_function(field_type, "default")
                    .is_some()
                {
                    self.create_default_static_call(node, field_type)
                } else {
                    // This type doesn't have a default implementation, skip it
                    return None;
                }
            }

            _ => {
                // For primitives and other types without default implementations, skip them
                return None;
            }
        };

        Some(self.create_expr(kind, field_type.clone(), node))
    }

    fn create_static_member_call(
        &mut self,
        function_name: &str,
        arguments: &[ArgumentExpression],
        node: &swamp_ast::Node,
        ty: &TypeRef,
    ) -> ExpressionKind {
        self.lookup_associated_function(ty, function_name)
            .map_or_else(
                || {
                    self.create_err(
                        ErrorKind::NoAssociatedFunction(ty.clone(), function_name.to_string()),
                        node,
                    )
                        .kind
                },
                |function| {
                    let Function::Internal(internal_function) = &function else {
                        panic!("only allowed for internal functions");
                    };

                    ExpressionKind::InternalCall(internal_function.clone(), arguments.to_vec())
                },
            )
    }

    fn create_static_member_intrinsic_call(
        &mut self,
        function_name: &str,
        arguments: &[ArgumentExpression],
        node: &swamp_ast::Node,
        ty: &TypeRef,
    ) -> ExpressionKind {
        // Look up the function first to avoid closure borrowing issues
        if let Some(function) = self.lookup_associated_function(ty, function_name) {
            let Function::Internal(internal_function) = &function else {
                panic!("only allowed for internal functions");
            };

            if let Some((intrinsic_fn, _)) =
                Self::extract_single_intrinsic_call(&internal_function.body)
            {
                ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments.to_vec())
            } else {
                self.create_err(
                    ErrorKind::NoAssociatedFunction(ty.clone(), function_name.to_string()),
                    node,
                )
                    .kind
            }
        } else {
            self.create_err(
                ErrorKind::NoAssociatedFunction(ty.clone(), function_name.to_string()),
                node,
            )
                .kind
        }
    }

    fn create_default_static_call(
        &mut self,
        node: &swamp_ast::Node,
        ty: &TypeRef,
    ) -> ExpressionKind {
        self.create_static_member_call("default", &[], node, ty)
    }

    fn add_postfix(
        &mut self,
        vec: &mut Vec<Postfix>,
        kind: PostfixKind,
        ty: TypeRef,
        node: &swamp_ast::Node,
    ) {
        let resolved_node = self.to_node(node);
        let postfix = Postfix {
            node: resolved_node,
            ty,
            kind,
        };

        vec.push(postfix);
    }


    /// # Panics
    ///
    /// # Errors
    ///
    pub fn analyze_struct_field(
        &mut self,
        field_name: &swamp_ast::Node,
        tv: &TypeRef,
    ) -> (AnonymousStructType, usize, TypeRef) {
        let field_name_str = self.get_text(field_name).to_string();

        let anon_struct_ref = match &*tv.kind {
            TypeKind::NamedStruct(struct_type) => struct_type.anon_struct_type.clone(),
            TypeKind::AnonymousStruct(anon_struct) => {
                // Create a TypeRef from the AnonymousStructType
                self.shared
                    .state
                    .types
                    .anonymous_struct(anon_struct.clone())
            }
            TypeKind::Range(range_struct_ref) => {
                // Extract NamedStructType from TypeRef, then AnonymousStructType from that
                if let TypeKind::NamedStruct(named_struct) = &*range_struct_ref.kind {
                    named_struct.anon_struct_type.clone()
                } else {
                    self.add_err(ErrorKind::UnknownStructField, field_name);
                    // Return fallback values
                    return (
                        AnonymousStructType::new(SeqMap::new()),
                        0,
                        self.shared.state.types.unit(),
                    );
                }
            }
            _ => {
                self.add_err(ErrorKind::UnknownStructField, field_name);
                // Return fallback values
                return (
                    AnonymousStructType::new(SeqMap::new()),
                    0,
                    self.shared.state.types.unit(),
                );
            }
        };

        // Extract the AnonymousStructType from the TypeRef
        if let TypeKind::AnonymousStruct(anon_struct) = &*anon_struct_ref.kind
            && let Some(found_field) = anon_struct.field_name_sorted_fields.get(&field_name_str) {
            let index = anon_struct
                .field_name_sorted_fields
                .get_index(&field_name_str)
                .expect("checked earlier");

            return (anon_struct.clone(), index, found_field.field_type.clone());
        }

        self.add_err(ErrorKind::UnknownStructField, field_name);
        // Return fallback values
        (
            AnonymousStructType::new(SeqMap::new()),
            0,
            self.shared.state.types.unit(),
        )
    }

    pub fn analyze_static_call(
        &mut self,
        ast_node: &swamp_ast::Node,
        maybe_associated_to_type: Option<TypeRef>,
        func_def: Function,
        maybe_generic_arguments: &Option<Vec<swamp_ast::GenericParameter>>,
        arguments: &[swamp_ast::Expression],
        context: &TypeContext,
    ) -> Expression {
        let signature = func_def.signature().clone();

        let analyzed_arguments =
            self.analyze_and_verify_parameters(ast_node, &signature.parameters, arguments);

        let expr_kind = match &func_def {
            Function::Internal(internal) => {
                ExpressionKind::InternalCall(internal.clone(), analyzed_arguments)
            }
            Function::External(host) => ExpressionKind::HostCall(host.clone(), analyzed_arguments),
            Function::Intrinsic(intrinsic) => {
                ExpressionKind::IntrinsicCallEx(intrinsic.intrinsic.clone(), analyzed_arguments)
            }
        };

        let expr = self.create_expr(expr_kind, signature.return_type.clone(), ast_node);

        // Check if function returns a type that needs explicit storage and we don't have an lvalue target
        if self.needs_storage_error(&expr, true, context, ast_node) {
            return self.create_err(ErrorKind::NeedStorage, ast_node);
        }

        expr
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_postfix_chain(
        &mut self,
        chain: &swamp_ast::PostfixChain,
        context: &TypeContext,
    ) -> Expression {
        let maybe_start_of_chain_base =
            self.analyze_start_chain_expression_get_mutability(&chain.base);

        let mut start_index = 0;

        let start_of_chain_kind = if let Some(start_of_chain_base) = maybe_start_of_chain_base {
            //trace!(?start_of_chain_base, "start of postfix chain");
            match start_of_chain_base {
                StartOfChainBase::FunctionReference(func_def) => {
                    // In this language version, it is not allowed to provide references to functions
                    // So it must mean that this is a function call
                    if let swamp_ast::Postfix::FunctionCall(
                        ast_node,
                        maybe_generic_arguments,
                        arguments,
                    ) = &chain.postfixes[0]
                    {
                        start_index = 1;
                        let call_expr = self.analyze_static_call(
                            ast_node,
                            None,
                            func_def,
                            maybe_generic_arguments,
                            arguments,
                            context,
                        );
                        if chain.postfixes.len() == 1 {
                            return call_expr;
                        }
                        StartOfChainKind::Expression(Box::from(call_expr))
                    } else {
                        panic!("must be a normal function call")
                    }
                }
                StartOfChainBase::Variable(var) => StartOfChainKind::Variable(var),
            }
        } else {
            let ctx = TypeContext::new_anything_argument(true); // we will allocate space for the starting point
            StartOfChainKind::Expression(Box::from(self.analyze_expression(&chain.base, &ctx)))
        };

        let start_of_chain_node = self.to_node(&chain.base.node);

        let start_of_chain = StartOfChain {
            kind: start_of_chain_kind.clone(),
            node: start_of_chain_node,
        };

        let mut tv = TypeWithMut {
            resolved_type: start_of_chain_kind.ty(),
            is_mutable: start_of_chain_kind.is_mutable(),
        };

        let mut uncertain = false;
        let mut previous_was_optional_chaining = false;

        let mut suffixes = Vec::new();

        for (index, item) in chain.postfixes[start_index..].iter().enumerate() {
            let is_last = index == chain.postfixes[start_index..].len() - 1;

            // Check if this operator is invalid after optional chaining
            if previous_was_optional_chaining {
                match item {
                    swamp_ast::Postfix::FieldAccess(_)
                    | swamp_ast::Postfix::MemberCall(_, _, _)
                    | swamp_ast::Postfix::Subscript(_)
                    | swamp_ast::Postfix::SubscriptTuple(_, _) => {
                        // These are valid after optional chaining
                    }
                    swamp_ast::Postfix::OptionalChainingOperator(node) => {
                        return self
                            .create_err(ErrorKind::InvalidOperatorAfterOptionalChaining, node);
                    }
                    swamp_ast::Postfix::FunctionCall(node, _, _) => {
                        return self
                            .create_err(ErrorKind::InvalidOperatorAfterOptionalChaining, node);
                    }
                }
            }

            match item {
                /*
                swamp_ast::Postfix::AdvancedFunctionCall(..) => {
                    todo!("AdvancedFunctionCall")
                }

                 */
                swamp_ast::Postfix::FieldAccess(field_name) => {
                    previous_was_optional_chaining = false;
                    let (struct_type_ref, index, return_type) =
                        self.analyze_struct_field(&field_name.clone(), &tv.resolved_type);
                    let struct_type_type_ref = self
                        .shared
                        .state
                        .types
                        .anonymous_struct(struct_type_ref.clone());
                    self.add_postfix(
                        &mut suffixes,
                        PostfixKind::StructField(struct_type_type_ref, index),
                        return_type.clone(),
                        field_name,
                    );

                    tv.resolved_type = return_type.clone();
                    // keep previous `is_mutable`
                }

                swamp_ast::Postfix::SubscriptTuple(col_expr, row_expr) => {
                    previous_was_optional_chaining = false;
                    let collection_type = tv.resolved_type.clone();
                    match &*collection_type.kind {
                        TypeKind::GridStorage(element_type, x, _) => {
                            let int_type = self.shared.state.types.int();
                            let unsigned_int_x_context =
                                TypeContext::new_argument(&int_type, false);
                            let unsigned_int_x_expression =
                                self.analyze_expression(col_expr, &unsigned_int_x_context);

                            let unsigned_int_y_context =
                                TypeContext::new_argument(&int_type, false);
                            let unsigned_int_y_expression =
                                self.analyze_expression(row_expr, &unsigned_int_y_context);

                            let vec_type = GridType {
                                element: element_type.clone(),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::GridSubscript(
                                    vec_type,
                                    unsigned_int_x_expression,
                                    unsigned_int_y_expression,
                                ),
                                element_type.clone(),
                                &col_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = element_type.clone();
                        }
                        _ => return self.create_err(ErrorKind::ExpectedTupleType, &chain.base.node),
                    }
                }

                swamp_ast::Postfix::Subscript(lookup_expr) => {
                    previous_was_optional_chaining = false;
                    let collection_type = tv.resolved_type.clone();

                    match &*collection_type.kind {
                        // Map lookups can involve a wide range of lookup (key) types, so handle the specifically
                        TypeKind::MapStorage(key_type, value_type, ..) | TypeKind::DynamicLengthMapView(key_type, value_type) => {
                            let (postfix, return_type) = self.analyze_map_subscript(key_type, value_type, lookup_expr);
                            suffixes.push(postfix);
                            tv.resolved_type = return_type;
                        }
                        _ => {
                            // If it is not a map lookup, it can only be with an (unsigned) int or a range
                            let anything_context = context.with_argument_anything();
                            let anything_expression =
                                self.analyze_expression(lookup_expr, &anything_context);

                            match &*anything_expression.ty.kind {
                                TypeKind::Int => {
                                    let (postfix, return_type) = self.analyze_subscript_int(collection_type, anything_expression);
                                    suffixes.push(postfix);
                                    tv.resolved_type = return_type;
                                }
                                TypeKind::Range(_range_type) => {
                                    let (postfix, return_type) = self.analyze_subscript_range(collection_type, anything_expression);
                                    suffixes.push(postfix);
                                    tv.resolved_type = return_type;
                                }
                                _ => {
                                    return self.create_err(ErrorKind::CanNotSubscriptWithThatType, &lookup_expr.node)
                                }
                            }
                        }
                    }
                }

                swamp_ast::Postfix::MemberCall(member_name, generic_arguments, ast_arguments) => {
                    previous_was_optional_chaining = false;
                    let member_name_str = self.get_text(member_name).to_string();
                    let underlying_type = tv.resolved_type;

                    let (kind, return_type) = self.analyze_member_call(
                        &underlying_type,
                        &member_name_str,
                        generic_arguments.clone(),
                        ast_arguments,
                        tv.is_mutable,
                        member_name,
                    );

                    self.add_postfix(&mut suffixes, kind, return_type.clone(), member_name);
                    tv.resolved_type = return_type.clone();
                    tv.is_mutable = false;
                }
                swamp_ast::Postfix::FunctionCall(node, _generic_arguments, arguments) => {
                    return self.create_err(ErrorKind::CanOnlyHaveFunctionCallAtStartOfPostfixChain, node);
                }

                swamp_ast::Postfix::OptionalChainingOperator(option_node) => {
                    if is_last {
                        return self.create_err(
                            ErrorKind::InvalidOperatorAfterOptionalChaining,
                            option_node,
                        );
                    }

                    if let TypeKind::Optional(unwrapped_type) = &*tv.resolved_type.kind {
                        uncertain = true;
                        self.add_postfix(
                            &mut suffixes,
                            PostfixKind::OptionalChainingOperator,
                            (*unwrapped_type).clone(),
                            option_node,
                        );
                        tv.resolved_type = (*unwrapped_type).clone();
                        previous_was_optional_chaining = true;
                    } else {
                        return self.create_err(ErrorKind::ExpectedOptional, option_node);
                    }
                }
            }
        }

        if uncertain {
            if let TypeKind::Optional(_) = &*tv.resolved_type.kind {} else {
                tv.resolved_type = self.shared.state.types.optional(&tv.resolved_type.clone());
            }
        }

        self.create_expr(
            ExpressionKind::PostfixChain(start_of_chain, suffixes),
            tv.resolved_type,
            &chain.base.node,
        )
    }

    fn analyze_bool_argument_expression(
        &mut self,
        expression: &swamp_ast::Expression,
    ) -> BooleanExpression {
        let bool_type = self.shared.state.types.bool();
        let bool_context = TypeContext::new_argument(&bool_type, false);
        let resolved_expression = self.analyze_expression(expression, &bool_context);
        let expr_type = resolved_expression.ty.clone();

        let bool_expression = match &*expr_type.kind {
            TypeKind::Bool => resolved_expression,
            TypeKind::Optional(_) => {
                let bool = self.types().bool();
                self.create_expr(
                    ExpressionKind::CoerceOptionToBool(Box::new(resolved_expression)),
                    bool,
                    &expression.node,
                )
            }
            _ => self.create_err(ErrorKind::ExpectedBooleanExpression, &expression.node),
        };

        BooleanExpression {
            expression: Box::from(bool_expression),
        }
    }

    /// Check if an expression is a function call that returns an aggregate type
    fn is_aggregate_function_call(expression: &swamp_ast::Expression) -> bool {
        match &expression.kind {
            swamp_ast::ExpressionKind::PostfixChain(chain) => {
                if chain.postfixes.is_empty() {
                    return false;
                }

                for postfix in &chain.postfixes {
                    if let swamp_ast::Postfix::FunctionCall(_, _, _) = postfix {
                        return true;
                    }
                }
                false
            }
            _ => false, // Other expressions (variables, literals, etc.) are not function calls
        }
    }

    fn analyze_iterable(
        &mut self,
        mut_requested_for_value_variable: Option<swamp_ast::Node>,
        expression: &swamp_ast::Expression,
    ) -> Iterable {
        // Only set has_lvalue_target=false for function calls that return aggregates
        // Variables/parameters already have storage, so they should have has_lvalue_target=true
        let has_lvalue_target = !Self::is_aggregate_function_call(expression);
        let any_context = TypeContext::new_anything_argument(has_lvalue_target);

        let resolved_expression = self.analyze_expression(expression, &any_context);

        let resolved_type = &resolved_expression.ty.clone();
        let int_type = Some(self.types().int());
        let (key_type, value_type): (Option<TypeRef>, TypeRef) = match &*resolved_type.kind {
            TypeKind::String(_, char) => (int_type, char.clone()),
            TypeKind::SliceView(element_type) => (int_type, element_type.clone()),
            TypeKind::VecStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::SparseStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::StackStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::StackView(element_type) => (int_type, element_type.clone()),
            TypeKind::QueueStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::QueueView(element_type) => (int_type, element_type.clone()),
            TypeKind::DynamicLengthVecView(element_type) => (int_type, element_type.clone()),
            TypeKind::SparseView(element_type) => (int_type, element_type.clone()),
            TypeKind::FixedCapacityAndLengthArray(element_type, _fixed_size) => {
                (int_type, element_type.clone())
            }
            TypeKind::DynamicLengthMapView(key_type, value_type)
            | TypeKind::MapStorage(key_type, value_type, _) => {
                (Some(key_type.clone()), value_type.clone())
            }
            TypeKind::Range(_) => (None, self.types().int()),
            _ => {
                error!(?resolved_type, "not an iterator");
                return Iterable {
                    key_type: None,
                    value_type: self.shared.state.types.unit(),
                    resolved_expression: Box::new(
                        self.create_err(ErrorKind::NotAnIterator, &expression.node),
                    ),
                };
            }
        };

        if mut_requested_for_value_variable.is_some() {
            // we check if we can get to a lvalue, otherwise it is not mutable:
            let _resulting_location =
                self.analyze_to_location(expression, &any_context, LocationSide::Mutable);
            // Note: mutable references are now handled through SingleLocationExpression
            // The value_type stays the same, mutability is tracked separately
        }

        Iterable {
            key_type,
            value_type,
            resolved_expression: Box::new(resolved_expression),
        }
    }

    fn analyze_argument_expressions(
        &mut self,
        expected_type: Option<&TypeRef>,
        context: &TypeContext,
        ast_expressions: &[swamp_ast::Expression],
    ) -> Vec<Expression> {
        let mut resolved_expressions = Vec::new();
        // Function arguments should not have lvalue targets by default - they need proper storage allocation
        let argument_expressions_context = TypeContext::new_unsure_argument(expected_type, false);

        for expression in ast_expressions {
            resolved_expressions
                .push(self.analyze_expression(expression, &argument_expressions_context));
        }
        resolved_expressions
    }

    fn analyze_block(
        &mut self,
        _node: &swamp_ast::Node,
        context: &TypeContext,
        ast_expressions: &[swamp_ast::Expression],
    ) -> (Vec<Expression>, TypeRef) {
        if ast_expressions.is_empty() {
            return (vec![], self.shared.state.types.unit());
        }

        self.push_block_scope("block");

        let mut resolved_expressions = Vec::with_capacity(ast_expressions.len());

        for expression in &ast_expressions[..ast_expressions.len() - 1] {
            let unit_type = self.shared.state.types.unit();
            let stmt_context =
                context.with_expected_type(Some(&unit_type), context.has_lvalue_target);
            let expr = self.analyze_expression(expression, &stmt_context);

            resolved_expressions.push(expr);
        }

        // Process the last expression - it determines the block's type
        let last_expr =
            self.analyze_expression(&ast_expressions[ast_expressions.len() - 1], context);
        let last_type = last_expr.ty.clone();
        resolved_expressions.push(last_expr);

        self.pop_block_scope("block");

        (resolved_expressions, last_type)
    }

    fn analyze_interpolated_string_lowering(
        &mut self,
        node: &swamp_ast::Node,
        string_parts: &[swamp_ast::StringPart],
    ) -> Expression {
        let mut last_expression: Option<Expression> = None;
        for part in string_parts {
            let created_expression = match part {
                swamp_ast::StringPart::Literal(string_node, processed_string) => {
                    let string_literal =
                        ExpressionKind::StringLiteral(processed_string.to_string());
                    let basic_literal = string_literal;
                    let string_type = self.shared.state.types.string();
                    self.create_expr(basic_literal, string_type, string_node)
                }
                swamp_ast::StringPart::Interpolation(expression, format_specifier) => {
                    let any_context = TypeContext::new_anything_argument(false); // String interpolation context has no lvalue target for storage

                    let expr = self.analyze_expression(expression, &any_context);

                    let tk = (*expr.ty.kind).clone();

                    let maybe_to_string = self
                        .shared
                        .state
                        .associated_impls
                        .get_internal_member_function(&expr.ty, "string");

                    if matches!(tk, TypeKind::String { .. }) {
                        expr
                    } else {
                        let underlying = expr.ty.clone();

                        // Check if the expression needs storage validation before proceeding
                        if self.needs_storage_error(&expr, true, &any_context, &expression.node) {
                            return self.create_err(ErrorKind::NeedStorage, &expression.node);
                        }

                        let call_expr_kind = if maybe_to_string.is_none() {
                            let string_type = self.types().string();

                            ExpressionKind::StringLiteral(format!("missing {string_type}"))
                            /* TODO:

                                       return self.create_err(
                                           ErrorKind::MissingToString(string_type),
                                           &expression.node,
                                       );

                            */
                        } else {
                            let expr_as_param = ArgumentExpression::Expression(expr);
                            self.create_static_member_call(
                                "string",
                                &[expr_as_param.clone()],
                                &expression.node,
                                &underlying,
                            )
                        };

                        /*
                        TODO: SUPPORT FORMAT SPEC
                        let resolved_format_specifier =
                            self.analyze_format_specifier(Option::from(format_specifier));

                         */

                        let string_type = self.shared.state.types.string();
                        self.create_expr(call_expr_kind, string_type, &expression.node)
                    }
                }
            };

            let x_last_expr = if let Some(last_expr) = last_expression.clone() {
                let op_kind = BinaryOperatorKind::Add;
                let node = created_expression.node.clone();
                let op = BinaryOperator {
                    left: Box::new(last_expr),
                    right: Box::new(created_expression),
                    kind: op_kind,
                    node: node.clone(),
                };

                let string_type = self.shared.state.types.string();
                create_expr_resolved(ExpressionKind::BinaryOp(op), string_type, &node)
            } else {
                created_expression
            };

            last_expression = Some(x_last_expr);
        }

        // if we have no last_expression, then it means the whole
        // interpolation was completely was empty (`''`)
        if let Some(last) = last_expression {
            last
        } else {
            let string_type = self.shared.state.types.string();
            self.create_expr(
                ExpressionKind::StringLiteral(String::new()),
                string_type,
                node,
            )
        }
    }

    pub(crate) fn analyze_identifier(
        &mut self,
        qualified_func_name: &swamp_ast::QualifiedIdentifier,
    ) -> Expression {
        // Must check variable first, since that is more intuitive for the user.
        // local variables before other functions
        if qualified_func_name.module_path.is_none()
            && qualified_func_name.generic_params.is_empty()
            && let Some(found_variable) = self.try_find_variable(&qualified_func_name.name) {
            return self.create_expr(
                ExpressionKind::VariableAccess(found_variable.clone()),
                found_variable.resolved_type.clone(),
                &qualified_func_name.name,
            );
        }

        let text = self.get_text(&qualified_func_name.name);
        self.create_err(
            ErrorKind::UnknownIdentifier(text.to_string()),
            &qualified_func_name.name,
        )
    }

    // The ast assumes it is something similar to a variable, but it can be a function reference as well.
    fn analyze_variable_reference(&mut self, var_node: &swamp_ast::Node) -> Expression {
        if let Some(found_variable) = self.try_find_variable(var_node) {
            return self.create_expr(
                ExpressionKind::VariableAccess(found_variable.clone()),
                found_variable.resolved_type.clone(),
                var_node,
            );
        }
        let text = self.get_text(var_node);
        self.create_err(ErrorKind::UnknownIdentifier(text.to_string()), var_node)
    }

    fn analyze_internal_initializer_pair_list(
        &mut self,
        node: &swamp_ast::Node,
        items: &[(swamp_ast::Expression, swamp_ast::Expression)],
        context: &TypeContext,
    ) -> (TypeRef, Vec<(Expression, Expression)>) {
        let (collection_type, key_type, value_type) = if let Some(expected_type) =
            context.expected_type
        {
            match &*expected_type.kind {
                TypeKind::MapStorage(key, value, capacity) => {
                    if items.len() > *capacity {
                        self.add_err(
                            ErrorKind::TooManyInitializerListElementsForStorage {
                                capacity: *capacity,
                            },
                            node,
                        );
                        return (self.types().unit(), vec![]);
                    }
                    (expected_type.clone(), key.clone(), value.clone())
                }
                TypeKind::DynamicLengthMapView(key, value) => {
                    (expected_type.clone(), key.clone(), value.clone())
                }
                _ => {
                    self.add_err(ErrorKind::ExpectedSlice, node);
                    return (self.types().unit(), vec![]);
                }
            }
        } else if items.is_empty() {
            self.add_err(ErrorKind::NoInferredTypeForEmptyInitializer, node);
            return (self.types().unit(), vec![]);
        } else {
            // Try to detect, by checking the first
            let maybe_key_context = TypeContext::new_anything_argument(true);

            let first_key_expression = self.analyze_expression(&items[0].0, &maybe_key_context);

            let maybe_value_context = TypeContext::new_anything_argument(true);
            let first_value_expression = self.analyze_expression(&items[0].1, &maybe_value_context);

            let required_key_type = first_key_expression.ty;
            let required_value_type = first_value_expression.ty;

            let inferred_type =
                self.types()
                    .map_storage(&required_key_type, &required_value_type, items.len());

            (inferred_type, required_key_type, required_value_type)
        };

        assert!(!matches!(*key_type.kind, TypeKind::Unit));
        assert!(!matches!(*value_type.kind, TypeKind::Unit));

        let required_key_context = TypeContext::new_argument(&key_type, true);
        let required_value_context = TypeContext::new_argument(&value_type, true);

        let mut resolved_items = Vec::new();

        for (key_expr, value_expr) in items {
            let analyzed_key_expr = self.analyze_expression(key_expr, &required_key_context);
            let analyzed_value_expr = self.analyze_expression(value_expr, &required_value_context);
            resolved_items.push((analyzed_key_expr, analyzed_value_expr));
        }

        (collection_type, resolved_items)
    }

    fn analyze_internal_initializer_list(
        &mut self,
        node: &swamp_ast::Node,
        items: &[swamp_ast::Expression],
        context: &TypeContext,
    ) -> (TypeRef, Vec<Expression>) {
        let (collection_type, element_type) = if let Some(expected_type) = context.expected_type {
            match &*expected_type.kind {
                TypeKind::GridStorage(element_type, width, height) => {
                    let capacity = width * height;
                    if items.len() > capacity {
                        return (
                            self.types().unit(),
                            self.create_err_vec(
                                ErrorKind::TooManyInitializerListElementsForStorage { capacity },
                                node,
                            ),
                        );
                    }
                    (expected_type.clone(), element_type.clone())
                }
                TypeKind::StackStorage(element_type, capacity)
                | TypeKind::QueueStorage(element_type, capacity)
                | TypeKind::SparseStorage(element_type, capacity)
                | TypeKind::VecStorage(element_type, capacity) => {
                    if items.len() > *capacity {
                        return (
                            self.types().unit(),
                            self.create_err_vec(
                                ErrorKind::TooManyInitializerListElementsForStorage {
                                    capacity: *capacity,
                                },
                                node,
                            ),
                        );
                    }
                    (expected_type.clone(), element_type.clone())
                }
                TypeKind::QueueView(element_type) => {
                    // For QueueView expected type, infer QueueStorage for the literal
                    let inferred_storage_type =
                        self.types().queue_storage(element_type, items.len());
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&inferred_storage_type, &default_node);
                    (inferred_storage_type, element_type.clone())
                }
                TypeKind::SparseView(element_type) => {
                    // For SparseView expected type, infer SparseStorage for the literal
                    let inferred_storage_type =
                        self.types().sparse_storage(element_type, items.len());
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&inferred_storage_type, &default_node);
                    (inferred_storage_type, element_type.clone())
                }
                TypeKind::StackView(element_type) => {
                    // For StackView expected type, infer StackStorage for the literal
                    let inferred_storage_type =
                        self.types().stack_storage(element_type, items.len());
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&inferred_storage_type, &default_node);
                    (inferred_storage_type, element_type.clone())
                }
                TypeKind::GridView(element_type) => {
                    // For GridView, we can't infer dimensions from a 1D list, so keep the expected type
                    (expected_type.clone(), element_type.clone())
                }
                TypeKind::DynamicLengthVecView(element_type) => {
                    // For DynamicLengthVecView expected type, infer VecStorage for the literal
                    let inferred_storage_type = self.types().vec_storage(element_type, items.len());
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&inferred_storage_type, &default_node);
                    (inferred_storage_type, element_type.clone())
                }
                TypeKind::SliceView(element_type) => {
                    // For SliceView expected type, infer VecStorage for the literal
                    let inferred_storage_type = self.types().vec_storage(element_type, items.len());
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&inferred_storage_type, &default_node);
                    (inferred_storage_type, element_type.clone())
                }
                TypeKind::FixedCapacityAndLengthArray(element_type, _size) => {
                    (expected_type.clone(), element_type.clone())
                }
                _ => {
                    return (
                        self.types().unit(),
                        self.create_err_vec(
                            ErrorKind::ExpectedInitializerTarget {
                                destination_type: expected_type.clone(),
                            },
                            node,
                        ),
                    );
                }
            }
        } else if items.is_empty() {
            return (
                self.types().unit(),
                self.create_err_vec(ErrorKind::NoInferredTypeForEmptyInitializer, node),
            );
        } else {
            // Try to detect, by checking the first
            let maybe_context = TypeContext::new_anything_argument(true);
            let first = self.analyze_expression(&items[0], &maybe_context);
            let required_type = first.ty;
            let inferred_vec_storage_type = self.types().vec_storage(&required_type, items.len());
            // Generate default functions for the inferred vec storage type
            let default_node = swamp_ast::Node::default();
            self.add_default_functions(&inferred_vec_storage_type, &default_node);
            (inferred_vec_storage_type, required_type)
        };

        let required_context = TypeContext::new_argument(&element_type, true);
        let mut resolved_items = Vec::new();
        for item in items {
            let resolved_expr = self.analyze_expression(item, &required_context);
            resolved_items.push(resolved_expr);
        }
        (collection_type, resolved_items)
    }

    fn push_block_scope(&mut self, debug_str: &str) {
        let register_watermark = self.scope.total_scopes.current_register;

        self.scope.active_scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Open,
            lookup: Default::default(),
            variables: SeqMap::default(),
            register_watermark,
        });
    }

    fn push_lambda_scope(&mut self, debug_str: &str) {
        // Lambda scopes are virtual and completely transparent to register allocation
        // They don't save any watermark and don't affect register allocation
        self.scope.active_scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Lambda,
            lookup: SeqMap::default(),
            variables: SeqMap::default(),
            register_watermark: 0, // Not used for lambda scopes
        });
    }

    fn pop_block_scope(&mut self, debug_str: &str) {
        self.pop_any_block_scope();
    }

    fn push_closed_block_scope(&mut self) {
        let register_watermark = self.scope.total_scopes.current_register;
        self.scope.active_scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Closed,
            lookup: Default::default(),
            variables: SeqMap::default(),
            register_watermark,
        });
    }

    fn pop_closed_block_scope(&mut self) {
        //self.scope.active_scope.block_scope_stack.pop();
        self.pop_any_block_scope();
    }

    fn pop_any_block_scope(&mut self) {
        let scope = self.scope.active_scope.block_scope_stack.pop().unwrap();

        // Record the highest watermark (greatest depth of virtual registers)
        self.scope.total_scopes.highest_virtual_register = self.scope.total_scopes.current_register;

        // Check if we're inside a lambda scope - if so, don't restore register counter
        let is_inside_lambda = self
            .scope
            .active_scope
            .block_scope_stack
            .iter()
            .any(|s| matches!(s.mode, BlockScopeMode::Lambda));

        if matches!(scope.mode, BlockScopeMode::Lambda) {
            // lambda scope - completely transparent, no register changes
        } else if is_inside_lambda {
            // block scope inside lambda - virtual, no register restoration
        } else {
            // Regular scopes restore their watermark to free up registers
            self.scope.total_scopes.current_register = scope.register_watermark;
        }
    }

    fn analyze_match(
        &mut self,
        scrutinee: &swamp_ast::Expression,
        default_context: &TypeContext,
        arms: &Vec<swamp_ast::MatchArm>,
    ) -> (Match, TypeRef) {
        let mut known_type = default_context.expected_type.cloned();
        let own_context = default_context.clone();
        // Analyze the scrutinee with no specific expected type
        let scrutinee_context = TypeContext::new_anything_argument(true); // we just using the pointer, so pretend that it is a target
        let resolved_scrutinee = self.analyze_expression(scrutinee, &scrutinee_context);
        let scrutinee_type = resolved_scrutinee.ty.clone();

        let mut resolved_arms = Vec::with_capacity(arms.len());

        // Ensure we have at least one arm
        if arms.is_empty() {
            let err_match = Match {
                expression: Box::new(
                    self.create_err(ErrorKind::MatchMustHaveAtLeastOneArm, &scrutinee.node),
                ),
                arms: resolved_arms,
            };
            return (err_match, self.types().unit());
        }

        for arm in arms {
            let (resolved_arm, _anyone_wants_mutable) = self.analyze_arm(
                arm,
                &resolved_scrutinee,
                &own_context.with_expected_type(known_type.as_ref(), false),
                &scrutinee_type,
            );

            if known_type.is_none() {
                known_type = Some(resolved_arm.expression.ty.clone());
            }
            resolved_arms.push(resolved_arm);
        }

        if let Some(encountered_type) = known_type {
            (
                Match {
                    expression: Box::new(resolved_scrutinee),
                    arms: resolved_arms,
                },
                encountered_type,
            )
        } else {
            let err_expr = self.create_err(ErrorKind::MatchArmsMustHaveTypes, &scrutinee.node);

            (
                Match {
                    expression: Box::new(err_expr),
                    arms: resolved_arms.clone(),
                },
                self.types().unit(),
            )
        }
    }

    fn analyze_arm(
        &mut self,
        arm: &swamp_ast::MatchArm,
        _expression: &Expression,
        type_context: &TypeContext,
        expected_condition_type: &TypeRef,
    ) -> (MatchArm, bool) {
        let (resolved_pattern, scope_was_pushed, anyone_wants_mutable) =
            self.analyze_pattern(&arm.pattern, expected_condition_type);

        let resolved_expression = self.analyze_expression(&arm.expression, type_context);
        if scope_was_pushed {
            self.pop_block_scope("analyze_arm");
        }

        let resolved_type = resolved_expression.ty.clone();

        (
            MatchArm {
                pattern: resolved_pattern,
                expression: Box::from(resolved_expression),
                expression_type: resolved_type,
            },
            anyone_wants_mutable,
        )
    }

    fn str_to_int(text: &str) -> Result<i32, ParseIntError> {
        let text = text.replace('_', "");
        text.strip_prefix("0x").map_or_else(
            || {
                text.strip_prefix("-0x").map_or_else(
                    || text.parse::<i32>(),
                    |rest| i32::from_str_radix(rest, 16).map(|x| -x),
                )
            },
            |rest| i32::from_str_radix(rest, 16),
        )
    }

    fn str_to_unsigned_int(text: &str) -> Result<u32, ParseIntError> {
        let text = text.replace('_', "");
        text.strip_prefix("0x")
            .map_or_else(|| text.parse::<u32>(), |rest| u32::from_str_radix(rest, 16))
    }

    fn str_to_float(text: &str) -> Result<f32, ParseFloatError> {
        text.parse::<f32>()
    }

    fn str_to_bool(text: &str) -> Result<bool, ParseBoolError> {
        bool::from_str(text)
    }

    fn analyze_pattern_literal(
        &mut self,
        node: &swamp_ast::Node,
        ast_literal: &swamp_ast::LiteralKind,
        expected_condition_type: &TypeRef,
    ) -> NormalPattern {
        let required_condition_type_context =
            TypeContext::new_argument(expected_condition_type, false);
        let literal = self.analyze_literal(
            node,
            ast_literal,
            &required_condition_type_context,
            &required_condition_type_context,
        );

        if !self
            .types()
            .compatible_with(&literal.ty, expected_condition_type)
        {
            return NormalPattern::Literal(self.create_err(
                ErrorKind::IncompatibleTypes {
                    expected: expected_condition_type.clone(),
                    found: literal.ty,
                },
                node,
            ));
        }

        NormalPattern::Literal(literal)
    }

    const fn to_node(&self, node: &swamp_ast::Node) -> Node {
        Node {
            span: Span {
                file_id: self.shared.file_id,
                offset: node.span.offset,
                length: node.span.length,
            },
        }
    }

    fn get_module_path(&self, module_path: Option<&swamp_ast::ModulePath>) -> Vec<String> {
        module_path.as_ref().map_or_else(Vec::new, |found| {
            let mut strings = Vec::new();
            for path_item in &found.0 {
                strings.push(self.get_text(path_item).to_string());
            }
            strings
        })
    }

    fn get_enum_variant_type(
        &mut self,
        qualified_type_identifier: &swamp_ast::QualifiedTypeIdentifier,
        variant_name: &str,
    ) -> EnumVariantType {
        let Some((symbol_table, enum_name)) =
            self.get_symbol_table_and_name(qualified_type_identifier)
        else {
            self.add_err(
                ErrorKind::UnknownEnumVariantType,
                &qualified_type_identifier.name.0,
            );
            return EnumVariantType {
                common: EnumVariantCommon {
                    name: Default::default(),
                    assigned_name: String::new(),
                    container_index: 0,
                },
                payload_type: self.types().unit(),
            };
        };

        if let Some(enum_name) = symbol_table.get_enum_variant_type(&enum_name, variant_name) {
            enum_name
        } else {
            self.add_err(
                ErrorKind::UnknownEnumVariantType,
                &qualified_type_identifier.name.0,
            );
            EnumVariantType {
                common: EnumVariantCommon {
                    name: Default::default(),
                    assigned_name: String::new(),
                    container_index: 0,
                },
                payload_type: self.types().unit(),
            }
        }
    }

    pub(crate) fn get_symbol_table_and_name(
        &self,
        type_identifier: &swamp_ast::QualifiedTypeIdentifier,
    ) -> Option<(&SymbolTable, String)> {
        let path = self.get_module_path(type_identifier.module_path.as_ref());
        let name = self.get_text(&type_identifier.name.0).to_string();

        if let Some(found) = self.shared.get_symbol_table(&path) {
            Some((found, name))
        } else {
            None
        }
    }

    const fn analyze_compound_operator(
        &self,
        ast_operator: &swamp_ast::CompoundOperator,
    ) -> CompoundOperator {
        let resolved_node = self.to_node(&ast_operator.node);
        let resolved_kind = match ast_operator.kind {
            swamp_ast::CompoundOperatorKind::Add => CompoundOperatorKind::Add,
            swamp_ast::CompoundOperatorKind::Sub => CompoundOperatorKind::Sub,
            swamp_ast::CompoundOperatorKind::Mul => CompoundOperatorKind::Mul,
            swamp_ast::CompoundOperatorKind::Div => CompoundOperatorKind::Div,
            swamp_ast::CompoundOperatorKind::Modulo => CompoundOperatorKind::Modulo,
        };

        CompoundOperator {
            node: resolved_node,
            kind: resolved_kind,
        }
    }

    const fn to_node_option(&self, maybe_node: Option<&swamp_ast::Node>) -> Option<Node> {
        match maybe_node {
            None => None,
            Some(node) => Some(self.to_node(node)),
        }
    }

    const fn analyze_format_specifier(
        &self,
        ast_format_specifier: Option<&swamp_ast::FormatSpecifier>,
    ) -> Option<FormatSpecifier> {
        let f = match ast_format_specifier {
            None => return None,
            Some(ast_format) => match ast_format {
                swamp_ast::FormatSpecifier::LowerHex(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::LowerHex,
                },
                swamp_ast::FormatSpecifier::UpperHex(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::UpperHex,
                },
                swamp_ast::FormatSpecifier::Binary(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::Binary,
                },
                swamp_ast::FormatSpecifier::Float(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::Float,
                },
                swamp_ast::FormatSpecifier::Precision(value, node, x) => {
                    let (precision_type, precision_node) = match x {
                        swamp_ast::PrecisionType::Float(node) => {
                            (PrecisionType::Float, self.to_node(node))
                        }
                        swamp_ast::PrecisionType::String(node) => {
                            (PrecisionType::String, self.to_node(node))
                        }
                    };
                    FormatSpecifier {
                        node: self.to_node(node),
                        kind: FormatSpecifierKind::Precision(
                            *value,
                            precision_node,
                            precision_type,
                        ),
                    }
                }
            },
        };

        Some(f)
    }

    fn analyze_with_expr(
        &mut self,
        context: &TypeContext,
        variables: &[swamp_ast::VariableBinding],
        expression: &swamp_ast::Expression,
    ) -> Expression {
        let mut variable_expressions = Vec::new();

        for variable in variables {
            let any_context = TypeContext::new_anything_argument(false);
            let must_have_expression = if let Some(x) = &variable.expression {
                x
            } else {
                &swamp_ast::Expression {
                    kind: swamp_ast::ExpressionKind::VariableReference(variable.variable.clone()),
                    node: variable.variable.name.clone(),
                }
            };

            // Determine the correct LocationSide based on the variable binding type
            let location_side = if variable.variable.is_mutable.is_some() {
                // For mutable variables in with statements, we need to check if this is an lvalue binding
                if let Some(expr) = &variable.expression {
                    // Use analyze_maybe_ref_expression to properly detect mutable references
                    let maybe_ref = self.analyze_maybe_ref_expression(expr);
                    if maybe_ref.has_borrow_mutable_reference.is_some() {
                        LocationSide::Mutable
                    } else {
                        LocationSide::Rhs
                    }
                } else {
                    // For cases like `with mut x` (no expression), it's an alias
                    LocationSide::Rhs
                }
            } else {
                // For immutable variables, always use Rhs
                LocationSide::Rhs
            };

            let var = self.analyze_mut_or_immutable_expression(
                must_have_expression,
                &any_context,
                location_side,
            );

            variable_expressions.push(var);
        }

        self.push_closed_block_scope();
        let mut expressions = Vec::new();
        for (variable_binding, resolved_expression) in variables.iter().zip(variable_expressions) {
            let initialize_variable_expression = self
                .create_variable_binding_for_with(&variable_binding.variable, resolved_expression);

            // Always add the expression to ensure proper initialization/binding
            // Even for aliases (VariableAccess), we need the expression in the block
            // to establish the proper scope and binding
            expressions.push(initialize_variable_expression);
        }

        let resolved_expression = self.analyze_expression(expression, context);
        // with statements are for side effects only, so they always return Unit type
        let block_type = self.types().unit();
        expressions.push(resolved_expression);

        let block_expression_kind = ExpressionKind::Block(expressions);
        self.pop_closed_block_scope();

        self.create_expr(block_expression_kind, block_type, &expression.node)
    }

    fn analyze_when_expr(
        &mut self,
        context: &TypeContext,
        variables: &[swamp_ast::VariableBinding],
        true_expr: &swamp_ast::Expression,
        else_expr: Option<&swamp_ast::Expression>,
    ) -> Expression {
        // Since we are making new variable bindings, we have to push a scope for them
        self.push_block_scope("when");
        let mut bindings = Vec::new();
        for variable_binding in variables {
            let mut_expr = if let Some(found_expr) = &variable_binding.expression {
                let any_context = TypeContext::new_anything_argument(true); // we are not just having an alias binding to another value, so we can think of it having a target
                self.analyze_mut_or_immutable_expression(
                    found_expr,
                    &any_context,
                    LocationSide::Rhs,
                )
                    .expect_immutable()
                    .unwrap()
            } else {
                let same_var = self.find_variable(&variable_binding.variable);

                // For when expressions, we always want to extract the value from the optional,
                // not create a mutable reference to the original variable
                let generated_expr_kind = ExpressionKind::VariableAccess(same_var.clone());
                self.create_expr(
                    generated_expr_kind,
                    same_var.resolved_type.clone(),
                    &variable_binding.variable.name,
                )
            };

            let tk = &mut_expr.ty.kind;

            if let TypeKind::Optional(found_ty) = &**tk {
                // TODO: In the future we should check if this binding variable is just the payload of the variable
                // given to it. in that case we should just create a param like variable, in the sense
                // that it is *not* allocated in the stack frame, but is just represented as a register
                let variable_ref = { //variable_binding.expression.is_some() || variable_binding.variable.is_mutable.is_some() {
                    self.create_variable(&variable_binding.variable, found_ty)
                }/* else {
                    self.create_local_variable_parameter_like(&variable_binding.variable.name, Option::from(&variable_binding.variable.is_mutable), found_ty, false)
                }*/;

                let binding = WhenBinding {
                    variable: variable_ref,
                    expr: mut_expr,
                };
                bindings.push(binding);
            } else {
                return self.create_err(ErrorKind::ExpectedOptional, &true_expr.node);
            }
        }

        let resolved_true = self.analyze_expression(true_expr, context);
        let block_type = resolved_true.ty.clone();

        self.pop_block_scope("when");

        let maybe_resolved_else = if let Some(found_else) = else_expr {
            let block_type_for_true_context = context.we_know_expected_type(&block_type, false);
            Some(Box::new(self.analyze_expression(
                found_else,
                &block_type_for_true_context,
            )))
        } else {
            None
        };

        let when_kind =
            ExpressionKind::When(bindings, Box::from(resolved_true), maybe_resolved_else);

        self.create_expr(when_kind, block_type, &true_expr.node)
    }

    fn analyze_guard(
        &mut self,
        node: &swamp_ast::Node,
        context: &TypeContext,
        guard_expressions: &Vec<swamp_ast::GuardExpr>,
    ) -> Expression {
        let mut guards = Vec::new();
        let mut found_wildcard = None;
        let mut detected_type = context.expected_type.cloned();

        for guard in guard_expressions {
            let resolved_condition = match &guard.clause {
                swamp_ast::GuardClause::Wildcard(x) => {
                    if found_wildcard.is_some() {
                        return self.create_err(ErrorKind::GuardCanNotHaveMultipleWildcards, node);
                    }
                    found_wildcard = Some(x);
                    None
                }
                swamp_ast::GuardClause::Expression(clause_expr) => {
                    if found_wildcard.is_some() {
                        return self.create_err(ErrorKind::WildcardMustBeLastInGuard, node);
                    }
                    Some(self.analyze_bool_argument_expression(clause_expr))
                }
            };

            let resolved_result = self.analyze_expression(
                &guard.result,
                &context.with_expected_type(detected_type.as_ref(), false),
            );
            let ty = resolved_result.ty.clone();
            if detected_type.is_none() {
                detected_type = Some(ty.clone());
            }

            guards.push(Guard {
                condition: resolved_condition,
                result: resolved_result,
            });
        }

        if found_wildcard.is_none() {
            return self.create_err(ErrorKind::GuardMustHaveWildcard, node);
        }

        let kind = ExpressionKind::Guard(guards);

        if let Some(found_expecting_type) = detected_type {
            self.create_expr(kind, found_expecting_type, node)
        } else {
            self.create_err(ErrorKind::GuardHasNoType, node)
        }
    }

    fn analyze_lambda(
        &mut self,
        node: &swamp_ast::Node,
        variables: &[swamp_ast::Variable],
        ast_expr: &swamp_ast::Expression,
        context: &TypeContext,
    ) -> Expression {
        let TypeKind::Function(signature) = &*context.expected_type.unwrap().kind else {
            return self.create_err(ErrorKind::ExpectedLambda, node);
        };

        let return_block_type = TypeContext::new_argument(&signature.return_type, false);

        // Create a lambda scope for proper variable scoping and shadowing
        // But the register allocation will "continue" from parent scope (no decrement on pop)
        self.push_lambda_scope("lambda");

        let arity_required = signature.parameters.len();
        let variable_types_to_create = if variables.len() == arity_required {
            &signature.parameters
        } else if variables.len() + 1 == arity_required {
            &signature.parameters[1..].to_vec()
        } else {
            return self.create_err(ErrorKind::WrongNumberOfArguments(0, 0), node);
        };

        let mut resolved_variables = Vec::new();
        for (variable, variable_type) in variables.iter().zip(variable_types_to_create) {
            let variable_ref = self.create_local_variable(
                &variable.name,
                variable.is_mutable.as_ref(),
                &variable_type.resolved_type,
                false,
            );
            resolved_variables.push(variable_ref);
        }

        // Analyze the lambda body expression directly without creating additional block scopes
        // This ensures lambda variables are allocated in the lambda scope, not inner scopes
        let analyzed_expression = self.analyze_expression(ast_expr, &return_block_type);

        self.pop_block_scope("lambda");

        let function_type = self.types().function(signature.clone());
        self.create_expr(
            ExpressionKind::Lambda(resolved_variables, Box::new(analyzed_expression)),
            function_type,
            node,
        )
    }

    #[must_use]
    pub fn chain_is_owned_result(
        start_of_chain: &StartOfChain,
        chains: &Vec<Postfix>,
    ) -> (bool, bool) {
        let mut is_owned_result = matches!(start_of_chain.kind, StartOfChainKind::Expression(_));
        let mut is_mutable = if let StartOfChainKind::Variable(var) = &start_of_chain.kind {
            var.is_mutable()
        } else {
            false
        };

        for chain in chains {
            match chain.kind {
                PostfixKind::StructField(_, _) => {
                    is_owned_result = false;
                }
                PostfixKind::MemberCall(_, _) => {
                    is_owned_result = true;
                    is_mutable = false;
                }
                PostfixKind::OptionalChainingOperator => {
                    is_owned_result = true;
                    is_mutable = false;
                }
                PostfixKind::VecSubscript(_, _) => {}
                PostfixKind::VecSubscriptRange(_, _) => {}
                PostfixKind::SparseSubscript(_, _) => {}
                PostfixKind::GridSubscript(_, _, _) => {}
                PostfixKind::MapSubscript(_, _) => {}
            }
        }

        (is_owned_result, is_mutable)
    }

    pub fn check_assignment_mode(
        &mut self,
        lhs_is_mutable: bool,
        source_expression: &Expression,
        ty: &TypeRef,
    ) -> AssignmentMode {
        if matches!(
            &source_expression.kind,
            _ | ExpressionKind::IntrinsicCallEx(_, _)
        ) {
            return AssignmentMode::OwnedValue;
        }

        /* TODO:
        if ty.is_direct() {
            return AssignmentMode::OwnedValue;
        }

         */

        if let ExpressionKind::PostfixChain(start_chain, postfix) = &source_expression.kind {
            let (chain_is_owned, chain_is_mutable) =
                Self::chain_is_owned_result(start_chain, postfix);
            if lhs_is_mutable {
                return if chain_is_mutable {
                    AssignmentMode::CopyBlittable
                } else if lhs_is_mutable {
                    if chain_is_owned {
                        AssignmentMode::OwnedValue
                    } else {
                        AssignmentMode::CopyBlittable
                    }
                } else {
                    AssignmentMode::CopySharedPtr
                };
            }
            // if not mutable, it is always ok
        }

        AssignmentMode::CopyBlittable
    }

    pub const fn check_mutable_assignment(&mut self, assignment_mode: AssignmentMode, node: &Node) {}

    pub const fn check_mutable_variable_assignment(
        &mut self,
        source_expression: &swamp_ast::Expression,
        ty: &TypeRef,
        variable: &swamp_ast::Variable,
    ) {
        //let is_allowed_to_assign = Self::check_mutable_assignment_is_valid(ty);
        //if !is_allowed_to_assign {
        //  error!(?ty, "variable is wrong");
        //return Err(self.create_err(ErrorKind::ArgumentIsNotMutable, &variable.name));
        //}
    }

    /// # Errors
    ///
    pub fn analyze_variable_assignment(
        &mut self,
        variable: &swamp_ast::Variable,
        source_expression: &swamp_ast::Expression,
    ) -> Expression {
        let maybe_found_variable = self.try_find_variable(&variable.name);

        let required_type = maybe_found_variable
            .as_ref()
            .map(|found_variable| found_variable.resolved_type.clone());

        let source_expr = if let Some(target_type) = &required_type {
            self.analyze_expression_for_assignment_with_target_type(target_type, source_expression)
        } else {
            let any_type_context = TypeContext::new_anything_argument(true);
            self.analyze_expression(source_expression, &any_type_context)
        };

        // Check if the variable type (target type) can be stored in a variable
        // Skip this check for parameters since they already have allocated storage
        let target_type = if let Some(found_var) = &maybe_found_variable {
            &found_var.resolved_type
        } else {
            &source_expr.ty
        };

        // Only check blittable for local variables, not parameters
        let should_check_blittable = if let Some(found_var) = &maybe_found_variable {
            !matches!(found_var.variable_type, VariableType::Parameter)
        } else {
            true // New variable definitions need to be blittable
        };

        if should_check_blittable && !target_type.is_blittable() {
            let debug_text = self.get_text(&variable.name);
            if !debug_text.starts_with('_') {
                return self.create_err(
                    ErrorKind::VariableTypeMustBeBlittable(target_type.clone()),
                    &variable.name,
                );
            }
        }

        let kind: ExpressionKind = if let Some(found_var) = maybe_found_variable {
            if !found_var.is_mutable() {
                return self.create_err(ErrorKind::VariableIsNotMutable, &variable.name);
            }
            if !self
                .types()
                .compatible_with(&found_var.resolved_type, &source_expr.ty)
            {
                return self.create_err(
                    ErrorKind::IncompatibleTypes {
                        expected: source_expr.ty,
                        found: found_var.resolved_type.clone(),
                    },
                    &variable.name,
                );
            }
            self.check_mutable_variable_assignment(source_expression, &source_expr.ty, variable);
            ExpressionKind::VariableReassignment(found_var, Box::from(source_expr))
        } else {
            if !source_expr.ty.is_blittable() {
                let text = self.get_text(&variable.name);
                error!(?text, ?required_type, ?source_expr, "variable is wrong");
            }

            // If it is mutable, we might need to clone the source
            // so make some extra verification
            if variable.is_mutable.is_some() {
                self.check_mutable_variable_assignment(
                    source_expression,
                    &source_expr.ty,
                    variable,
                );
            }
            let new_var = self.create_variable(variable, &source_expr.ty);
            ExpressionKind::VariableDefinition(new_var, Box::from(source_expr))
        };

        let unit_type = self.shared.state.types.unit();
        self.create_expr(kind, unit_type, &variable.name)
    }

    fn analyze_create_variable(
        &mut self,
        var: &swamp_ast::Variable,
        annotation_type: Option<&swamp_ast::Type>,
        source_expression: &swamp_ast::Expression,
    ) -> Expression {
        let maybe_annotated_type =
            annotation_type.map(|found_ast_type| self.analyze_type(found_ast_type, &TypeAnalyzeContext::default()));

        let unsure_arg_context =
            TypeContext::new_unsure_argument(maybe_annotated_type.as_ref(), true);

        let resolved_source = self.analyze_expression(source_expression, &unsure_arg_context);

        let resulting_type = if let Some(annotated_type) = maybe_annotated_type {
            annotated_type
        } else {
            resolved_source.ty.clone()
        };
        let var_ref = self.create_local_variable(
            &var.name,
            Option::from(&var.is_mutable),
            &resulting_type,
            true,
        );

        if *resulting_type.kind == TypeKind::Unit {
            return self.create_err(ErrorKind::VariableTypeMustBeBlittable(resulting_type), &var.name);
        }
        assert_ne!(&*resulting_type.kind, &TypeKind::Unit);
        let kind = ExpressionKind::VariableDefinition(var_ref, Box::from(resolved_source));

        let unit_type = self.shared.state.types.unit();

        self.create_expr(kind, unit_type, &var.name)
    }

    fn add_location_item(
        &mut self,
        vec: &mut Vec<LocationAccess>,
        kind: LocationAccessKind,
        ty: TypeRef,
        ast_node: &swamp_ast::Node,
    ) {
        let resolved_node = self.to_node(ast_node);
        let postfix = LocationAccess {
            node: resolved_node,
            ty,
            kind,
        };

        vec.push(postfix);
    }

    fn extract_single_intrinsic_call(
        body: &Expression,
    ) -> Option<(IntrinsicFunction, Vec<ArgumentExpression>)> {
        if let ExpressionKind::Block(expressions) = &body.kind {
            let first_kind = &expressions[0].kind;
            if let ExpressionKind::IntrinsicCallEx(intrinsic_fn, args) = &first_kind {
                return Some((intrinsic_fn.clone(), args.clone()));
            }
        }
        None
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_chain_to_location(
        &mut self,
        chain: &swamp_ast::PostfixChain,
        context: &TypeContext,
        location_side: LocationSide,
    ) -> SingleLocationExpression {
        let mut items = Vec::new();

        let nothing_context = TypeContext::new(None, true);

        let base_expr = self.analyze_expression(&chain.base, &nothing_context);
        let ExpressionKind::VariableAccess(start_variable) = base_expr.kind else {
            self.add_err(ErrorKind::NotValidLocationStartingPoint, &chain.base.node);
            let unit_type = self.types().unit();
            let err_variable = Variable {
                name: Default::default(),
                assigned_name: String::new(),
                resolved_type: unit_type,
                mutable_node: None,
                variable_type: VariableType::Local,
                scope_index: 0,
                variable_index: 0,
                unique_id_within_function: 0,
                virtual_register: 0,
                is_unused: false,
            };
            return SingleLocationExpression {
                kind: MutableReferenceKind::MutVariableRef,
                node: self.to_node(&chain.base.node),
                ty: self.shared.state.types.unit(),
                starting_variable: VariableRef::from(err_variable),
                access_chain: vec![],
            };
        };

        if !start_variable.is_mutable() {
            self.add_err(ErrorKind::VariableIsNotMutable, &chain.base.node);

            let unit_type = self.types().unit();
            let err_variable = Variable {
                name: Default::default(),
                assigned_name: String::new(),
                resolved_type: unit_type,
                mutable_node: None,
                variable_type: VariableType::Local,
                scope_index: 0,
                variable_index: 0,
                unique_id_within_function: 0,
                virtual_register: 0,
                is_unused: false,
            };
            return SingleLocationExpression {
                kind: MutableReferenceKind::MutVariableRef,
                node: self.to_node(&chain.base.node),
                ty: self.shared.state.types.unit(),
                starting_variable: VariableRef::from(err_variable),
                access_chain: vec![],
            };
        }

        let mut ty = start_variable.resolved_type.clone();
        for (i, item) in chain.postfixes.iter().enumerate() {
            let is_absolute_last_in_chain = i == chain.postfixes.len() - 1;
            match &item {
                swamp_ast::Postfix::FieldAccess(field_name_node) => {
                    //let field_name_resolved = self.to_node(field_name_node)
                    let (struct_type_ref, index, return_type) =
                        self.analyze_struct_field(field_name_node, &ty);

                    self.add_location_item(
                        &mut items,
                        LocationAccessKind::FieldIndex(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name_node,
                    );

                    ty = return_type.clone();
                }
                swamp_ast::Postfix::SubscriptTuple(x_expr, y_expr) => match &*ty.kind {
                    TypeKind::GridView(element_type)
                    | TypeKind::GridStorage(element_type, _, _) => {
                        let int_type = self.types().int();
                        let unsigned_int_context = TypeContext::new_argument(&int_type, true);
                        let unsigned_int_x_expr =
                            self.analyze_expression(x_expr, &unsigned_int_context);

                        let unsigned_int_y_expr =
                            self.analyze_expression(y_expr, &unsigned_int_context);

                        let grid_type = GridType {
                            element: element_type.clone(),
                        };

                        self.add_location_item(
                            &mut items,
                            LocationAccessKind::GridSubscript(
                                grid_type,
                                unsigned_int_x_expr,
                                unsigned_int_y_expr,
                            ),
                            element_type.clone(),
                            &x_expr.node,
                        );

                        ty = element_type.clone();
                    }
                    _ => {
                        self.add_err_resolved(ErrorKind::CanNotSubscriptWithThatType, &base_expr.node);

                        return SingleLocationExpression {
                            kind: MutableReferenceKind::MutVariableRef,
                            node: Default::default(),
                            ty,
                            starting_variable: Rc::new(Variable {
                                name: Default::default(),
                                assigned_name: String::new(),
                                resolved_type: Rc::new(Type {
                                    id: TypeId::new(0),
                                    flags: Default::default(),
                                    kind: Rc::new(TypeKind::Byte),
                                }),
                                mutable_node: None,
                                variable_type: VariableType::Local,
                                scope_index: 0,
                                variable_index: 0,
                                unique_id_within_function: 0,
                                virtual_register: 0,
                                is_unused: false,
                            }),
                            access_chain: vec![],
                        };
                    }
                },
                swamp_ast::Postfix::Subscript(ast_key_expression) => {
                    let underlying = &ty.kind;
                    match &**underlying {
                        TypeKind::SliceView(element_type)
                        | TypeKind::StackStorage(element_type, _)
                        | TypeKind::StackView(element_type)
                        | TypeKind::VecStorage(element_type, _)
                        | TypeKind::DynamicLengthVecView(element_type)
                        | TypeKind::FixedCapacityAndLengthArray(element_type, _) => {
                            let int_type = self.types().int();
                            let unsigned_int_context = TypeContext::new_argument(&int_type, false);
                            let unsigned_int_expr =
                                self.analyze_expression(ast_key_expression, &unsigned_int_context);

                            let slice_type = SliceViewType {
                                element: element_type.clone(),
                            };

                            self.add_location_item(
                                &mut items,
                                LocationAccessKind::SliceViewSubscript(
                                    slice_type,
                                    unsigned_int_expr,
                                ),
                                element_type.clone(),
                                &ast_key_expression.node,
                            );

                            ty = element_type.clone();
                        }
                        TypeKind::SparseView(element_type)
                        | TypeKind::SparseStorage(element_type, _) => {
                            let int_type = self.types().int();
                            let unsigned_int_context = TypeContext::new_argument(&int_type, false);
                            let unsigned_int_expr =
                                self.analyze_expression(ast_key_expression, &unsigned_int_context);

                            let slice_type = SparseType {
                                element: element_type.clone(),
                            };

                            self.add_location_item(
                                &mut items,
                                LocationAccessKind::SparseSubscript(slice_type, unsigned_int_expr),
                                element_type.clone(),
                                &ast_key_expression.node,
                            );

                            ty = element_type.clone();
                        }
                        TypeKind::MapStorage(key_type, value_type, ..)
                        | TypeKind::DynamicLengthMapView(key_type, value_type) => {
                            let key_index_context = TypeContext::new_argument(key_type, false);
                            let key_expr =
                                self.analyze_expression(ast_key_expression, &key_index_context);

                            let map_like_type = MapType {
                                key: key_type.clone(),
                                value: value_type.clone(),
                            };

                            match location_side {
                                LocationSide::Lhs => {
                                    // Only if it is the absolute last subscript in the chain that we should
                                    // create a new map entry, otherwise we should just lookup an existing entry and get the
                                    // address to it.
                                    let access_kind = if is_absolute_last_in_chain {
                                        LocationAccessKind::MapSubscriptCreateIfNeeded(
                                            map_like_type,
                                            key_expr,
                                        )
                                    } else {
                                        LocationAccessKind::MapSubscriptMustExist(
                                            map_like_type,
                                            key_expr,
                                        )
                                    };

                                    self.add_location_item(
                                        &mut items,
                                        access_kind,
                                        value_type.clone(),
                                        &ast_key_expression.node,
                                    );
                                }
                                LocationSide::Mutable | LocationSide::Rhs => {
                                    self.add_location_item(
                                        &mut items,
                                        LocationAccessKind::MapSubscriptMustExist(
                                            map_like_type,
                                            key_expr,
                                        ),
                                        value_type.clone(),
                                        &ast_key_expression.node,
                                    );
                                }
                            }
                            ty = value_type.clone();
                        }

                        _ => {
                            self.add_err_resolved(ErrorKind::CanNotSubscriptWithThatType, &base_expr.node);

                            return SingleLocationExpression {
                                kind: MutableReferenceKind::MutVariableRef,
                                node: Default::default(),
                                ty,
                                starting_variable: Rc::new(Variable {
                                    name: Default::default(),
                                    assigned_name: String::new(),
                                    resolved_type: Rc::new(Type {
                                        id: TypeId::new(0),
                                        flags: Default::default(),
                                        kind: Rc::new(TypeKind::Byte),
                                    }),
                                    mutable_node: None,
                                    variable_type: VariableType::Local,
                                    scope_index: 0,
                                    variable_index: 0,
                                    unique_id_within_function: 0,
                                    virtual_register: 0,
                                    is_unused: false,
                                }),
                                access_chain: vec![],
                            };
                        }
                    }
                }

                swamp_ast::Postfix::MemberCall(node, _generic_arguments, _regular_args) => {
                    return SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(node),
                        ty: self.shared.state.types.unit(),
                        starting_variable: start_variable,
                        access_chain: vec![],
                    };
                }
                /*
                swamp_ast::Postfix::AdvancedFunctionCall(_, node, ..) => {
                    return Err(self.create_err(ErrorKind::CallsCanNotBePartOfChain, node));
                }

                 */
                swamp_ast::Postfix::FunctionCall(node, _generic_arguments, _regular_args) => {
                    return SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(node),
                        ty: self.shared.state.types.unit(),
                        starting_variable: start_variable,
                        access_chain: vec![],
                    };
                }
                swamp_ast::Postfix::OptionalChainingOperator(node) => {
                    return SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(node),
                        ty: self.shared.state.types.unit(),
                        starting_variable: start_variable,
                        access_chain: vec![],
                    };
                }
            }
        }

        if let Some(found_expected_type) = context.expected_type
            && !self.types().compatible_with(found_expected_type, &ty) {
            self.add_err(
                ErrorKind::IncompatibleTypes {
                    expected: found_expected_type.clone(),
                    found: ty.clone(),
                },
                &chain.base.node,
            );
        }

        SingleLocationExpression {
            kind: MutableReferenceKind::MutVariableRef,
            node: self.to_node(&chain.base.node),
            ty,
            starting_variable: start_variable,
            access_chain: items,
        }
    }

    fn analyze_to_location(
        &mut self,
        expr: &swamp_ast::Expression,
        context: &TypeContext,
        location_type: LocationSide,
    ) -> SingleLocationExpression {
        match &expr.kind {
            swamp_ast::ExpressionKind::PostfixChain(chain) => {
                self.analyze_chain_to_location(chain, context, location_type)
            }
            swamp_ast::ExpressionKind::VariableReference(variable) => {
                let var = self.find_variable(variable);
                if !var.is_mutable() {
                    self.add_err(ErrorKind::VariableIsNotMutable, &expr.node);
                }

                SingleLocationExpression {
                    kind: MutableReferenceKind::MutVariableRef,
                    node: self.to_node(&variable.name),
                    ty: var.resolved_type.clone(),
                    starting_variable: var,
                    access_chain: vec![],
                }
            }
            swamp_ast::ExpressionKind::IdentifierReference(qualified_identifier) => {
                let generated_var = swamp_ast::Variable {
                    name: qualified_identifier.name.clone(),
                    is_mutable: None,
                };
                let var = self.find_variable(&generated_var);
                if !var.is_mutable() {
                    self.add_err(ErrorKind::VariableIsNotMutable, &expr.node);
                }
                SingleLocationExpression {
                    kind: MutableReferenceKind::MutVariableRef,
                    node: self.to_node(&generated_var.name),
                    ty: var.resolved_type.clone(),
                    starting_variable: var,
                    access_chain: vec![],
                }
            }
            _ => {
                self.add_err(ErrorKind::NotValidLocationStartingPoint, &expr.node);
                let unit_type = self.types().unit();
                SingleLocationExpression {
                    kind: MutableReferenceKind::MutVariableRef,
                    node: self.to_node(&expr.node),
                    ty: unit_type.clone(),
                    starting_variable: Rc::new(Variable {
                        name: Default::default(),
                        assigned_name: String::new(),
                        resolved_type: unit_type,
                        mutable_node: None,
                        variable_type: VariableType::Local,
                        scope_index: 0,
                        variable_index: 0,
                        unique_id_within_function: 0,
                        virtual_register: 0,
                        is_unused: false,
                    }),
                    access_chain: vec![],
                }
            }
        }
    }

    fn analyze_expression_for_assignment_compound(
        &mut self,
        target_expression: &swamp_ast::Expression,
        ast_source_expression: &swamp_ast::Expression,
    ) -> (TargetAssignmentLocation, Expression) {
        let any_argument_context = TypeContext::new_anything_argument(true);
        let source_expr = self.analyze_expression(ast_source_expression, &any_argument_context);
        let source_expr_type_context = TypeContext::new_argument(&source_expr.ty, true);

        let resolved_location = TargetAssignmentLocation(self.analyze_to_location(
            target_expression,
            &source_expr_type_context,
            LocationSide::Rhs,
        ));

        (resolved_location, source_expr)
    }

    /*
    fn try_convert_for_assignment(
        &mut self,
        expr: &Expression,
        target_type: &TypeRef,
        ast_node: &swamp_ast::Expression,
    ) -> Result<Option<Expression>, Error> {
        match (target_type, &expr.ty) {
            // Conversion cases that require transformation
            (TypeKind::VecStorage(target_elem, capacity), TypeKind::FixedSlice(source_elem, _)) => {
                // Create conversion with checks
                // ...
            }

            (TypeKind::String, TypeKind::Int) => {
                // Create int-to-string conversion
                // ...
            }

            // No conversion possible
            _ => Ok(None),
        }
    }

     */

    fn analyze_expression_for_assignment_with_target_type(
        &mut self,
        target_type: &TypeRef,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Expression {
        let base_context = TypeContext::new_argument(target_type, true);
        let source_expr = self.analyze_expression(ast_source_expression, &base_context);

        let final_expr = if self.types().compatible_with(target_type, &source_expr.ty) {
            source_expr
        } else {
            let source_type = source_expr.ty.clone();
            self.types_did_not_match_try_late_coerce_expression(
                source_expr,
                target_type,
                &source_type,
                &ast_source_expression.node,
            )
        };

        let assignment_mode = self.check_assignment_mode(true, &final_expr, target_type); // TODO: Fill in correct lhs_is_mutable

        self.check_mutable_assignment(assignment_mode, &final_expr.node);

        final_expr
    }

    fn analyze_expression_for_assignment(
        &mut self,
        ast_target_location_expression: &swamp_ast::Expression,
        ast_source_expression: &swamp_ast::Expression,
    ) -> (TargetAssignmentLocation, Expression) {
        let any_argument_context = TypeContext::new_anything_argument(true);
        let resolved_location = self.analyze_to_location(
            ast_target_location_expression,
            &any_argument_context,
            LocationSide::Lhs,
        );

        let target_type = resolved_location.ty.clone();
        let mut_type = target_type; // Mutable references now use the same type
        let mut_location = TargetAssignmentLocation(resolved_location);

        let final_expr = self
            .analyze_expression_for_assignment_with_target_type(&mut_type, ast_source_expression);

        (mut_location, final_expr)
    }

    fn analyze_assignment_compound(
        &mut self,
        target_expression: &swamp_ast::Expression,
        ast_op: &swamp_ast::CompoundOperator,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Expression {
        let resolved_op = self.analyze_compound_operator(ast_op);

        let (resolved_location, source_expr) = self
            .analyze_expression_for_assignment_compound(target_expression, ast_source_expression);

        let kind = ExpressionKind::CompoundAssignment(
            resolved_location,
            resolved_op.kind,
            Box::from(source_expr),
        );

        let unit_type = self.shared.state.types.unit();

        self.create_expr(kind, unit_type, &target_expression.node)
    }

    fn analyze_assignment_mode(lhs: SingleLocationExpression) {}

    fn analyze_assignment(
        &mut self,
        target_location: &swamp_ast::Expression,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Expression {
        let (mut_location, source_expr) =
            self.analyze_expression_for_assignment(target_location, ast_source_expression);
        let kind = ExpressionKind::Assignment(Box::from(mut_location), Box::from(source_expr));
        let unit_type = self.shared.state.types.unit();

        // Assignments are always of type Unit

        self.create_expr(kind, unit_type, &target_location.node)
    }

    #[must_use]
    pub const fn create_expr(
        &self,
        kind: ExpressionKind,
        ty: TypeRef,
        ast_node: &swamp_ast::Node,
    ) -> Expression {
        //info!(%ty, ?kind, "create_expr()");
        Expression {
            kind,
            ty,
            node: self.to_node(ast_node),
        }
    }

    fn analyze_destructuring(
        &mut self,
        node: &swamp_ast::Node,
        target_ast_variables: &[swamp_ast::Variable],
        tuple_expression: &swamp_ast::Expression,
    ) -> Expression {
        let any_context = TypeContext::new_anything_argument(true);
        let tuple_resolved = self.analyze_expression(tuple_expression, &any_context);
        let tuple_expr_type = &tuple_resolved.ty;
        let unit = self.types().unit();

        let mut variable_refs = Vec::new();
        if let TypeKind::Tuple(tuple) = &*tuple_expr_type.kind {
            if target_ast_variables.len() > tuple.len() {
                return self.create_err(ErrorKind::TooManyDestructureVariables, node);
            }
            for (ast_variable, tuple_type) in target_ast_variables.iter().zip(tuple.clone()) {
                let variable_ref = self.create_local_variable(
                    &ast_variable.name,
                    ast_variable.is_mutable.as_ref(),
                    &tuple_type,
                    true,
                );
                variable_refs.push(variable_ref);
            }
            let expr_kind = ExpressionKind::TupleDestructuring(
                variable_refs,
                tuple_expr_type.clone(),
                Box::from(tuple_resolved),
            );

            self.create_expr(expr_kind, unit, node)
        } else {
            self.create_err(ErrorKind::CanNotDestructure, node)
        }
    }

    fn analyze_normal_member_call(
        &mut self,
        type_that_member_is_on: &TypeRef,
        found_function: &FunctionRef,
        generic_arguments: Vec<TypeRef>,
        ast_arguments: &[swamp_ast::Expression],
        is_mutable: bool,
        node: &swamp_ast::Node,
    ) -> Signature {
        let resolved_node = self.to_node(node);
        // TODO:

        found_function.signature().clone()
    }

    fn queue_member_signature(
        &mut self,
        self_type: &TypeRef,
        key_type: Option<&TypeRef>,
        element_type: &TypeRef,
        field_name_str: &str,
        lambda_variable_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };
        let self_mutable_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: true,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "enqueue" => (
                IntrinsicFunction::VecPush,
                Signature {
                    parameters: vec![
                        self_mutable_type_param,
                        TypeForParameter {
                            name: "element".to_string(),
                            resolved_type: element_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().unit(),
                },
            ),
            "dequeue" => (
                IntrinsicFunction::VecRemoveFirstIndexGetValue,
                Signature {
                    parameters: vec![self_mutable_type_param],
                    return_type: element_type.clone(),
                },
            ),
            _ => {
                self.slice_member_signature(
                    self_type,
                    key_type,
                    element_type,
                    field_name_str,
                    lambda_variable_count,
                    node,
                )
            }?,
        };

        Some(intrinsic_and_signature)
    }

    fn sparse_member_signature(
        &mut self,
        self_type: &TypeRef,
        element_type: &TypeRef,
        field_name_str: &str,
        lambda_variable_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let key_type = self.types().int(); // TODO: SparseID

        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };
        let self_mutable_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: true,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "add" => (
                IntrinsicFunction::SparseAdd,
                Signature {
                    parameters: vec![
                        self_mutable_type_param,
                        TypeForParameter {
                            name: "element".to_string(),
                            resolved_type: element_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().int(),
                },
            ),
            "remove" => (
                IntrinsicFunction::SparseRemove,
                Signature {
                    parameters: vec![
                        self_mutable_type_param,
                        TypeForParameter {
                            name: "key".to_string(),
                            resolved_type: key_type,
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().unit(),
                },
            ),
            "is_alive" => (
                IntrinsicFunction::SparseIsAlive,
                Signature {
                    parameters: vec![
                        self_type_param,
                        TypeForParameter {
                            name: "element".to_string(),
                            resolved_type: element_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().bool(),
                },
            ),

            _ => {
                self.slice_member_signature(
                    self_type,
                    Option::from(key_type).as_ref(),
                    element_type,
                    field_name_str,
                    lambda_variable_count,
                    node,
                )
            }?,
        };
        Some(intrinsic_and_signature)
    }

    fn vec_member_signature(
        &mut self,
        self_type: &TypeRef,
        element_type: &TypeRef,
        field_name_str: &str,
        lambda_variable_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let key_type = self.types().int();
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };
        let self_mutable_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: true,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "prepend" => (
                IntrinsicFunction::VecPush,
                Signature {
                    parameters: vec![
                        self_mutable_type_param,
                        TypeForParameter {
                            name: "element".to_string(),
                            resolved_type: element_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().unit(),
                },
            ),
            "push" => (
                IntrinsicFunction::VecPush,
                Signature {
                    parameters: vec![
                        self_mutable_type_param,
                        TypeForParameter {
                            name: "element".to_string(),
                            resolved_type: element_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().unit(),
                },
            ),
            "pop" => (
                IntrinsicFunction::VecPop,
                Signature {
                    parameters: vec![self_mutable_type_param],
                    return_type: element_type.clone(),
                },
            ),

            "slice" => {
                let range_type = self.types().range_int();
                (
                    IntrinsicFunction::VecSlice,
                    Signature {
                        parameters: vec![self_type_param,
                                         TypeForParameter {
                                             name: "range".to_string(),
                                             resolved_type: range_type,
                                             is_mutable: false,
                                             node: None,
                                         }, ],
                        return_type: self_type.clone(),
                    },
                )
            }

            _ => {
                self.slice_member_signature(
                    self_type,
                    Option::from(key_type).as_ref(),
                    element_type,
                    field_name_str,
                    lambda_variable_count,
                    node,
                )
            }?,
        };
        Some(intrinsic_and_signature)
    }

    #[allow(clippy::unnecessary_wraps)]
    fn grid_member_signature(
        &mut self,
        self_type: &TypeRef,
        element_type: &TypeRef,
        field_name_str: &str,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };
        let self_mutable_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: true,
            node: None,
        };
        let element_param = TypeForParameter {
            name: "element".to_string(),
            resolved_type: element_type.clone(),
            is_mutable: false,
            node: None,
        };
        let int_type = self.types().int();

        let int_param = TypeForParameter {
            name: "x_or_y".to_string(),
            resolved_type: int_type.clone(),
            is_mutable: false,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "set" => (
                IntrinsicFunction::GridSet,
                Signature {
                    parameters: vec![self_type_param, int_param.clone(), int_param, element_param],
                    return_type: self.types().unit(),
                },
            ),

            "get" => (
                IntrinsicFunction::GridGet,
                Signature {
                    parameters: vec![self_type_param, int_param.clone(), int_param],
                    return_type: element_type.clone(),
                },
            ),

            "width" => (
                IntrinsicFunction::GridWidth,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: int_type,
                },
            ),

            "height" => (
                IntrinsicFunction::GridHeight,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: int_type,
                },
            ),

            _ => panic!("unknown grid method {field_name_str}"),
        };

        Some(intrinsic_and_signature)
    }

    fn basic_collection_member_signature(
        &mut self,
        self_type: &TypeRef,
        field_name_str: &str,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };
        let self_mut_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: true,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "len" => {
                let signature = Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().int(),
                };
                (IntrinsicFunction::VecLen, signature)
            }
            "is_empty" => (
                IntrinsicFunction::VecIsEmpty,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().bool(),
                },
            ),
            "clear" => {
                let signature = Signature {
                    parameters: vec![self_mut_type_param],
                    return_type: self.types().unit(),
                };
                (IntrinsicFunction::VecClear, signature)
            }
            "capacity" => {
                let signature = Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().int(),
                };
                (IntrinsicFunction::VecCapacity, signature)
            }
            _ => {
                self.add_err(ErrorKind::UnknownMemberFunction(self_type.clone()), node);

                return None;
            }
        };
        Some(intrinsic_and_signature)
    }
    #[allow(clippy::too_many_lines)]
    fn codepoint_member_signature(
        &mut self,
        self_type: &TypeRef,
        key_type: Option<&TypeRef>,
        element_type: &TypeRef,
        field_name_str: &str,
        lambda_variable_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };

        match field_name_str {
            "to_int" => Some((
                IntrinsicFunction::CodepointToInt,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().int(),
                },
            )),
            _ => None,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn byte_member_signature(
        &mut self,
        self_type: &TypeRef,
        key_type: Option<&TypeRef>,
        element_type: &TypeRef,
        field_name_str: &str,
        lambda_variable_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };

        match field_name_str {
            "to_int" => Some((
                IntrinsicFunction::ByteToInt,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().int(),
                },
            )),
            _ => None,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn string_member_signature(
        &mut self,
        self_type: &TypeRef,
        key_type: Option<&TypeRef>,
        element_type: &TypeRef,
        field_name_str: &str,
        lambda_variable_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        self.vec_member_signature(
            self_type,
            element_type,
            field_name_str,
            lambda_variable_count,
            node,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn slice_member_signature(
        &mut self,
        self_type: &TypeRef,
        key_type: Option<&TypeRef>,
        element_type: &TypeRef,
        field_name_str: &str,
        lambda_variable_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let slice_view_type = self.types().slice_view(&element_type.clone());
        let int_type = self.types().int();
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: slice_view_type.clone(),
            is_mutable: false,
            node: None,
        };
        let self_mut_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: slice_view_type,
            is_mutable: true,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "for" => {
                let parameters = if lambda_variable_count == 2 {
                    vec![
                        TypeForParameter {
                            name: "key".to_string(),
                            resolved_type: key_type.unwrap().clone(),
                            is_mutable: false,
                            node: None,
                        },
                        TypeForParameter {
                            name: "element".to_string(),
                            resolved_type: element_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ]
                } else {
                    vec![TypeForParameter {
                        name: "element".to_string(),
                        resolved_type: element_type.clone(),
                        is_mutable: false,
                        node: None,
                    }]
                };
                let lambda_signature = Signature {
                    parameters,
                    return_type: self.types().unit(),
                };
                let lambda_function_type = self.types().function(lambda_signature);
                (
                    IntrinsicFunction::TransformerFor,
                    Signature {
                        parameters: vec![
                            self_type_param,
                            TypeForParameter {
                                name: "lambda".to_string(),
                                resolved_type: lambda_function_type,
                                is_mutable: false,
                                node: None,
                            },
                        ],
                        return_type: self.types().unit(), // VecFor is only used for side effects
                    },
                )
            }
            "while" => {
                let parameters = if lambda_variable_count == 2 {
                    vec![
                        TypeForParameter {
                            name: "key".to_string(),
                            resolved_type: key_type.unwrap().clone(),
                            is_mutable: false,
                            node: None,
                        },
                        TypeForParameter {
                            name: "element".to_string(),
                            resolved_type: element_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ]
                } else {
                    vec![TypeForParameter {
                        name: "element".to_string(),
                        resolved_type: element_type.clone(),
                        is_mutable: false,
                        node: None,
                    }]
                };
                let lambda_signature = Signature {
                    parameters,
                    return_type: self.types().bool(), // it returns if the while loop should continue
                };
                let lambda_function_type = self.types().function(lambda_signature);
                (
                    IntrinsicFunction::TransformerWhile,
                    Signature {
                        parameters: vec![
                            self_type_param,
                            TypeForParameter {
                                name: "lambda".to_string(),
                                resolved_type: lambda_function_type,
                                is_mutable: false,
                                node: None,
                            },
                        ],
                        return_type: self.types().unit(), // VecFor is only used for side effects
                    },
                )
            }
            "filter" => {
                let lambda_signature = Signature {
                    parameters: vec![TypeForParameter {
                        name: "element".to_string(),
                        resolved_type: element_type.clone(),
                        is_mutable: false,
                        node: None,
                    }],
                    return_type: self.types().bool(),
                };
                let lambda_function_type = self.types().function(lambda_signature);
                (
                    IntrinsicFunction::TransformerFilter,
                    Signature {
                        parameters: vec![
                            self_type_param,
                            TypeForParameter {
                                name: "lambda".to_string(),
                                resolved_type: lambda_function_type,
                                is_mutable: false,
                                node: None,
                            },
                        ],
                        return_type: self.shared.state.types.slice_view(&element_type.clone()),
                    },
                )
            }
            "find" => {
                let lambda_signature = Signature {
                    parameters: vec![TypeForParameter {
                        name: "element".to_string(),
                        resolved_type: element_type.clone(),
                        is_mutable: false,
                        node: None,
                    }],
                    return_type: self.types().bool(),
                };
                let lambda_function_type = self.types().function(lambda_signature);
                (
                    IntrinsicFunction::TransformerFind,
                    Signature {
                        parameters: vec![
                            self_type_param,
                            TypeForParameter {
                                name: "lambda".to_string(),
                                resolved_type: lambda_function_type,
                                is_mutable: false,
                                node: None,
                            },
                        ],
                        return_type: self.shared.state.types.optional(&element_type.clone()),
                    },
                )
            }

            "remove" => {
                let signature = Signature {
                    parameters: vec![
                        self_mut_type_param,
                        TypeForParameter {
                            name: "index".to_string(),
                            resolved_type: int_type,
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().unit(),
                };
                (IntrinsicFunction::VecRemoveIndex, signature)
            }
            _ => return self.basic_collection_member_signature(self_type, field_name_str, node),
        };
        Some(intrinsic_and_signature)
    }

    #[allow(clippy::unnecessary_wraps, clippy::result_large_err)]
    fn map_member_signature(
        &mut self,
        self_type: &TypeRef,
        key_type: &TypeRef,
        value_type: &TypeRef,
        field_name_str: &str,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: false,
            node: None,
        };

        let mutable_self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: true,
            node: None,
        };

        let intrinsic_and_signature = match field_name_str {
            "has" => (
                IntrinsicFunction::MapHas,
                Signature {
                    parameters: vec![
                        self_type_param,
                        TypeForParameter {
                            name: "key".to_string(),
                            resolved_type: key_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().bool(),
                },
            ),
            "remove" => (
                IntrinsicFunction::MapRemove,
                Signature {
                    parameters: vec![
                        mutable_self_type_param,
                        TypeForParameter {
                            name: "key".to_string(),
                            resolved_type: key_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: self.types().unit(),
                },
            ),
            "len" => (
                IntrinsicFunction::MapLen,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().int(),
                },
            ),
            "capacity" => (
                IntrinsicFunction::MapCapacity,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().int(),
                },
            ),
            "is_empty" => (
                IntrinsicFunction::MapIsEmpty,
                Signature {
                    parameters: vec![self_type_param],
                    return_type: self.types().bool(),
                },
            ),
            _ => todo!("unknown map member"),
        };

        Some(intrinsic_and_signature)
    }

    fn check_intrinsic_member_signature(
        &mut self,
        type_that_member_is_on: &TypeRef,
        field_name_str: &str,
        lambda_variables_count: usize,
        node: &swamp_ast::Node,
    ) -> Option<(IntrinsicFunction, Signature)> {
        let ty = type_that_member_is_on;
        let int_type = self.types().int();
        match &*ty.kind {
            TypeKind::GridStorage(element_type, ..) | TypeKind::GridView(element_type) => self
                .grid_member_signature(type_that_member_is_on, element_type, field_name_str, node),
            TypeKind::SparseStorage(element_type, ..) | TypeKind::SparseView(element_type) => self
                .sparse_member_signature(
                    type_that_member_is_on,
                    element_type,
                    field_name_str,
                    lambda_variables_count,
                    node,
                ),
            TypeKind::QueueStorage(element_type, ..) => self.queue_member_signature(
                type_that_member_is_on,
                None,
                element_type,
                field_name_str,
                lambda_variables_count,
                node,
            ),
            TypeKind::StackStorage(element_type, ..)
            | TypeKind::QueueStorage(element_type, ..)
            | TypeKind::VecStorage(element_type, ..)
            | TypeKind::DynamicLengthVecView(element_type) => self.vec_member_signature(
                type_that_member_is_on,
                element_type,
                field_name_str,
                lambda_variables_count,
                node,
            ),
            TypeKind::SliceView(element_type) => self.slice_member_signature(
                type_that_member_is_on,
                Some(&int_type),
                element_type,
                field_name_str,
                lambda_variables_count,
                node,
            ),
            TypeKind::Byte => {
                let element_type = self.shared.state.types.byte();
                self.byte_member_signature(
                    type_that_member_is_on,
                    Some(&int_type),
                    &element_type,
                    field_name_str,
                    lambda_variables_count,
                    node,
                )
            }
            TypeKind::Codepoint => {
                let element_type = self.shared.state.types.codepoint();
                self.codepoint_member_signature(
                    type_that_member_is_on,
                    Some(&int_type),
                    &element_type,
                    field_name_str,
                    lambda_variables_count,
                    node,
                )
            }
            TypeKind::String { .. } | TypeKind::StringStorage(..) => {
                let element_type = self.shared.state.types.byte();
                self.string_member_signature(
                    type_that_member_is_on,
                    Some(&int_type),
                    &element_type,
                    field_name_str,
                    lambda_variables_count,
                    node,
                )
            }
            TypeKind::DynamicLengthMapView(key, value) | TypeKind::MapStorage(key, value, _) => {
                self.map_member_signature(type_that_member_is_on, key, value, field_name_str, node)
            }

            TypeKind::FixedCapacityAndLengthArray(element_type, _) => self.slice_member_signature(
                type_that_member_is_on,
                Some(&int_type),
                element_type,
                field_name_str,
                lambda_variables_count,
                node,
            ),
            _ => {
                self.add_err(
                    ErrorKind::UnknownMemberFunction(type_that_member_is_on.clone()),
                    node,
                );

                None
            }
        }
    }

    fn analyze_member_call(
        &mut self,
        type_that_member_is_on: &TypeRef,
        field_name_str: &str,
        ast_maybe_generic_arguments: Option<Vec<swamp_ast::GenericParameter>>,
        ast_arguments: &[swamp_ast::Expression],
        chain_self_is_mutable: bool,
        node: &swamp_ast::Node,
    ) -> (PostfixKind, TypeRef) {
        let generic_arguments = if let Some(ast_generic_arguments) = ast_maybe_generic_arguments {
            let mut resolved_types = Vec::new();
            for ast_type in ast_generic_arguments {
                resolved_types.push(self.analyze_type(ast_type.get_type(), &TypeAnalyzeContext::default()));
            }
            resolved_types
        } else {
            vec![]
        };

        let maybe_function = self
            .shared
            .state
            .associated_impls
            .get_member_function(type_that_member_is_on, field_name_str)
            .cloned();

        let (function_ref, instantiated_signature) = if let Some(found_function) = maybe_function {
            let signature = self.analyze_normal_member_call(
                type_that_member_is_on,
                &found_function,
                generic_arguments,
                ast_arguments,
                chain_self_is_mutable,
                node,
            );
            (found_function, signature)
        } else {
            let lambda_variables_count = if ast_arguments.is_empty() {
                0
            } else if let swamp_ast::ExpressionKind::Lambda(variables, ..) = &ast_arguments[0].kind
            {
                variables.len()
            } else {
                0
            };
            let Some((intrinsic_fn, signature)) = self.check_intrinsic_member_signature(
                type_that_member_is_on,
                field_name_str,
                lambda_variables_count,
                node,
            ) else {
                return (PostfixKind::OptionalChainingOperator, self.types().unit());
            };
            let def = IntrinsicFunctionDefinition {
                name: field_name_str.to_string(),
                signature,
                intrinsic: intrinsic_fn,
            };
            let function_ref = FunctionRef::from(Function::Intrinsic(
                IntrinsicFunctionDefinitionRef::from(def.clone()),
            ));
            (function_ref, def.signature)
        };

        let self_type_in_signature = &instantiated_signature.parameters[0];

        if self_type_in_signature.is_mutable && !chain_self_is_mutable {
            self.add_err(ErrorKind::SelfNotCorrectMutableState, node);
        }

        let resolved_arguments = self.analyze_and_verify_parameters(
            node,
            &instantiated_signature.parameters[1..],
            ast_arguments,
        );

        (
            PostfixKind::MemberCall(function_ref, resolved_arguments),
            TypeRef::from(instantiated_signature.return_type.clone()),
        )
    }

    fn analyze_postfix_member_call(
        &mut self,
        type_that_member_is_on: &TypeRef,
        is_mutable: bool,
        member_name: &swamp_ast::Node,
        ast_maybe_generic_arguments: Option<Vec<swamp_ast::GenericParameter>>,
        ast_arguments: &[swamp_ast::Expression],
        suffixes: &mut Vec<Postfix>,
    ) -> TypeRef {
        let field_name_str = self.get_text(member_name).to_string();

        let resolved_node = self.to_node(member_name);

        let (kind, return_type) = self.analyze_member_call(
            type_that_member_is_on,
            &field_name_str,
            ast_maybe_generic_arguments,
            ast_arguments,
            is_mutable,
            member_name,
        );
        let postfix = Postfix {
            node: resolved_node,
            ty: return_type,
            kind,
        };

        let last_type = postfix.ty.clone();
        suffixes.push(postfix);

        last_type
    }

    fn is_compatible_initializer_list_target(
        &mut self,
        target_type: &TypeRef,
        initializer_element_type: &TypeRef,
    ) -> bool {
        match &*target_type.kind {
            TypeKind::VecStorage(vec_element_type, _vec_capacity) => self
                .types()
                .compatible_with(vec_element_type, initializer_element_type),
            TypeKind::FixedCapacityAndLengthArray(array_element_type, _array_capacity) => self
                .types()
                .compatible_with(array_element_type, initializer_element_type),
            _ => false,
        }
    }

    fn is_compatible_initializer_pair_list_target(
        &mut self,
        target_type: &TypeRef,
        initializer_key_type: &TypeRef,
        initializer_value_type: &TypeRef,
    ) -> bool {
        match &*target_type.kind {
            TypeKind::MapStorage(storage_key, storage_value, _) => {
                self.types()
                    .compatible_with(initializer_key_type, storage_key)
                    && self
                    .types()
                    .compatible_with(initializer_value_type, storage_value)
            }
            _ => false,
        }
    }

    fn types_did_not_match_try_late_coerce_expression(
        &mut self,
        expr: Expression,
        special_expected_type: &TypeRef,
        special_encountered_type: &TypeRef,
        node: &swamp_ast::Node,
    ) -> Expression {
        let expected_type = special_expected_type;
        let encountered_type = special_encountered_type;

        if let TypeKind::Any = &*expected_type.kind {
            if encountered_type.is_storage() {
                let wrapped = self.create_expr(
                    ExpressionKind::CoerceToAny(Box::new(expr)),
                    expected_type.clone(),
                    node,
                );
                return wrapped;
            }
        }

        let encountered_is_optional = matches!(&*encountered_type.kind, TypeKind::Optional(_));
        if let TypeKind::Optional(expected_inner_type) = &*expected_type.kind {
            let inner_is_also_optional =
                matches!(&*expected_inner_type.kind, TypeKind::Optional(_));
            // If an optional is expected, we can wrap it if this type has the exact same
            // inner type
            assert!(!inner_is_also_optional);

            // First make sure it is not already an optional type. we can not wrap an option with an option
            // TODO: Improve error handling
            if !encountered_is_optional {
                // good it isn't, lets see if they share inner types
                if self
                    .types()
                    .compatible_with(expected_inner_type, encountered_type)
                {
                    // they share inner types as well, lets wrap it up
                    let wrapped = self.create_expr(
                        ExpressionKind::Option(Option::from(Box::new(expr))),
                        expected_type.clone(),
                        node,
                    );
                    return wrapped;
                }
            }
        }

        if matches!(&*expected_type.kind, &TypeKind::Bool) {
            // if it has a mut or immutable optional, then it works well to wrap it
            if encountered_is_optional {
                let bool_type = self.types().bool();
                let wrapped = self.create_expr(
                    ExpressionKind::CoerceOptionToBool(Box::from(expr)),
                    bool_type,
                    node,
                );
                return wrapped;
            }
        }

        if matches!(
            (&*expected_type.kind, &*encountered_type.kind),
            (TypeKind::Codepoint, TypeKind::Int)
        ) {
            let coerced = self.create_expr(
                ExpressionKind::CoerceIntToChar(Box::new(expr)),
                expected_type.clone(),
                node,
            );
            return coerced;
        }

        error!(?expected_type, ?encountered_type, "incompatible");
        self.create_err(
            ErrorKind::IncompatibleTypes {
                expected: expected_type.clone(),
                found: encountered_type.clone(),
            },
            node,
        )
    }

    #[must_use]
    pub fn analyze_generic_parameter_usize(&self, generic_parameter: &GenericParameter) -> usize {
        let usize_node = generic_parameter.get_unsigned_int_node();
        let usize_str = self.get_text(usize_node);
        Self::str_to_unsigned_int(usize_str).unwrap() as usize
    }

    #[must_use]
    pub fn analyze_generic_parameter_usize_tuple(
        &self,
        generic_parameter: &GenericParameter,
    ) -> (usize, usize) {
        let (first, second) = generic_parameter.get_unsigned_int_tuple_nodes();
        let first_str = self.get_text(first);
        let second_str = self.get_text(second);
        let first_value = Self::str_to_unsigned_int(first_str).unwrap() as usize;
        let second_value = Self::str_to_unsigned_int(second_str).unwrap() as usize;

        (first_value, second_value)
    }

    pub fn analyze_special_named_type(
        &mut self,
        path: &[String],
        name: &str,
        ast_generic_parameters: &[GenericParameter],
    ) -> Option<TypeRef> {
        let converted_type = match name {
            "Any" => {
                let new_type = self.shared.state.types.any();
                let default_node = swamp_ast::Node::default();
                new_type
            }
            "String" => {
                if ast_generic_parameters.len() == 1 {
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[0]);
                    let new_type = self.shared.state.types.string_storage(fixed_size);
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&new_type, &default_node);
                    new_type
                } else {
                    return None;
                }
            }
            "Vec" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let vec_type = self.shared.state.types.dynamic_vec_view(&element_type);
                    // Generate default functions for the new dynamic vec view type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&vec_type, &default_node);
                    vec_type
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    let vec_storage_type = self
                        .shared
                        .state
                        .types
                        .vec_storage(&element_type, fixed_size);
                    // Generate default functions for the new vec storage type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&vec_storage_type, &default_node);
                    vec_storage_type
                } else {
                    panic!("todo: make this into an error")
                }
            }
            "Stack" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let stack_view_type = self.shared.state.types.stack_view(&element_type);
                    // Generate default functions for the new stack view type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&stack_view_type, &default_node);
                    stack_view_type
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    let stack_storage_type = self
                        .shared
                        .state
                        .types
                        .stack_storage(&element_type, fixed_size);
                    // Generate default functions for the new stack storage type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&stack_storage_type, &default_node);
                    stack_storage_type
                } else {
                    panic!("todo: make this into an error")
                }
            }
            "Queue" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let queue_view_type = self.shared.state.types.queue_view(&element_type);
                    // Generate default functions for the new queue view type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&queue_view_type, &default_node);
                    queue_view_type
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    let queue_storage_type = self
                        .shared
                        .state
                        .types
                        .queue_storage(&element_type, fixed_size);
                    // Generate default functions for the new queue storage type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&queue_storage_type, &default_node);
                    queue_storage_type
                } else {
                    panic!("todo: make this into an error")
                }
            }
            "Sparse" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let sparse_view_type = self.shared.state.types.sparse_view(&element_type);
                    // Generate default functions for the new sparse view type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&sparse_view_type, &default_node);
                    sparse_view_type
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    let sparse_storage_type = self
                        .shared
                        .state
                        .types
                        .sparse_storage(&element_type, fixed_size);
                    // Generate default functions for the new sparse storage type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&sparse_storage_type, &default_node);
                    sparse_storage_type
                } else {
                    panic!("todo: make this into an error")
                }
            }

            "Grid" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let grid_view_type = self.shared.state.types.grid_view(&element_type);
                    // Generate default functions for the new grid view type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&grid_view_type, &default_node);
                    grid_view_type
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type(), &TypeAnalyzeContext::default());
                    let (width, height) =
                        self.analyze_generic_parameter_usize_tuple(&ast_generic_parameters[1]);
                    let grid_storage_type =
                        self.shared
                            .state
                            .types
                            .grid_storage(&element_type, width, height);
                    // Generate default functions for the new grid storage type
                    let default_node = swamp_ast::Node::default();
                    self.add_default_functions(&grid_storage_type, &default_node);
                    grid_storage_type
                } else {
                    panic!("todo: make this into an error")
                }
            }

            _ => return None,
        };

        Some(converted_type)
    }

    fn special_static_member(
        &self,
        type_identifier: &QualifiedTypeIdentifier,
        member_name_node: &swamp_ast::Node,
    ) -> Option<Function> {
        if type_identifier.generic_params.is_empty() {
            return None;
        }

        if type_identifier.module_path.is_some() {
            return None;
        }

        let member_name = self.get_text(member_name_node);
        let name = self.get_text(&type_identifier.name.0);

        match name {
            "Stack" => None,
            _ => None,
        }
    }

    fn analyze_subscript_int(&mut self, collection_type: TypeRef, unsigned_int_expression: Expression) -> (Postfix, TypeRef) {
        let node = &unsigned_int_expression.node;
        match &*collection_type.kind {
            TypeKind::QueueStorage(element_type, _)
            | TypeKind::StackStorage(element_type, _)
            | TypeKind::StackView(element_type)
            | TypeKind::VecStorage(element_type, _)
            | TypeKind::StringStorage(element_type, _, _)
            | TypeKind::String(element_type, _)
            | TypeKind::FixedCapacityAndLengthArray(element_type, _)
            | TypeKind::DynamicLengthVecView(element_type)
            | TypeKind::SliceView(element_type) => {
                let vec_type = VecType {
                    element: element_type.clone(),
                };

                let postfix = Postfix {
                    node: node.clone(),
                    ty: collection_type.clone(),
                    kind: PostfixKind::VecSubscript(
                        vec_type,
                        unsigned_int_expression,
                    ),
                };

                (postfix, element_type.clone())
            }
            // Sparse
            TypeKind::SparseStorage(element_type, _)
            | TypeKind::SparseView(element_type) => {
                let sparse_type = SparseType {
                    element: element_type.clone(),
                };


                let postfix = Postfix {
                    node: node.clone(),
                    ty: collection_type.clone(),
                    kind: PostfixKind::SparseSubscript(
                        sparse_type,
                        unsigned_int_expression,
                    ),
                };

                (postfix, element_type.clone())
            }

            _ => {
                self
                    .add_err_resolved(ErrorKind::CanNotSubscriptWithThatType, node);

                let error_vec_type = VecType {
                    element: self.types().unit(),
                };

                let error_postfix = Postfix {
                    node: node.clone(),
                    ty: collection_type.clone(),
                    kind: PostfixKind::VecSubscript(
                        error_vec_type,
                        unsigned_int_expression,
                    ),
                };

                (error_postfix, self.types().unit())
            }
        }
    }


    fn analyze_subscript_range(&mut self, collection_type: TypeRef, range_expression: Expression) -> (Postfix, TypeRef) {
        let node = &range_expression.node;
        match &*collection_type.kind {
            TypeKind::QueueStorage(element_type, _)
            | TypeKind::StackStorage(element_type, _)
            | TypeKind::StackView(element_type)
            | TypeKind::VecStorage(element_type, _)
            | TypeKind::StringStorage(element_type, _, _)
            | TypeKind::String(element_type, _)
            | TypeKind::FixedCapacityAndLengthArray(element_type, _)
            | TypeKind::DynamicLengthVecView(element_type)
            | TypeKind::SliceView(element_type) => {
                let vec_type = VecType {
                    element: element_type.clone(),
                };

                let postfix = Postfix {
                    node: node.clone(),
                    ty: collection_type.clone(),
                    kind: PostfixKind::VecSubscriptRange(
                        vec_type,
                        range_expression,
                    ),
                };

                // A range subscript returns the same type again
                (postfix, collection_type.clone())
            }

            _ => {
                self
                    .add_err_resolved(ErrorKind::CanNotSubscriptWithThatType, node);

                let error_vec_type = VecType {
                    element: self.types().unit(),
                };

                let error_postfix = Postfix {
                    node: node.clone(),
                    ty: collection_type.clone(),
                    kind: PostfixKind::VecSubscriptRange(
                        error_vec_type,
                        range_expression,
                    ),
                };

                (error_postfix, self.types().unit())
            }
        }
    }
    fn analyze_map_subscript(&mut self, key_type: &TypeRef, value_type: &TypeRef, lookup_expr: &swamp_ast::Expression) -> (Postfix, TypeRef) {
        let key_context = TypeContext::new_argument(key_type, false);
        let key_expression = self.analyze_expression(lookup_expr, &key_context);

        let map_type = MapType {
            key: key_type.clone(),
            value: value_type.clone(),
        };

        let postfix = Postfix {
            node: self.to_node(&lookup_expr.node),
            ty: key_type.clone(),
            kind: PostfixKind::MapSubscript(map_type, key_expression),
        };

        (postfix, value_type.clone())
    }
}
