/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod access;
mod attributes;
pub mod call;
pub mod constant;
pub mod def;
pub mod err;
pub mod literal;
pub mod operator;
pub mod pattern;
pub mod prelude;
mod structure;
pub mod types;
pub mod variable;
use crate::call::MaybeBorrowMutRefExpression;
use crate::err::{Error, ErrorKind};
use seq_map::SeqMap;
use source_map_cache::SourceMap;
use source_map_node::{FileId, Node, Span};
use std::mem::take;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use std::str::{FromStr, ParseBoolError};
use swamp_ast::{GenericParameter, QualifiedTypeIdentifier};
use swamp_attributes::Attributes;
use swamp_modules::prelude::*;
use swamp_modules::symtbl::SymbolTableRef;
use swamp_semantic::prelude::*;
use swamp_semantic::{
    ArgumentExpression, BinaryOperatorKind, BlockScope, BlockScopeMode, FunctionScopeState,
    GridType, InternalMainExpression, LocationAccess, LocationAccessKind, MapType,
    MutableReferenceKind, NormalPattern, Postfix, PostfixKind, SingleLocationExpression,
    SliceViewType, SparseType, TargetAssignmentLocation, TypeWithMut, VariableType, VecType,
    WhenBinding,
};
use swamp_semantic::{StartOfChain, StartOfChainKind};
use swamp_types::TypeKind;
use swamp_types::prelude::*;
use tracing::trace;
use tracing::{error, info};

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

/// `TypeRef` checking context
#[derive(Debug, Clone)]
pub struct TypeContext<'a> {
    /// Expected type for the current expression
    pub expected_type: Option<&'a TypeRef>,
}

impl<'a> TypeContext<'a> {
    #[must_use]
    pub const fn new(expected_type: Option<&'a TypeRef>) -> Self {
        Self { expected_type }
    }

    #[must_use]
    pub const fn new_argument(required_type: &'a TypeRef) -> Self {
        Self {
            expected_type: Some(required_type),
        }
    }

    #[must_use]
    pub const fn new_unsure_argument(expected_type: Option<&'a TypeRef>) -> Self {
        Self { expected_type }
    }

    #[must_use]
    pub const fn new_anything_argument() -> Self {
        Self {
            expected_type: None,
        }
    }

    #[must_use]
    pub const fn new_function(required_type: &'a TypeRef) -> Self {
        Self {
            expected_type: Some(required_type),
        }
    }

    #[must_use]
    pub const fn with_expected_type(&self, expected_type: Option<&'a TypeRef>) -> Self {
        Self { expected_type }
    }

    pub(crate) const fn we_know_expected_type(&self, found_type: &'a TypeRef) -> Self {
        self.with_expected_type(Some(found_type))
    }
}

pub struct SharedState<'a> {
    pub state: &'a mut ProgramState,
    pub lookup_table: SymbolTable,
    pub definition_table: SymbolTable,
    pub modules: &'a Modules,
    pub source_map: &'a SourceMap,
    pub file_id: FileId,
    pub core_symbol_table: SymbolTableRef,
}

impl<'a> SharedState<'a> {
    #[must_use]
    pub fn get_symbol_table(&'a self, path: &[String]) -> Option<&'a SymbolTable> {
        if path.is_empty() {
            return Some(&self.lookup_table);
        }
        self.get_module(path).map(|module| &module.symbol_table)
    }

    #[must_use]
    pub fn get_module(&'a self, path: &[String]) -> Option<&'a ModuleRef> {
        let resolved_path = {
            self.lookup_table.get_package_version(&path[0]).map_or_else(
                || path.to_vec(),
                |found_version| {
                    let mut new_path = path.to_vec();
                    let complete_name = format!("{}-{found_version}", path[0]);
                    new_path[0] = complete_name;
                    new_path
                    //path.to_vec()
                },
            )
        };

        if path.len() == 1 {
            if let Some(module_ref) = self.lookup_table.get_module_link(&path[0]) {
                return Some(module_ref);
            }
        }

        if let Some(x) = self.modules.get(&resolved_path) {
            return Some(x);
        }

        None
    }
}

pub struct Analyzer<'a> {
    pub shared: SharedState<'a>,
    scope: FunctionScopeState,
    function_variables: Vec<VariableRef>,
    function_parameters: Vec<VariableRef>,
    global: FunctionScopeState,
    module_path: Vec<String>,
    errors: Vec<Error>,
}

impl Analyzer<'_> {
    #[must_use]
    pub const fn scopes(&self) -> &FunctionScopeState {
        &self.scope
    }
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
        Self {
            scope: FunctionScopeState::new(),
            global: FunctionScopeState::new(),
            shared,
            module_path: module_path.to_vec(),
            function_variables: Vec::new(),
            function_parameters: Vec::new(),
            errors: vec![],
        }
    }

    fn start_function(&mut self) {
        self.global.block_scope_stack = take(&mut self.scope.block_scope_stack);
        self.scope = FunctionScopeState::new();
        self.function_variables.clear();
        self.function_parameters.clear();
    }

    fn stop_function(&mut self) {
        self.scope.block_scope_stack = take(&mut self.global.block_scope_stack);
        self.function_variables.clear();
        self.function_parameters.clear();
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
            let else_context = branch_context.with_expected_type(detected.as_ref());
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
            Some(x) => self.analyze_type(x),
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
            Some(ast_type) => self.analyze_type(ast_type),
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

    fn analyze_parameters(
        &mut self,
        parameters: &Vec<swamp_ast::Parameter>,
    ) -> Vec<TypeForParameter> {
        let mut resolved_parameters = Vec::new();
        for parameter in parameters {
            let param_type = self.analyze_type(&parameter.param_type);
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

        if let Some(found_table) = self.shared.get_symbol_table(&path) {
            if let Some(found_func) = found_table.get_function(function_name) {
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
        info!(%debug_line, "source");

        //        info!(?line, ?col, "source reference");
    }

    /// # Errors
    ///
    pub fn analyze_main_expression(
        &mut self,
        ast_expression: &swamp_ast::Expression,
    ) -> InternalMainExpression {
        self.start_function();

        let context = TypeContext::new_anything_argument();
        let analyzed_expr = self.analyze_expression(ast_expression, &context);
        let main_expr = InternalMainExpression {
            expression: analyzed_expr,
            function_variables: self.function_variables.clone(),
            function_parameters: self.function_parameters.clone(),
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
        {
            if let swamp_ast::UnaryOperator::BorrowMutRef(node) = found_unary {
                //let inner = self.analyze_expression(ast_inner_expression, context)?;
                let resolved_node = self.to_node(node);
                return MaybeBorrowMutRefExpression {
                    ast_expression: *ast_inner_expression.clone(),
                    has_borrow_mutable_reference: Some(resolved_node),
                };
            }
        }

        MaybeBorrowMutRefExpression {
            ast_expression: ast_expr.clone(),
            has_borrow_mutable_reference: None,
        }
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression(
        &mut self,
        ast_expression: &swamp_ast::Expression,
        context: &TypeContext,
    ) -> Expression {
        info!(?ast_expression, "analyze expression");
        self.debug_expression(ast_expression, "analyze");
        let expr = self.analyze_expression_internal(ast_expression, context);

        let encountered_type = expr.ty.clone();

        info!(?expr, "analyze expression");
        if let Some(found_expected_type) = context.expected_type {
            let reduced_expected = found_expected_type;

            let reduced_encountered_type = encountered_type;
            if self
                .shared
                .state
                .types
                .compatible_with(reduced_expected, &reduced_encountered_type)
            {
                return expr;
            }

            self.types_did_not_match_try_late_coerce_expression(
                expr,
                reduced_expected,
                &reduced_encountered_type,
                &ast_expression.node,
            )
        } else {
            expr
            //todo!()
            // TODO: self.coerce_unrestricted_type(&ast_expression.node, expr)?
        }
    }

    fn new_from_slice(&mut self, analyzed_element_type: &TypeRef) -> Expression {
        todo!()
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
                self.analyze_postfix_chain(postfix_chain)
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
                panic!("can not have separate member func ref {type_name:?} {debug_name}")
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
                self.analyze_named_struct_literal(struct_identifier, fields, *has_rest)
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
                self.analyze_literal(&ast_expression.node, literal, context)
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
            let ty = self.analyze_type(analyzed_type.get_type());

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
            panic!("what is this {type_name_to_find:?}")
        }
    }

    fn create_default_value_for_type(
        &mut self,
        node: &swamp_ast::Node,
        field_type: &TypeRef,
    ) -> Expression {
        let kind = match &*field_type.kind {
            TypeKind::Bool => ExpressionKind::BoolLiteral(false),
            TypeKind::Int => ExpressionKind::IntLiteral(0),
            TypeKind::Float => ExpressionKind::FloatLiteral(Fp::zero()),
            TypeKind::String => ExpressionKind::StringLiteral(String::new()),
            TypeKind::Tuple(tuple_type_ref) => {
                let mut expressions = Vec::new();
                for resolved_type in tuple_type_ref {
                    let expr = self.create_default_value_for_type(node, resolved_type);
                    expressions.push(expr);
                }
                ExpressionKind::TupleLiteral(expressions)
            }
            TypeKind::Optional(_optional_type) => ExpressionKind::NoneLiteral,

            TypeKind::NamedStruct(struct_ref) => self.create_default_static_call(node, field_type),
            _ => {
                return self.create_err(ErrorKind::NoDefaultImplemented(field_type.clone()), node);
            }
        };

        self.create_expr(kind, field_type.clone(), node)
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
        if let TypeKind::AnonymousStruct(anon_struct) = &*anon_struct_ref.kind {
            if let Some(found_field) = anon_struct.field_name_sorted_fields.get(&field_name_str) {
                let index = anon_struct
                    .field_name_sorted_fields
                    .get_index(&field_name_str)
                    .expect("checked earlier");

                return (anon_struct.clone(), index, found_field.field_type.clone());
            }
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
    ) -> Expression {
        let signature = if let Some(found_generic_arguments) = maybe_generic_arguments {
            // TODO:
            func_def.signature().clone()
        } else {
            func_def.signature().clone()
        };

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

        self.create_expr(expr_kind, signature.return_type.clone(), ast_node)
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_postfix_chain(&mut self, chain: &swamp_ast::PostfixChain) -> Expression {
        let maybe_start_of_chain_base =
            self.analyze_start_chain_expression_get_mutability(&chain.base);

        let mut start_index = 0;

        let start_of_chain_kind = if let Some(start_of_chain_base) = maybe_start_of_chain_base {
            trace!(?start_of_chain_base, "start of postfix chain");
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
            let ctx = TypeContext::new_anything_argument();
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

        let mut suffixes = Vec::new();

        for item in &chain.postfixes[start_index..] {
            trace!(?item, "postfix");
            match item {
                /*
                swamp_ast::Postfix::AdvancedFunctionCall(..) => {
                    todo!("AdvancedFunctionCall")
                }

                 */
                swamp_ast::Postfix::FieldAccess(field_name) => {
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
                    let collection_type = tv.resolved_type.clone();
                    match &*collection_type.kind {
                        TypeKind::GridStorage(element_type, x, _) => {
                            let int_type = self.shared.state.types.int();
                            let unsigned_int_x_context = TypeContext::new_argument(&int_type);
                            let unsigned_int_x_expression =
                                self.analyze_expression(col_expr, &unsigned_int_x_context);

                            let unsigned_int_y_context = TypeContext::new_argument(&int_type);
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
                        _ => panic!("not a subscript tuple"),
                    }
                }

                swamp_ast::Postfix::Subscript(lookup_expr) => {
                    let collection_type = tv.resolved_type.clone();
                    match &*collection_type.kind {
                        TypeKind::FixedCapacityAndLengthArray(element_type_in_slice, _) => {
                            let int_type = self.shared.state.types.int();
                            let unsigned_int_context = TypeContext::new_argument(&int_type);
                            let unsigned_int_expression =
                                self.analyze_expression(lookup_expr, &unsigned_int_context);

                            let slice_type = SliceViewType {
                                element: element_type_in_slice.clone(),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::SliceViewSubscript(
                                    slice_type,
                                    unsigned_int_expression,
                                ),
                                collection_type.clone(),
                                &lookup_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = element_type_in_slice.clone();
                        }
                        TypeKind::QueueStorage(element_type, _)
                        | TypeKind::StackStorage(element_type, _)
                        | TypeKind::StackView(element_type)
                        | TypeKind::VecStorage(element_type, _)
                        | TypeKind::DynamicLengthVecView(element_type)
                        | TypeKind::SliceView(element_type) => {
                            let int_type = self.shared.state.types.int();
                            let unsigned_int_context = TypeContext::new_argument(&int_type);
                            let unsigned_int_expression =
                                self.analyze_expression(lookup_expr, &unsigned_int_context);

                            let vec_type = VecType {
                                element: element_type.clone(),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::VecSubscript(vec_type, unsigned_int_expression),
                                element_type.clone(),
                                &lookup_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = element_type.clone();
                        }
                        TypeKind::SparseStorage(element_type, _)
                        | TypeKind::SparseView(element_type) => {
                            let int_type = self.shared.state.types.int();
                            let unsigned_int_context = TypeContext::new_argument(&int_type);
                            let unsigned_int_expression =
                                self.analyze_expression(lookup_expr, &unsigned_int_context);

                            let vec_type = SparseType {
                                element: element_type.clone(),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::SparseSubscript(vec_type, unsigned_int_expression),
                                element_type.clone(),
                                &lookup_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = element_type.clone();
                        }
                        TypeKind::MapStorage(key_type, value_type, _)
                        | TypeKind::DynamicLengthMapView(key_type, value_type) => {
                            let key_context = TypeContext::new_argument(key_type);
                            let key_expression = self.analyze_expression(lookup_expr, &key_context);

                            let map_type = MapType {
                                key: key_type.clone(),
                                value: value_type.clone(),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::MapSubscript(map_type, key_expression),
                                value_type.clone(),
                                &lookup_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = value_type.clone();
                        }
                        _ => {
                            eprintln!("xwhat is this: {collection_type:?}");
                            todo!()
                        }
                    }

                    /*

                    let temp_lookup_context = TypeContext::new_anything_argument();
                    let temp_analyzed_expr =
                        self.analyze_expression(index_expr, &temp_lookup_context)?;

                    let mut subscript_function_name = "subscript";

                    if let TypeKind::NamedStruct(named_struct) = temp_analyzed_expr.ty {
                        if named_struct.assigned_name == "Range"
                            && named_struct.module_path == vec!["core-0.0.0".to_string()]
                        {
                            subscript_function_name = "subscript_range";
                        }
                    };

                    if let Some(found) = self
                        .shared
                        .state
                        .instantiator
                        .associated_impls
                        .get_member_function(&tv.resolved_type, subscript_function_name)
                        .cloned()
                    {
                        let cloned = found.clone();
                        let required_type = &found.signature().parameters[1].resolved_type;
                        let subscript_lookup_context = TypeContext::new_argument(&required_type);
                        let analyzed_expr =
                            self.analyze_expression(index_expr, &subscript_lookup_context)?;

                        let return_type = *found.signature().return_type.clone();

                        let argument = MutRefOrImmutableExpression::Expression(analyzed_expr);
                        self.add_postfix(
                            &mut suffixes,
                            PostfixKind::MemberCall(cloned, vec![argument]),
                            return_type.clone(),
                            &index_expr.node,
                        );
                        tv.resolved_type = return_type.clone();
                    } else {
                        return self.create_err(ErrorKind::MissingSubscriptMember, &index_expr.node);
                    }

                     */
                }

                swamp_ast::Postfix::MemberCall(member_name, generic_arguments, ast_arguments) => {
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
                    /*
                    if let Some(_found_member) = self
                        .shared
                        .state
                        .instantiator
                        .associated_impls
                        .get_member_function(&underlying_type, &member_name_str)
                    {
                        let return_type = self.analyze_postfix_member_call(
                            &tv.resolved_type,
                            tv.is_mutable,
                            member_name,
                            generic_arguments.clone(),
                            ast_arguments,
                            &mut suffixes,
                        )?;

                        //self.add_postfix(&mut suffixes, kind, return_type.clone(), member_name);
                        tv.resolved_type = return_type.clone();
                        tv.is_mutable = false;
                    } else {
                        return self.create_err(
                            ErrorKind::UnknownMemberFunction(underlying_type.clone()),
                            member_name,
                        );
                    }

                     */
                }
                swamp_ast::Postfix::FunctionCall(node, _generic_arguments, arguments) => {
                    /*
                    if let TypeKind::Function(signature) = &*tv.resolved_type.kind {
                        let resolved_node = self.to_node(node);
                        let resolved_arguments = self.analyze_and_verify_parameters(
                            &resolved_node,
                            &signature.parameters,
                            arguments,
                        )?;

                        let call_kind = PostfixKind::FunctionCall(resolved_arguments);

                        self.add_postfix(
                            &mut suffixes,
                            call_kind,
                            *signature.return_type.clone(),
                            node,
                        );

                        tv.resolved_type = *signature.return_type.clone();
                        tv.is_mutable = false;

                    } else {
                        panic!("{}", &format!("what is this type {:?} ", tv.resolved_type))
                    }

                     */
                    panic!("can only have function call at the start of a postfix chain")
                }

                swamp_ast::Postfix::NoneCoalescingOperator(default_expr) => {
                    let unwrapped_type = if let TypeKind::Optional(unwrapped_type) =
                        &*tv.resolved_type.kind
                    {
                        unwrapped_type
                    } else if uncertain {
                        &tv.resolved_type
                    } else {
                        return self.create_err(ErrorKind::CanNotNoneCoalesce, &default_expr.node);
                    };

                    let unwrapped_type_context = TypeContext::new_argument(unwrapped_type);
                    let resolved_default_expr =
                        self.analyze_expression(default_expr, &unwrapped_type_context);
                    self.add_postfix(
                        &mut suffixes,
                        PostfixKind::NoneCoalescingOperator(resolved_default_expr),
                        (*unwrapped_type).clone(),
                        &default_expr.node,
                    );
                    tv.resolved_type = (*unwrapped_type).clone();
                    uncertain = false; // the chain is safe, because this will always solve None
                }

                swamp_ast::Postfix::OptionalChainingOperator(option_node) => {
                    if let TypeKind::Optional(unwrapped_type) = &*tv.resolved_type.kind {
                        uncertain = true;
                        self.add_postfix(
                            &mut suffixes,
                            PostfixKind::OptionalChainingOperator,
                            (*unwrapped_type).clone(),
                            option_node,
                        );
                        tv.resolved_type = (*unwrapped_type).clone();
                    } else {
                        return self.create_err(ErrorKind::ExpectedOptional, option_node);
                    }
                }
            }
        }

        if uncertain {
            if let TypeKind::Optional(_) = &*tv.resolved_type.kind {
            } else {
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
        let bool_context = TypeContext::new_argument(&bool_type);
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

    fn generate_to_string_for_named_struct(
        &mut self,
        named: &NamedStructType,
        self_expression: Expression,
    ) -> Expression {
        let node = self_expression.node.clone();
        let struct_name_string_kind =
            ExpressionKind::StringLiteral(format!("{} ", named.assigned_name.clone()));
        let string_type = self.types().string();
        let struct_name_string_expr =
            self.create_expr_resolved(struct_name_string_kind, string_type, &node);
        let anon_struct_string_expr =
            self.generate_to_string_for_anon_struct(&named.anon_struct_type, self_expression);

        let concat_kind = BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(struct_name_string_expr),
            right: Box::new(anon_struct_string_expr),
            node: node.clone(),
        };

        let string_type = self.types().string();
        self.create_expr_resolved(ExpressionKind::BinaryOp(concat_kind), string_type, &node)
    }

    fn generate_to_string_for_anon_struct(
        &mut self,
        anonymous_struct_type_ref: &TypeRef,
        self_expression: Expression,
    ) -> Expression {
        let node = self_expression.node.clone();
        let string_type = self.types().string();

        // Create opening brace string
        let opening_kind = ExpressionKind::StringLiteral("{ ".to_string());
        let mut result_expr = self.create_expr_resolved(opening_kind, string_type.clone(), &node);

        let TypeKind::AnonymousStruct(anonymous_struct_type) = &*anonymous_struct_type_ref.kind
        else {
            return self
                .create_err_resolved(ErrorKind::UnknownStructTypeReference, &self_expression.node);
        };

        // Process each field
        for (field_index, (field_name, field_type)) in anonymous_struct_type
            .field_name_sorted_fields
            .iter()
            .enumerate()
        {
            // If not the first field, add a comma separator
            if field_index > 0 {
                let separator_kind = ExpressionKind::StringLiteral(", ".to_string());
                let separator_expr =
                    self.create_expr_resolved(separator_kind, string_type.clone(), &node);

                // Concatenate using + operator
                let concat_kind = BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_expr),
                    right: Box::new(separator_expr),
                    node: node.clone(),
                };
                result_expr = self.create_expr_resolved(
                    ExpressionKind::BinaryOp(concat_kind),
                    string_type.clone(),
                    &node,
                );
            }

            // Add field name
            let field_name_kind = ExpressionKind::StringLiteral(format!("{field_name}: "));
            let field_name_expr =
                self.create_expr_resolved(field_name_kind, string_type.clone(), &node);

            // Concatenate field name to result
            let concat_name_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_expr),
                right: Box::new(field_name_expr),
                node: node.clone(),
            };

            result_expr = self.create_expr_resolved(
                ExpressionKind::BinaryOp(concat_name_kind),
                string_type.clone(),
                &node,
            );

            // Get field value from the struct
            let postfix_kind =
                PostfixKind::StructField(anonymous_struct_type_ref.clone(), field_index);
            let postfix_lookup_field_in_self = Postfix {
                node: node.clone(),
                ty: field_type.field_type.clone(),
                kind: postfix_kind,
            };

            // Get to_string function for the field type
            if let Some(to_string_fn) = self
                .shared
                .state
                .associated_impls
                .get_internal_member_function(&field_type.field_type, "to_string")
            {
                let function_ref = Function::Internal(to_string_fn.clone());

                // Create call to to_string for the field
                let postfix_call_to_string = Postfix {
                    node: node.clone(),
                    ty: self.types().string(),
                    kind: PostfixKind::MemberCall(FunctionRef::from(function_ref), vec![]),
                };

                // Create chain to access field and call to_string
                let start_of_chain = StartOfChain {
                    kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                    node: node.clone(),
                };

                let lookup_kind = ExpressionKind::PostfixChain(
                    start_of_chain,
                    vec![postfix_lookup_field_in_self, postfix_call_to_string],
                );

                let field_value_expr =
                    self.create_expr_resolved(lookup_kind, string_type.clone(), &node);

                // Concatenate field value to result
                let concat_value_kind = BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_expr),
                    right: Box::new(field_value_expr),
                    node: node.clone(),
                };
                result_expr = self.create_expr_resolved(
                    ExpressionKind::BinaryOp(concat_value_kind),
                    string_type.clone(),
                    &node,
                );
            }
        }

        // Create closing brace string
        let closing_kind = ExpressionKind::StringLiteral(" }".to_string());
        let closing_expr = self.create_expr_resolved(closing_kind, string_type.clone(), &node);

        // Concatenate closing brace to result
        let final_concat_kind = BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(result_expr),
            right: Box::new(closing_expr),
            node: node.clone(),
        };

        self.create_expr_resolved(
            ExpressionKind::BinaryOp(final_concat_kind),
            string_type,
            &node,
        )
    }

    fn generate_to_string_for_enum(
        &mut self,
        enum_type: &EnumType,
        argument_expression: Expression,
    ) -> Expression {
        let node = argument_expression.node.clone();
        let mut arms = Vec::new();
        let string_type = self.types().string();
        for (variant_name, variant_type) in &enum_type.variants {
            let kind = ExpressionKind::StringLiteral(variant_name.clone());
            let string_expr = self.create_expr_resolved(kind.clone(), string_type.clone(), &node);

            let arm_kind = MatchArm {
                pattern: Pattern::Normal(
                    NormalPattern::EnumPattern(variant_type.clone(), None),
                    None,
                ),
                expression: Box::new(string_expr.clone()),
                expression_type: self.types().int(),
            };
            arms.push(arm_kind);
        }

        self.create_expr_resolved(
            ExpressionKind::Match(Match {
                arms,
                expression: Box::new(argument_expression),
            }),
            string_type,
            &node,
        )
    }

    pub fn generate_to_string_function_for_type(
        &mut self,
        ty: &TypeRef,
        node: &swamp_ast::Node,
    ) -> InternalFunctionDefinition {
        let resolved_node = self.to_node(node);
        let string_type = self.types().string();
        let variable = Variable {
            name: resolved_node.clone(),
            assigned_name: "self".to_string(),
            resolved_type: ty.clone(),
            mutable_node: None,
            variable_type: VariableType::Parameter,

            scope_index: 0,
            variable_index: 0,

            unique_id_within_function: 0,
            is_unused: false,
        };

        let variable_ref = Rc::new(variable);

        let first_self_param = self.create_expr_resolved(
            ExpressionKind::VariableAccess(variable_ref.clone()),
            string_type,
            &resolved_node,
        );

        let body_expr = match &*ty.kind {
            TypeKind::Int => todo!(),
            TypeKind::Float => todo!(),
            TypeKind::String => todo!(),
            TypeKind::Bool => todo!(),
            TypeKind::Unit => todo!(),
            TypeKind::Tuple(_) => todo!(),
            TypeKind::NamedStruct(named) => {
                self.generate_to_string_for_named_struct(named, first_self_param)
            }
            TypeKind::AnonymousStruct(_anon_struct) => {
                self.generate_to_string_for_anon_struct(ty, first_self_param)
            }
            TypeKind::Range(_) => todo!(),
            TypeKind::Enum(enum_type) => {
                self.generate_to_string_for_enum(&enum_type.clone(), first_self_param)
            }
            TypeKind::Function(_) => todo!(),
            TypeKind::Optional(_) => todo!(),
            TypeKind::FixedCapacityAndLengthArray(_, _) => todo!(),
            TypeKind::SliceView(_) => todo!(),
            TypeKind::DynamicLengthVecView(_) => todo!(),
            TypeKind::VecStorage(_, _) => todo!(),
            TypeKind::SparseStorage(_, _) => todo!(),
            TypeKind::GridStorage(_, _, _) => todo!(),
            TypeKind::StackView(_) => todo!(),
            TypeKind::QueueView(_) => todo!(),
            TypeKind::GridView(_) => todo!(),
            TypeKind::SparseView(_) => todo!(),
            TypeKind::StackStorage(_, _) => todo!(),
            TypeKind::QueueStorage(_, _) => todo!(),
            TypeKind::MapStorage(_, _, _) => todo!(),
            TypeKind::DynamicLengthMapView(_, _) => todo!(),
        };

        let unique_function_id = self.shared.state.allocate_internal_function_id();

        InternalFunctionDefinition {
            body: body_expr,
            name: LocalIdentifier(resolved_node),
            assigned_name: "to_string".to_string(),
            associated_with_type: Option::from(ty.clone()),
            defined_in_module_path: self.module_path.clone(),
            signature: Signature {
                parameters: vec![TypeForParameter {
                    name: "self".to_string(),
                    resolved_type: ty.clone(),
                    is_mutable: false,
                    node: None,
                }],
                return_type: self.shared.state.types.string(),
            },
            parameters: vec![variable_ref],
            function_variables: vec![],
            program_unique_id: unique_function_id,
            attributes: Attributes::default(),
        }
    }

    fn analyze_iterable(
        &mut self,
        mut_requested_for_value_variable: Option<swamp_ast::Node>,
        expression: &swamp_ast::Expression,
    ) -> Iterable {
        let any_context = TypeContext::new_anything_argument();

        let resolved_expression = self.analyze_expression(expression, &any_context);

        let resolved_type = &resolved_expression.ty.clone();
        let int_type = Some(self.types().int());
        let (key_type, value_type): (Option<TypeRef>, TypeRef) = match &*resolved_type.kind {
            TypeKind::String => (int_type, self.types().string()),
            TypeKind::SliceView(element_type) => (int_type, element_type.clone()),
            TypeKind::VecStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::SparseStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::StackStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::StackView(element_type) => (int_type, element_type.clone()),
            TypeKind::QueueStorage(element_type, _fixed_size) => (int_type, element_type.clone()),
            TypeKind::QueueView(element_type) => (int_type, element_type.clone()),
            TypeKind::DynamicLengthVecView(element_type) => (int_type, element_type.clone()),
            TypeKind::SparseView(element_type) => (int_type, element_type.clone()),
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
        ast_expressions: &[swamp_ast::Expression],
    ) -> Vec<Expression> {
        let mut resolved_expressions = Vec::new();
        let argument_expressions_context = TypeContext::new_unsure_argument(expected_type);
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
            let stmt_context = context.with_expected_type(Some(&unit_type));
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
                    let any_context = TypeContext::new_anything_argument();

                    let expr = self.analyze_expression(expression, &any_context);

                    let tk = (*expr.ty.kind).clone();

                    let maybe_to_string = self
                        .shared
                        .state
                        .associated_impls
                        .get_internal_member_function(&expr.ty, "to_string");

                    if matches!(tk, TypeKind::String) {
                        expr
                    } else {
                        let underlying = if let TypeKind::Optional(inner_type) = tk {
                            inner_type.clone()
                        } else {
                            expr.ty.clone()
                        };
                        if maybe_to_string.is_none() {
                            let string_type = self.types().string();
                            return self.create_expr(
                                ExpressionKind::StringLiteral("hello".to_string()),
                                string_type,
                                &expression.node,
                            );
                            /* todo:
                            return Err(self.create_err(
                                ErrorKind::MissingToString(underlying.clone()),
                                &expression.node,
                            ));

                             */
                        }

                        let expr_as_param = ArgumentExpression::Expression(expr);
                        let call_expr_kind = self.create_static_member_call(
                            "to_string",
                            &[expr_as_param.clone()],
                            &expression.node,
                            &underlying,
                        );

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
                self.create_expr_resolved(ExpressionKind::BinaryOp(op), string_type, &node)
            } else {
                created_expression
            };

            last_expression = Some(x_last_expr);
        }

        if last_expression.is_none() {
            let string_type = self.shared.state.types.string();
            return self.create_expr(
                ExpressionKind::StringLiteral("hello".to_string()),
                string_type,
                node,
            );
        }

        last_expression.unwrap()
    }

    pub(crate) fn analyze_identifier(
        &mut self,
        qualified_func_name: &swamp_ast::QualifiedIdentifier,
    ) -> Expression {
        // Must check variable first, since that is more intuitive for the user.
        // local variables before other functions
        if qualified_func_name.module_path.is_none()
            && qualified_func_name.generic_params.is_empty()
        {
            if let Some(found_variable) = self.try_find_variable(&qualified_func_name.name) {
                return self.create_expr(
                    ExpressionKind::VariableAccess(found_variable.clone()),
                    found_variable.resolved_type.clone(),
                    &qualified_func_name.name,
                );
            }
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
    fn analyze_slice_pair_literal(
        &mut self,
        node: &swamp_ast::Node,
        entries: &[(swamp_ast::Expression, swamp_ast::Expression)],
    ) -> (Vec<(Expression, Expression)>, TypeRef, TypeRef) {
        if entries.is_empty() {
            return (
                vec![],
                self.shared.state.types.unit(),
                self.shared.state.types.unit(),
            );
        }

        // Resolve first entry to determine map types
        let (first_key, first_value) = &entries[0];
        let anything_context = TypeContext::new_anything_argument();
        let resolved_first_key = self.analyze_expression(first_key, &anything_context);
        let resolved_first_value = self.analyze_expression(first_value, &anything_context);
        let key_type = resolved_first_key.ty.clone();
        let value_type = resolved_first_value.ty.clone();

        let key_context = TypeContext::new_argument(&key_type);
        let value_context = TypeContext::new_argument(&value_type);

        // Check all entries match the types
        let mut resolved_entries = Vec::new();
        resolved_entries.push((resolved_first_key, resolved_first_value));

        for (key, value) in entries.iter().skip(1) {
            let resolved_key = self.analyze_expression(key, &key_context);
            let resolved_value = self.analyze_expression(value, &value_context);

            if !self
                .shared
                .state
                .types
                .compatible_with(&resolved_key.ty, &key_type)
            {
                return (
                    vec![],
                    self.shared.state.types.unit(),
                    self.shared.state.types.unit(),
                );
            }

            if !self
                .shared
                .state
                .types
                .compatible_with(&resolved_value.ty, &value_type)
            {
                return (
                    vec![],
                    self.shared.state.types.unit(),
                    self.shared.state.types.unit(),
                );
            }

            resolved_entries.push((resolved_key, resolved_value));
        }

        (resolved_entries, key_type, value_type)
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
            let maybe_key_context = TypeContext::new_anything_argument();

            let first_key_expression = self.analyze_expression(&items[0].0, &maybe_key_context);

            let maybe_value_context = TypeContext::new_anything_argument();
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

        let required_key_context = TypeContext::new_argument(&key_type);
        let required_value_context = TypeContext::new_argument(&value_type);

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
                    (expected_type, element_type.clone())
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
                    (expected_type, element_type.clone())
                }
                TypeKind::QueueView(element_type)
                | TypeKind::SparseView(element_type)
                | TypeKind::StackView(element_type)
                | TypeKind::GridView(element_type)
                | TypeKind::DynamicLengthVecView(element_type) => {
                    (expected_type, element_type.clone())
                }
                TypeKind::FixedCapacityAndLengthArray(element_type, _size) => {
                    (expected_type, element_type.clone())
                }
                TypeKind::SliceView(element_type) => (expected_type, element_type.clone()),
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
            let maybe_context = TypeContext::new_anything_argument();
            let first = self.analyze_expression(&items[0], &maybe_context);
            let required_type = first.ty;
            (
                &self.types().vec_storage(&required_type, items.len()),
                required_type,
            )
        };

        let required_context = TypeContext::new_argument(&element_type);
        let mut resolved_items = Vec::new();
        for item in items {
            let resolved_expr = self.analyze_expression(item, &required_context);
            resolved_items.push(resolved_expr);
        }
        (collection_type.clone(), resolved_items)
    }

    fn push_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Open,
            variables: SeqMap::default(),
        });
    }

    fn pop_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.pop();
    }

    fn push_closed_block_scope(&mut self) {
        self.scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Closed,
            variables: SeqMap::default(),
        });
    }

    fn pop_closed_block_scope(&mut self) {
        self.scope.block_scope_stack.pop();
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
        let scrutinee_context = TypeContext::new_anything_argument();
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
                &own_context.with_expected_type(known_type.as_ref()),
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
        let required_condition_type_context = TypeContext::new_argument(expected_condition_type);
        let literal = self.analyze_literal(node, ast_literal, &required_condition_type_context);

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
            let any_context = TypeContext::new_anything_argument();
            let must_have_expression = if let Some(x) = &variable.expression {
                x
            } else {
                &swamp_ast::Expression {
                    kind: swamp_ast::ExpressionKind::VariableReference(variable.variable.clone()),
                    node: variable.variable.name.clone(),
                }

                // self.create_expr(ExpressionKind::VariableAccess())
            };

            let var = self.analyze_mut_or_immutable_expression(
                must_have_expression,
                &any_context,
                LocationSide::Rhs,
            );

            variable_expressions.push(var);
        }

        self.push_closed_block_scope();
        let mut expressions = Vec::new();
        for (variable_binding, resolved_expression) in variables.iter().zip(variable_expressions) {
            let initialize_variable_expression = self
                .create_variable_binding_for_with(&variable_binding.variable, resolved_expression);
            expressions.push(initialize_variable_expression);
        }

        let resolved_expression = self.analyze_expression(expression, context);
        let block_type = resolved_expression.ty.clone();
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
                let any_context = TypeContext::new_anything_argument();
                self.analyze_mut_or_immutable_expression(
                    found_expr,
                    &any_context,
                    LocationSide::Rhs,
                )
            } else {
                let same_var = self.find_variable(&variable_binding.variable);

                if same_var.is_mutable() {
                    let loc = SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(&variable_binding.variable.name),
                        ty: same_var.resolved_type.clone(),
                        starting_variable: same_var,
                        access_chain: vec![],
                    };
                    ArgumentExpression::BorrowMutableReference(loc)
                } else {
                    let generated_expr_kind = ExpressionKind::VariableAccess(same_var.clone());
                    let generated_expression = self.create_expr(
                        generated_expr_kind,
                        same_var.resolved_type.clone(),
                        &variable_binding.variable.name,
                    );
                    ArgumentExpression::Expression(generated_expression)
                }
            };

            let tk = &mut_expr.ty().kind;

            if let TypeKind::Optional(found_ty) = &**tk {
                let variable_ref = self.create_variable(&variable_binding.variable, found_ty);

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
            let block_type_for_true_context = context.we_know_expected_type(&block_type);
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
                &context.with_expected_type(detected_type.as_ref()),
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

        let return_block_type = TypeContext::new_argument(&signature.return_type);

        self.push_block_scope("lambda");

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
                Some(node),
                &variable_type.resolved_type,
                false,
            );
            resolved_variables.push(variable_ref);
        }

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
                PostfixKind::SliceViewSubscript(_, _) => {
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
                PostfixKind::NoneCoalescingOperator(_) => {
                    is_owned_result = true;
                    is_mutable = false;
                }
                PostfixKind::VecSubscript(_, _) => {}
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

    pub const fn check_mutable_assignment(&mut self, assignment_mode: AssignmentMode, node: &Node) {
    }

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
            let any_type_context = TypeContext::new_anything_argument();
            self.analyze_expression(source_expression, &any_type_context)
        };
        let tk = (*source_expr.ty.kind).clone();
        if !source_expr.ty.is_blittable() {
            let debug_text = self.get_text(&variable.name);
            if !debug_text.starts_with('_') {
                return self.create_err(
                    ErrorKind::VariableTypeMustBeConcrete(source_expr.ty),
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
            annotation_type.map(|found_ast_type| self.analyze_type(found_ast_type));

        let unsure_arg_context = TypeContext::new_unsure_argument(maybe_annotated_type.as_ref());

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

        let nothing_context = TypeContext::new(None);

        let base_expr = self.analyze_expression(&chain.base, &nothing_context);
        let ExpressionKind::VariableAccess(start_variable) = base_expr.kind else {
            self.add_err(ErrorKind::NotValidLocationStartingPoint, &chain.base.node);
            let unit_type = self.types().unit();
            let variable = Variable {
                name: Default::default(),
                assigned_name: String::new(),
                resolved_type: unit_type,
                mutable_node: None,
                variable_type: VariableType::Local,
                scope_index: 0,
                variable_index: 0,
                unique_id_within_function: 0,
                is_unused: false,
            };
            return SingleLocationExpression {
                kind: MutableReferenceKind::MutVariableRef,
                node: self.to_node(&chain.base.node),
                ty: self.shared.state.types.unit(),
                starting_variable: VariableRef::from(variable),
                access_chain: vec![],
            };
        };

        if !start_variable.is_mutable() {
            self.add_err(ErrorKind::VariableIsNotMutable, &chain.base.node);

            let unit_type = self.types().unit();
            let variable = Variable {
                name: Default::default(),
                assigned_name: String::new(),
                resolved_type: unit_type,
                mutable_node: None,
                variable_type: VariableType::Local,
                scope_index: 0,
                variable_index: 0,
                unique_id_within_function: 0,
                is_unused: false,
            };
            return SingleLocationExpression {
                kind: MutableReferenceKind::MutVariableRef,
                node: self.to_node(&chain.base.node),
                ty: self.shared.state.types.unit(),
                starting_variable: VariableRef::from(variable),
                access_chain: vec![],
            };
        }

        let mut ty = start_variable.resolved_type.clone();
        for (i, item) in chain.postfixes.iter().enumerate() {
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
                        let unsigned_int_context = TypeContext::new_argument(&int_type);
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
                    _ => panic!("not allowed"),
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
                            let unsigned_int_context = TypeContext::new_argument(&int_type);
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
                            let unsigned_int_context = TypeContext::new_argument(&int_type);
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
                            let key_index_context = TypeContext::new_argument(key_type);
                            let key_expr =
                                self.analyze_expression(ast_key_expression, &key_index_context);

                            let map_like_type = MapType {
                                key: key_type.clone(),
                                value: value_type.clone(),
                            };

                            match location_side {
                                LocationSide::Lhs => {
                                    // if it is on the left hand side (assignment), then
                                    // we should create an empty if it doesn't exist
                                    self.add_location_item(
                                        &mut items,
                                        LocationAccessKind::MapSubscriptCreateIfNeeded(
                                            map_like_type,
                                            key_expr,
                                        ),
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
                            eprintln!("can not subscript with this type: {underlying}");
                            todo!()
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
                swamp_ast::Postfix::NoneCoalescingOperator(expr) => {
                    return SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(&expr.node),
                        ty: self.shared.state.types.unit(),
                        starting_variable: start_variable,
                        access_chain: vec![],
                    };
                }
            }
        }

        if let Some(found_expected_type) = context.expected_type {
            if !self.types().compatible_with(found_expected_type, &ty) {
                self.add_err(
                    ErrorKind::IncompatibleTypes {
                        expected: found_expected_type.clone(),
                        found: ty.clone(),
                    },
                    &chain.base.node,
                );
            }
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
        let any_argument_context = TypeContext::new_anything_argument();
        let source_expr = self.analyze_expression(ast_source_expression, &any_argument_context);
        let source_expr_type_context = TypeContext::new_argument(&source_expr.ty);

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
        let base_context = TypeContext::new_argument(target_type);
        let source_expr = self.analyze_expression(ast_source_expression, &base_context);

        /*
        // previously checked for exact type
        let lhs_argument_context = TypeContext::new_argument(&ty);
        let source_expr = self.analyze_expression(ast_source_expression, &lhs_argument_context)?;
         */

        let final_expr = if self.types().compatible_with(target_type, &source_expr.ty) {
            source_expr
        }
        /*else if let Some(converted) =
            self.try_convert_for_assignment(&source_expr, &target_type, ast_source_expression)?

        {
            converted
        }         */
        else {
            return self.create_err(
                ErrorKind::IncompatibleTypesForAssignment {
                    expected: target_type.clone(),
                    found: source_expr.ty,
                },
                &ast_source_expression.node,
            );
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
        let any_argument_context = TypeContext::new_anything_argument();
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

    #[must_use]
    pub fn create_expr_resolved(
        &self,
        kind: ExpressionKind,
        ty: TypeRef,
        ast_node: &Node,
    ) -> Expression {
        Expression {
            kind,
            ty,
            node: ast_node.clone(),
        }
    }

    fn analyze_destructuring(
        &mut self,
        node: &swamp_ast::Node,
        target_ast_variables: &[swamp_ast::Variable],
        tuple_expression: &swamp_ast::Expression,
    ) -> Expression {
        let any_context = TypeContext::new_anything_argument();
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
            resolved_type: int_type,
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
                resolved_types.push(self.analyze_type(ast_type.get_type()));
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
        let binding = type_that_member_is_on.lowest_common_denominator_view();

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
        /*
        if let TypeKind::InternalInitializerPairList(slice_key, slice_value) = &*encountered_type.kind {
            if Self::is_compatible_initializer_pair_list_target(
                expected_type,
                slice_key,
                slice_value,
            ) {
                return Ok(expr);
            }
        } else if let TypeKind::InternalInitializerList(element_type) = &*encountered_type.kind {
            if Self::is_compatible_initializer_list_target(expected_type, element_type) {
                return Ok(expr);
            }
        } else*/
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
            "Vec" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    self.shared.state.types.dynamic_vec_view(&element_type)
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    self.shared
                        .state
                        .types
                        .vec_storage(&element_type, fixed_size)
                } else {
                    panic!("todo: make this into an error")
                }
            }
            "Stack" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    self.shared.state.types.stack_view(&element_type)
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    self.shared
                        .state
                        .types
                        .stack_storage(&element_type, fixed_size)
                } else {
                    panic!("todo: make this into an error")
                }
            }
            "Queue" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    self.shared.state.types.queue_view(&element_type)
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    self.shared
                        .state
                        .types
                        .queue_storage(&element_type, fixed_size)
                } else {
                    panic!("todo: make this into an error")
                }
            }
            "Sparse" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    self.shared.state.types.sparse_view(&element_type)
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    let fixed_size =
                        self.analyze_generic_parameter_usize(&ast_generic_parameters[1]);
                    self.shared
                        .state
                        .types
                        .sparse_storage(&element_type, fixed_size)
                } else {
                    panic!("todo: make this into an error")
                }
            }

            "Grid" => {
                if ast_generic_parameters.len() == 1 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    self.shared.state.types.grid_view(&element_type)
                } else if ast_generic_parameters.len() == 2 {
                    let element_type = self.analyze_type(ast_generic_parameters[0].get_type());
                    let (width, height) =
                        self.analyze_generic_parameter_usize_tuple(&ast_generic_parameters[1]);
                    self.shared
                        .state
                        .types
                        .grid_storage(&element_type, width, height)
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
}
