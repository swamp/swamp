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
use std::str::{FromStr, ParseBoolError};
use swamp_modules::prelude::*;
use swamp_modules::symtbl::{SymbolTableRef, TypeGeneratorKind};
use swamp_semantic::instantiator::TypeVariableScope;
use swamp_semantic::prelude::*;
use swamp_semantic::type_var_stack::SemanticContext;
use swamp_semantic::{
    ArgumentExpression, BinaryOperatorKind, BlockScope, BlockScopeMode, FunctionScopeState,
    InternalMainExpression, LocationAccess, LocationAccessKind, MapType, MutableReferenceKind,
    NormalPattern, Postfix, PostfixKind, SingleLocationExpression, SliceType,
    TargetAssignmentLocation, TypeWithMut, VecType, WhenBinding,
};
use swamp_semantic::{StartOfChain, StartOfChainKind};
use swamp_types::all_types_are_concrete_or_unit;
use swamp_types::prelude::*;
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
    Rhs,
}

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

/// Type checking context
#[derive(Debug, Clone)]
pub struct TypeContext<'a> {
    /// Expected type for the current expression
    pub expected_type: Option<&'a Type>,
}

impl<'a> TypeContext<'a> {
    #[must_use]
    pub const fn new(expected_type: Option<&'a Type>) -> Self {
        Self { expected_type }
    }

    #[must_use]
    pub const fn new_argument(required_type: &'a Type) -> Self {
        Self {
            expected_type: Some(required_type),
        }
    }

    #[must_use]
    pub const fn new_unsure_argument(expected_type: Option<&'a Type>) -> Self {
        Self { expected_type }
    }

    #[must_use]
    pub const fn new_anything_argument() -> Self {
        Self {
            expected_type: None,
        }
    }

    #[must_use]
    pub const fn new_function(required_type: &'a Type) -> Self {
        Self {
            expected_type: Some(required_type),
        }
    }

    #[must_use]
    pub const fn with_expected_type(&self, expected_type: Option<&'a Type>) -> Self {
        Self { expected_type }
    }

    pub(crate) const fn we_know_expected_type(&self, found_type: &'a Type) -> Self {
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
    pub type_variables: SemanticContext,
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
            type_variables: SemanticContext::default(),
        };
        Self {
            scope: FunctionScopeState::new(),
            global: FunctionScopeState::new(),
            shared,
            module_path: module_path.to_vec(),
            function_variables: Vec::new(),
            function_parameters: Vec::new(),
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
    ) -> Result<Expression, Error> {
        let resolved_condition = self.analyze_bool_argument_expression(condition)?;

        let branch_context = context;

        let true_expr = self.analyze_expression(true_expression, branch_context)?;
        let resolved_true = Box::new(true_expr);

        let mut detected = context.expected_type.cloned();
        if detected.is_none() && !matches!(resolved_true.ty, Type::Never) {
            detected = Some(resolved_true.ty.clone());
        }

        // Analyze the false branch if it exists
        let else_statements = if let Some(false_expression) = maybe_false_expression {
            let else_context = branch_context.with_expected_type(detected.as_ref());
            let else_expr = self.analyze_expression(false_expression, &else_context)?;
            if detected.is_none() && !matches!(else_expr.ty, Type::Never) {
                detected = Some(else_expr.ty.clone());
            }

            Some(Box::new(else_expr))
        } else {
            None
        };

        Ok(self.create_expr(
            ExpressionKind::If(resolved_condition, resolved_true, else_statements),
            detected.unwrap(),
            &condition.node,
        ))
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

    fn analyze_return_type(&mut self, function: &swamp_ast::Function) -> Result<Type, Error> {
        let ast_return_type = match function {
            swamp_ast::Function::Internal(x) => &x.declaration.return_type,
            swamp_ast::Function::External(x) => &x.return_type,
        };

        let resolved_return_type = match ast_return_type {
            None => Type::Unit,
            Some(x) => self.analyze_type(x)?,
        };

        Ok(resolved_return_type)
    }

    fn analyze_function_body_expression(
        &mut self,
        expression: &swamp_ast::Expression,
        return_type: &Type,
    ) -> Result<Expression, Error> {
        let context = TypeContext::new_function(return_type);
        let resolved_statement = self.analyze_expression(expression, &context)?;

        Ok(resolved_statement)
    }

    fn analyze_maybe_type(&mut self, maybe_type: Option<&swamp_ast::Type>) -> Result<Type, Error> {
        let found_type = match maybe_type {
            None => Type::Unit,
            Some(ast_type) => self.analyze_type(ast_type)?,
        };
        Ok(found_type)
    }

    fn analyze_for_pattern(
        &mut self,
        pattern: &swamp_ast::ForPattern,
        key_type: Option<&Type>,
        value_type: &Type,
    ) -> Result<ForPattern, Error> {
        match pattern {
            swamp_ast::ForPattern::Single(var) => {
                let variable_ref = self.create_local_variable(
                    &var.identifier,
                    Option::from(&var.is_mut),
                    value_type,
                )?;
                Ok(ForPattern::Single(variable_ref))
            }
            swamp_ast::ForPattern::Pair(first, second) => {
                let found_key = key_type.unwrap();
                let first_var_ref = self.create_local_variable(
                    &first.identifier,
                    Option::from(&first.is_mut),
                    found_key,
                )?;
                let second_var_ref = self.create_local_variable(
                    &second.identifier,
                    Option::from(&second.is_mut),
                    value_type,
                )?;
                Ok(ForPattern::Pair(first_var_ref, second_var_ref))
            }
        }
    }

    fn analyze_parameters(
        &mut self,
        parameters: &Vec<swamp_ast::Parameter>,
    ) -> Result<Vec<TypeForParameter>, Error> {
        let mut resolved_parameters = Vec::new();
        for parameter in parameters {
            let param_type = self.analyze_type(&parameter.param_type)?;
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
        Ok(resolved_parameters)
    }

    pub(crate) fn analyze_static_member_access(
        &mut self,
        named_type: &swamp_ast::QualifiedTypeIdentifier,
        member_name_node: &swamp_ast::Node,
    ) -> Option<Function> {
        let Some(some_type) = self.analyze_named_type(named_type).ok() else {
            return None;
        };

        let member_name = self.get_text(member_name_node);
        self.lookup_associated_function(&some_type, member_name)
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
                        &internal_fn.signature.signature,
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

                return Some(kind.clone());
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
        if let Some(found_func_def) = self.check_if_function_reference(ast_expression) {
            Some(StartOfChainBase::FunctionReference(found_func_def))
        } else if let swamp_ast::ExpressionKind::IdentifierReference(found_qualified_identifier) =
            &ast_expression.kind
        {
            self.try_find_variable(&found_qualified_identifier.name)
                .map(StartOfChainBase::Variable)
        } else {
            None
        }
    }

    pub fn debug_expression(&self, expr: &swamp_ast::Expression, description: &str) {
        self.debug_line(expr.node.span.offset as usize, description);
    }

    pub fn debug_line(&self, offset: usize, description: &str) {
        let (line, _col) = self
            .shared
            .source_map
            .get_span_location_utf8(self.shared.file_id, offset);
        let source_line = self
            .shared
            .source_map
            .get_source_line(self.shared.file_id, line);
        info!(?line, ?source_line, description);
    }

    /// # Errors
    ///
    pub fn analyze_main_expression(
        &mut self,
        ast_expression: &swamp_ast::Expression,
    ) -> Result<InternalMainExpression, Error> {
        self.start_function();

        let context = TypeContext::new_anything_argument();
        let analyzed_expr = self.analyze_expression(ast_expression, &context)?;
        let main_expr = InternalMainExpression {
            expression: analyzed_expr,
            function_variables: self.function_variables.clone(),
            function_parameters: self.function_parameters.clone(),
            program_unique_id: self.shared.state.allocate_internal_function_id(),
        };

        self.stop_function();

        Ok(main_expr)
    }

    fn analyze_maybe_ref_expression(
        &mut self,
        ast_expr: &swamp_ast::Expression,
    ) -> Result<MaybeBorrowMutRefExpression, Error> {
        if let swamp_ast::ExpressionKind::UnaryOp(found_unary, ast_inner_expression) =
            &ast_expr.kind
        {
            if let swamp_ast::UnaryOperator::BorrowMutRef(node) = found_unary {
                //let inner = self.analyze_expression(ast_inner_expression, context)?;
                let resolved_node = self.to_node(node);
                return Ok(MaybeBorrowMutRefExpression {
                    ast_expression: *ast_inner_expression.clone(),
                    has_borrow_mutable_reference: Some(resolved_node),
                });
            }
        }

        Ok(MaybeBorrowMutRefExpression {
            ast_expression: ast_expr.clone(),
            has_borrow_mutable_reference: None,
        })
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression(
        &mut self,
        ast_expression: &swamp_ast::Expression,
        context: &TypeContext,
    ) -> Result<Expression, Error> {
        //        info!(?ast_expression, "analyze expression");
        //self.debug_expression(ast_expression);
        let expr = self.analyze_expression_internal(ast_expression, context)?;

        let encountered_type = expr.ty.clone();

        let expr = if let Some(found_expected_type) = context.expected_type {
            if found_expected_type
                .underlying()
                .compatible_with(&encountered_type.underlying())
            {
                return Ok(expr);
            }

            self.types_did_not_match_try_late_coerce_expression(
                expr,
                found_expected_type,
                &encountered_type,
                &ast_expression.node,
            )?
        } else {
            expr
            //todo!()
            // TODO: self.coerce_unrestricted_type(&ast_expression.node, expr)?
        };

        Ok(expr)
    }

    fn new_from_slice(&mut self, analyzed_element_type: &Type) -> Result<Expression, Error> {
        todo!()
    }

    /*
        fn coerce_unrestricted_type(
            &mut self,
            ast_node: &swamp_ast::Node,
            expr: Expression,
        ) -> Result<Expression, Error> {
            let expr = match &expr.ty {
                Type::Slice(analyzed_element_type) => {
                    let vec_blueprint = self
                        .shared
                        .core_symbol_table
                        .get_blueprint("Vec")
                        .unwrap()
                        .clone();
                    let created_type = self
                        .shared
                        .state
                        .instantiator
                        .instantiate_blueprint_and_members(
                            &vec_blueprint,
                            &[*analyzed_element_type.clone()],
                        )?;
                    assert!(analyzed_element_type.is_concrete());
                    let mut_or_immut = MutRefOrImmutableExpression::Expression(expr);

                    let call_kind = self.create_static_member_intrinsic_call(
                        "new_from_slice",
                        &[mut_or_immut],
                        &ast_node,
                        &created_type,
                    )?;

                    self.create_expr(call_kind, created_type, ast_node)
                }
                Type::SlicePair(analyzed_key_type, analyzed_value_type) => {
                    let map_blueprint = self
                        .shared
                        .core_symbol_table
                        .get_blueprint("Map")
                        .unwrap()
                        .clone();

                    let created_type = self
                        .shared
                        .state
                        .instantiator
                        .instantiate_blueprint_and_members(
                            &map_blueprint,
                            &[*analyzed_key_type.clone(), *analyzed_value_type.clone()],
                        )?;

                    let mut_or_immut = MutRefOrImmutableExpression::Expression(expr);

                    let call_kind = self.create_static_member_intrinsic_call(
                        "new_from_slice_pair",
                        &[mut_or_immut],
                        &ast_node,
                        &created_type,
                    )?;

                    self.create_expr(call_kind, created_type, ast_node)
                }
                _ => expr,
            };
            Ok(expr)
        }
    */
    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression_internal(
        &mut self,
        ast_expression: &swamp_ast::Expression,
        context: &TypeContext,
    ) -> Result<Expression, Error> {
        let expression = match &ast_expression.kind {
            // Lookups
            swamp_ast::ExpressionKind::PostfixChain(postfix_chain) => {
                self.analyze_postfix_chain(postfix_chain)?
            }

            swamp_ast::ExpressionKind::VariableDefinition(
                variable,
                maybe_annotation,
                source_expression,
            ) => self.analyze_create_variable(
                variable,
                Option::from(maybe_annotation),
                source_expression,
            )?,

            swamp_ast::ExpressionKind::VariableAssignment(variable, source_expression) => {
                self.analyze_variable_assignment(variable, source_expression)?
            }

            swamp_ast::ExpressionKind::DestructuringAssignment(variables, expression) => {
                self.analyze_destructuring(&ast_expression.node, variables, expression)?
            }

            swamp_ast::ExpressionKind::IdentifierReference(qualified_identifier) => {
                self.analyze_identifier(qualified_identifier)?
            }

            swamp_ast::ExpressionKind::VariableReference(variable) => {
                self.analyze_variable_reference(&variable.name)?
            }

            swamp_ast::ExpressionKind::StaticMemberFunctionReference(
                type_identifier,
                member_name,
            ) => panic!("can not have separate member func ref"),

            swamp_ast::ExpressionKind::ConstantReference(constant_identifier) => {
                self.analyze_constant_access(constant_identifier)?
            }

            swamp_ast::ExpressionKind::Assignment(location, source) => {
                self.analyze_assignment(location, source)?
            }

            swamp_ast::ExpressionKind::CompoundAssignment(target, op, source) => {
                self.analyze_assignment_compound(target, op, source)?
            }

            // Operator
            swamp_ast::ExpressionKind::BinaryOp(resolved_a, operator, resolved_b) => {
                let (resolved_op, result_type) =
                    self.analyze_binary_op(resolved_a, operator, resolved_b)?;

                self.create_expr(
                    ExpressionKind::BinaryOp(resolved_op),
                    result_type,
                    &ast_expression.node,
                )
            }

            swamp_ast::ExpressionKind::UnaryOp(operator, expression) => {
                if let swamp_ast::UnaryOperator::BorrowMutRef(_node) = operator {
                    let inner_expr =
                        self.analyze_to_location(expression, context, LocationSide::Rhs)?;
                    let ty = inner_expr.ty.clone();
                    self.create_expr(
                        ExpressionKind::BorrowMutRef(Box::from(inner_expr)),
                        Type::MutableReference(Box::from(ty)),
                        &ast_expression.node,
                    )
                } else {
                    let (resolved_op, result_type) = self.analyze_unary_op(operator, expression)?;
                    self.create_expr(
                        ExpressionKind::UnaryOp(resolved_op),
                        result_type,
                        &ast_expression.node,
                    )
                }
            }

            swamp_ast::ExpressionKind::Block(expressions) => {
                let (block, resulting_type) =
                    self.analyze_block(&ast_expression.node, context, expressions)?;
                self.create_expr(
                    ExpressionKind::Block(block),
                    resulting_type,
                    &ast_expression.node,
                )
            }

            swamp_ast::ExpressionKind::With(variable_bindings, expression) => {
                self.analyze_with_expr(context, variable_bindings, expression)?
            }

            swamp_ast::ExpressionKind::When(variable_bindings, true_expr, else_expr) => {
                self.analyze_when_expr(context, variable_bindings, true_expr, else_expr.as_deref())?
            }

            swamp_ast::ExpressionKind::InterpolatedString(string_parts) => {
                self.analyze_interpolated_string_lowering(&ast_expression.node, string_parts)?
            }

            // Creation
            swamp_ast::ExpressionKind::NamedStructLiteral(struct_identifier, fields, has_rest) => {
                self.analyze_named_struct_literal(struct_identifier, fields, *has_rest)?
            }

            swamp_ast::ExpressionKind::AnonymousStructLiteral(fields, rest_was_specified) => self
                .analyze_anonymous_struct_literal(
                &ast_expression.node,
                fields,
                *rest_was_specified,
                context,
            )?,

            swamp_ast::ExpressionKind::Range(min_value, max_value, range_mode) => {
                self.analyze_range(min_value, max_value, range_mode, &ast_expression.node)?
            }

            swamp_ast::ExpressionKind::Literal(literal) => {
                self.analyze_complex_literal_to_expression(ast_expression, literal, context)?
            }

            swamp_ast::ExpressionKind::ForLoop(
                pattern,
                iterable_expression,
                guard_expr,
                statements,
            ) => {
                let _analyzed_guard = if let Some(found_guard) = guard_expr {
                    // TODO: Remove guard in for loops
                    Some(self.analyze_bool_argument_expression(found_guard)?)
                } else {
                    None
                };

                let resolved_iterator =
                    self.analyze_iterable(pattern.any_mut(), &iterable_expression.expression)?;

                self.push_block_scope("for_loop");
                let pattern = self.analyze_for_pattern(
                    pattern,
                    resolved_iterator.key_type.as_ref(),
                    &resolved_iterator.value_type,
                )?;
                let resolved_statements = self.analyze_expression(statements, context)?;
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
                let condition = self.analyze_bool_argument_expression(expression)?;
                //self.push_block_scope("while_loop");
                let resolved_statements = self.analyze_expression(statements, context)?;
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
                )?
            }

            swamp_ast::ExpressionKind::Match(expression, arms) => {
                let (match_expr, return_type) = self.analyze_match(expression, context, arms)?;
                self.create_expr(
                    ExpressionKind::Match(match_expr),
                    return_type,
                    &ast_expression.node,
                )
            }
            swamp_ast::ExpressionKind::Guard(guard_expressions) => {
                self.analyze_guard(&ast_expression.node, context, guard_expressions)?
            }

            swamp_ast::ExpressionKind::Lambda(variables, expression) => {
                self.analyze_lambda(&ast_expression.node, variables, expression, context)?
            }
        };

        //info!(ty=%expression.ty, kind=?expression.kind,  "resolved expression");

        Ok(expression)
    }

    fn get_struct_type(
        &mut self,
        qualified_type_identifier: &swamp_ast::QualifiedTypeIdentifier,
    ) -> Result<NamedStructType, Error> {
        let maybe_struct_type = self.analyze_named_type(qualified_type_identifier)?;
        match maybe_struct_type {
            Type::NamedStruct(struct_type) => Ok(struct_type),
            _ => Err(self.create_err(
                // For other Type variants that are not Struct
                ErrorKind::UnknownStructTypeReference,
                &qualified_type_identifier.name.0,
            )),
        }
    }

    pub(crate) fn analyze_named_type(
        &mut self,
        type_name_to_find: &swamp_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        let (path, name) = self.get_path(type_name_to_find);

        if path.is_empty() {
            // Check if it is a type variable first!
            if let Some(found_type) = self.shared.type_variables.resolve_type_variable(&name) {
                return Ok(found_type);
            }
        }

        let symbol = {
            let maybe_symbol_table = self.shared.get_symbol_table(&path);
            let symbol_table = maybe_symbol_table.ok_or_else(|| {
                self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)
            })?;
            symbol_table
                .get_symbol(&name)
                .ok_or_else(|| {
                    self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)
                })?
                .clone()
        };

        let mut analyzed_type_parameters = Vec::new();

        for analyzed_type in &type_name_to_find.generic_params {
            let ty = self.analyze_type(analyzed_type.get_type())?;

            analyzed_type_parameters.push(ty);
        }

        let result_type = if analyzed_type_parameters.is_empty() {
            match &symbol {
                Symbol::Type(base_type) => base_type.clone(),
                Symbol::Alias(alias_type) => alias_type.referenced_type.clone(),
                _ => {
                    return Err(
                        self.create_err(ErrorKind::UnexpectedType, &type_name_to_find.name.0)
                    );
                }
            }
        } else {
            self.analyze_generic_type(
                &symbol,
                &analyzed_type_parameters,
                &type_name_to_find.name.0,
            )?
        };

        Ok(result_type)
    }

    fn create_default_value_for_type(
        &mut self,
        node: &swamp_ast::Node,
        field_type: &Type,
    ) -> Result<Expression, Error> {
        let kind = match field_type {
            Type::Bool => ExpressionKind::Literal(Literal::BoolLiteral(false)),
            Type::Int => ExpressionKind::Literal(Literal::IntLiteral(0)),
            Type::Float => ExpressionKind::Literal(Literal::FloatLiteral(Fp::zero())),
            Type::String => ExpressionKind::Literal(Literal::StringLiteral(String::new())),
            Type::Tuple(tuple_type_ref) => {
                let mut expressions = Vec::new();
                for resolved_type in tuple_type_ref {
                    let expr = self.create_default_value_for_type(node, resolved_type)?;
                    expressions.push(expr);
                }
                ExpressionKind::Literal(Literal::TupleLiteral(tuple_type_ref.clone(), expressions))
            }
            Type::Optional(_optional_type) => ExpressionKind::Literal(Literal::NoneLiteral),

            Type::NamedStruct(struct_ref) => {
                self.create_default_static_call(node, &Type::NamedStruct(struct_ref.clone()))?
            }
            _ => {
                return Err(
                    self.create_err(ErrorKind::NoDefaultImplemented(field_type.clone()), node)
                );
            }
        };

        let expr = self.create_expr(kind, field_type.clone(), node);
        Ok(expr)
    }

    fn create_static_member_call(
        &mut self,
        function_name: &str,
        arguments: &[ArgumentExpression],
        node: &swamp_ast::Node,
        ty: &Type,
    ) -> Result<ExpressionKind, Error> {
        self.lookup_associated_function(ty, function_name)
            .map_or_else(
                || {
                    Err(self.create_err(
                        ErrorKind::NoAssociatedFunction(ty.clone(), function_name.to_string()),
                        node,
                    ))
                },
                |function| {
                    let Function::Internal(internal_function) = &function else {
                        panic!("only allowed for internal functions");
                    };
                    let expr_kind =
                        ExpressionKind::InternalCall(internal_function.clone(), arguments.to_vec());

                    Ok(expr_kind)
                },
            )
    }

    fn create_static_member_intrinsic_call(
        &mut self,
        function_name: &str,
        arguments: &[ArgumentExpression],
        node: &swamp_ast::Node,
        ty: &Type,
    ) -> Result<ExpressionKind, Error> {
        self.lookup_associated_function(ty, function_name)
            .map_or_else(
                || {
                    Err(self.create_err(
                        ErrorKind::NoAssociatedFunction(ty.clone(), function_name.to_string()),
                        node,
                    ))
                },
                |function| {
                    let Function::Internal(internal_function) = &function else {
                        panic!("only allowed for internal functions");
                    };

                    if let Some((intrinsic_fn, _)) =
                        Self::extract_single_intrinsic_call(&internal_function.body)
                    {
                        let expr_kind = ExpressionKind::IntrinsicCallEx(
                            intrinsic_fn.clone(),
                            arguments.to_vec(),
                        );

                        Ok(expr_kind)
                    } else {
                        Err(self.create_err(
                            ErrorKind::NoAssociatedFunction(ty.clone(), function_name.to_string()),
                            node,
                        ))
                    }
                },
            )
    }

    fn create_default_static_call(
        &mut self,
        node: &swamp_ast::Node,
        ty: &Type,
    ) -> Result<ExpressionKind, Error> {
        self.create_static_member_call("default", &[], node, ty)
    }

    fn add_postfix(
        &mut self,
        vec: &mut Vec<Postfix>,
        kind: PostfixKind,
        ty: Type,
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
        tv: &Type,
    ) -> Result<(AnonymousStructType, usize, Type), Error> {
        let field_name_str = self.get_text(field_name).to_string();

        let anon_struct_ref = match &tv.underlying() {
            Type::NamedStruct(struct_type) => struct_type.anon_struct_type.clone(),
            Type::AnonymousStruct(anon_struct) => anon_struct.clone(),
            Type::Range(anon_struct) => anon_struct.clone(),
            _ => return Err(self.create_err(ErrorKind::UnknownStructField, field_name)),
        };

        if let Some(found_field) = anon_struct_ref
            .field_name_sorted_fields
            .get(&field_name_str)
        {
            let index = anon_struct_ref
                .field_name_sorted_fields
                .get_index(&field_name_str)
                .expect("checked earlier");

            return Ok((
                anon_struct_ref.clone(),
                index,
                found_field.field_type.clone(),
            ));
        }

        Err(self.create_err(ErrorKind::UnknownStructField, field_name))
    }

    pub fn analyze_static_call(
        &mut self,
        ast_node: &swamp_ast::Node,
        maybe_associated_to_type: Option<Type>,
        func_def: Function,
        maybe_generic_arguments: &Option<Vec<swamp_ast::GenericParameter>>,
        arguments: &[swamp_ast::Expression],
    ) -> Result<Expression, Error> {
        let signature = if let Some(found_generic_arguments) = maybe_generic_arguments {
            let types: Vec<_> = found_generic_arguments
                .iter()
                .map(|param| param.get_type().clone())
                .collect();
            let analyzed_generic_argument_types = self.analyze_types(&types)?;
            if let Some(found_associated_to_type) = maybe_associated_to_type {
                self.instantiate_signature_if_needed(
                    &func_def.node(),
                    &found_associated_to_type,
                    &FunctionRef::from(func_def.clone()),
                    &analyzed_generic_argument_types,
                )?
            } else {
                self.instantiate_signature_without_self_if_needed(
                    &func_def.node(),
                    FunctionRef::from(func_def.clone()),
                    &analyzed_generic_argument_types,
                )?
            }
        } else {
            func_def.signature().clone()
        };

        let analyzed_arguments =
            self.analyze_and_verify_parameters(ast_node, &signature.parameters, arguments)?;

        let expr_kind = match &func_def {
            Function::Internal(internal) => {
                ExpressionKind::InternalCall(internal.clone(), analyzed_arguments)
            }
            Function::External(host) => ExpressionKind::HostCall(host.clone(), analyzed_arguments),
            Function::Intrinsic(intrinsic) => {
                ExpressionKind::IntrinsicCallEx(intrinsic.intrinsic.clone(), analyzed_arguments)
            }
        };

        Ok(self.create_expr(expr_kind, *signature.return_type.clone(), ast_node))
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_postfix_chain(
        &mut self,
        chain: &swamp_ast::PostfixChain,
    ) -> Result<Expression, Error> {
        let maybe_start_of_chain_base =
            self.analyze_start_chain_expression_get_mutability(&chain.base);

        let mut start_index = 0;

        let start_of_chain_kind = if let Some(start_of_chain_base) = maybe_start_of_chain_base {
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
                        )?;
                        if chain.postfixes.len() == 1 {
                            return Ok(call_expr);
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
            StartOfChainKind::Expression(Box::from(self.analyze_expression(&chain.base, &ctx)?))
        };

        let start_of_chain_node = self.to_node(&chain.base.node);

        let start_of_chain = StartOfChain {
            kind: start_of_chain_kind.clone(),
            node: start_of_chain_node,
        };

        let mut tv = TypeWithMut {
            resolved_type: start_of_chain_kind.ty().clone(),
            is_mutable: start_of_chain_kind.is_mutable(),
        };

        let mut uncertain = false;

        let mut suffixes = Vec::new();

        for item in &chain.postfixes[start_index..] {
            match item {
                swamp_ast::Postfix::FieldAccess(field_name) => {
                    let (struct_type_ref, index, return_type) =
                        self.analyze_struct_field(&field_name.clone(), &tv.resolved_type)?;
                    self.add_postfix(
                        &mut suffixes,
                        PostfixKind::StructField(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name,
                    );

                    tv.resolved_type = return_type.clone();
                    // keep previous `is_mutable`
                }

                swamp_ast::Postfix::Subscript(index_expr) => {
                    let collection_type = tv.resolved_type.clone();
                    match &collection_type.underlying() {
                        Type::DynamicSlice(element_type_in_slice) => {
                            let unsigned_int_context = TypeContext::new_argument(&Type::Int);
                            let unsigned_int_expression =
                                self.analyze_expression(index_expr, &unsigned_int_context)?;

                            let slice_type = SliceType {
                                element: Box::new(*element_type_in_slice.clone()),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::SliceSubscript(slice_type, unsigned_int_expression),
                                collection_type.clone(),
                                &index_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = *element_type_in_slice.clone();
                        }
                        Type::VecStorage(element_type, _) | Type::Vec(element_type) => {
                            let unsigned_int_context = TypeContext::new_argument(&Type::Int);
                            let unsigned_int_expression =
                                self.analyze_expression(index_expr, &unsigned_int_context)?;

                            let vec_type = VecType {
                                element: element_type.clone(),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::VecSubscript(vec_type, unsigned_int_expression),
                                *element_type.clone(),
                                &index_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = *element_type.clone();
                        }
                        Type::MapStorage(key_type, value_type, _)
                        | Type::Map(key_type, value_type) => {
                            let unsigned_int_context = TypeContext::new_argument(&Type::Int);
                            let unsigned_int_expression =
                                self.analyze_expression(index_expr, &unsigned_int_context)?;

                            let map_type = MapType {
                                key: Box::from(*key_type.clone()),
                                value: Box::from(*value_type.clone()),
                            };

                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::MapSubscript(map_type, unsigned_int_expression),
                                *value_type.clone(),
                                &index_expr.node,
                            );

                            // Keep previous mutable
                            tv.resolved_type = *value_type.clone();
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

                    if let Type::NamedStruct(named_struct) = temp_analyzed_expr.ty {
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
                        return Err(
                            self.create_err(ErrorKind::MissingSubscriptMember, &index_expr.node)
                        );
                    }

                     */
                }

                swamp_ast::Postfix::MemberCall(member_name, generic_arguments, ast_arguments) => {
                    let member_name_str = self.get_text(member_name).to_string();
                    let underlying_type = tv.resolved_type.underlying();

                    let (kind, return_type) = self.analyze_member_call(
                        &underlying_type,
                        &member_name_str,
                        generic_arguments.clone(),
                        ast_arguments,
                        tv.is_mutable,
                        member_name,
                    )?;

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
                        return Err(self.create_err(
                            ErrorKind::UnknownMemberFunction(underlying_type.clone()),
                            member_name,
                        ));
                    }

                     */
                }
                swamp_ast::Postfix::FunctionCall(node, _generic_arguments, arguments) => {
                    /*
                    if let Type::Function(signature) = &tv.resolved_type {
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
                    let unwrapped_type = if let Type::Optional(unwrapped_type) = &tv.resolved_type {
                        unwrapped_type
                    } else if uncertain {
                        &tv.resolved_type
                    } else {
                        return Err(
                            self.create_err(ErrorKind::CanNotNoneCoalesce, &default_expr.node)
                        );
                    };

                    let unwrapped_type_context = TypeContext::new_argument(unwrapped_type);
                    let resolved_default_expr =
                        self.analyze_expression(default_expr, &unwrapped_type_context)?;
                    self.add_postfix(
                        &mut suffixes,
                        PostfixKind::NoneCoalescingOperator(resolved_default_expr),
                        unwrapped_type.clone(),
                        &default_expr.node,
                    );
                    tv.resolved_type = unwrapped_type.clone();
                    uncertain = false; // the chain is safe, because this will always solve None
                }

                swamp_ast::Postfix::OptionalChainingOperator(option_node) => {
                    if let Type::Optional(unwrapped_type) = &tv.resolved_type {
                        uncertain = true;
                        self.add_postfix(
                            &mut suffixes,
                            PostfixKind::OptionalChainingOperator,
                            *unwrapped_type.clone(),
                            option_node,
                        );
                        tv.resolved_type = *unwrapped_type.clone();
                    } else {
                        return Err(self.create_err(ErrorKind::ExpectedOptional, option_node));
                    }
                }
            }
        }

        if uncertain {
            if let Type::Optional(_) = tv.resolved_type {
            } else {
                tv.resolved_type = Type::Optional(Box::from(tv.resolved_type.clone()));
            }
        }

        Ok(self.create_expr(
            ExpressionKind::PostfixChain(start_of_chain, suffixes),
            tv.resolved_type,
            &chain.base.node,
        ))
    }

    fn analyze_bool_argument_expression(
        &mut self,
        expression: &swamp_ast::Expression,
    ) -> Result<BooleanExpression, Error> {
        let bool_context = TypeContext::new_argument(&Type::Bool);
        let resolved_expression = self.analyze_expression(expression, &bool_context)?;
        let expr_type = resolved_expression.ty.clone();

        let bool_expression = match expr_type {
            Type::Bool => resolved_expression,
            Type::Optional(_) => self.create_expr(
                ExpressionKind::CoerceOptionToBool(Box::new(resolved_expression)),
                Type::Bool,
                &expression.node,
            ),
            _ => {
                return Err(self.create_err(ErrorKind::ExpectedBooleanExpression, &expression.node));
            }
        };

        Ok(BooleanExpression {
            expression: Box::from(bool_expression),
        })
    }

    fn analyze_iterable(
        &mut self,
        force_mut: Option<swamp_ast::Node>,
        expression: &swamp_ast::Expression,
    ) -> Result<Iterable, Error> {
        let any_context = TypeContext::new_anything_argument();
        let resolved_expression: ArgumentExpression = if force_mut.is_some() {
            let resolved_node = self.to_node(&force_mut.unwrap());
            ArgumentExpression::BorrowMutableReference(self.analyze_to_location(
                expression,
                &any_context,
                LocationSide::Rhs,
            )?)
        } else {
            self.analyze_mut_or_immutable_expression(expression, &any_context, LocationSide::Rhs)?
        };

        let resolved_type = &resolved_expression.ty().clone();
        let (key_type, value_type): (Option<Type>, Type) = match resolved_type {
            Type::String => (Some(Type::Int), Type::String),
            Type::Vec(element_type) => (Some(Type::Int), *element_type.clone()),
            Type::Range(_) => (None, Type::Int),

            /*
            _ => {
                if let Some(found_iter_fn) = self
                    .shared
                    .state
                    .instantiator
                    .associated_impls
                    .get_internal_member_function(resolved_type, "iter")
                {
                    let ret_type = found_iter_fn.signature.signature.return_type.clone();
                    match *ret_type {
                        Type::Tuple(tuple_items) => {
                            (Some(tuple_items[0].clone()), tuple_items[1].clone())
                        }
                        _ => {
                            return Err(self.create_err(ErrorKind::NotAnIterator, &expression.node));
                        }
                    }
                } else {
                    return Err(self.create_err(ErrorKind::NotAnIterator, &expression.node));
                }
            }

             */
            _ => return Err(self.create_err(ErrorKind::NotAnIterator, &expression.node)),
        };

        Ok(Iterable {
            key_type,
            value_type,
            resolved_expression: Box::new(resolved_expression),
        })
    }

    fn analyze_argument_expressions(
        &mut self,
        expected_type: Option<&Type>,
        ast_expressions: &[swamp_ast::Expression],
    ) -> Result<Vec<Expression>, Error> {
        let mut resolved_expressions = Vec::new();
        let argument_expressions_context = TypeContext::new_unsure_argument(expected_type);
        for expression in ast_expressions {
            resolved_expressions
                .push(self.analyze_expression(expression, &argument_expressions_context)?);
        }
        Ok(resolved_expressions)
    }

    fn analyze_block(
        &mut self,
        _node: &swamp_ast::Node,
        context: &TypeContext,
        ast_expressions: &[swamp_ast::Expression],
    ) -> Result<(Vec<Expression>, Type), Error> {
        if ast_expressions.is_empty() {
            return Ok((vec![], Type::Unit));
        }

        self.push_block_scope("block");

        let mut resolved_expressions = Vec::with_capacity(ast_expressions.len());

        for expression in &ast_expressions[..ast_expressions.len() - 1] {
            let stmt_context = context.with_expected_type(Some(&Type::Unit));
            let expr = self.analyze_expression(expression, &stmt_context)?;

            if matches!(expr.ty, Type::Never) {
                resolved_expressions.push(expr);
                return Ok((resolved_expressions, Type::Never));
            }

            resolved_expressions.push(expr);
        }

        // Process the last expression - it determines the block's type
        let last_expr =
            self.analyze_expression(&ast_expressions[ast_expressions.len() - 1], context)?;
        let last_type = last_expr.ty.clone();
        resolved_expressions.push(last_expr);

        self.pop_block_scope("block");

        Ok((resolved_expressions, last_type))
    }

    /*
    fn analyze_interpolated_string(
        &mut self,
        string_parts: &[swamp_ast::StringPart],
    ) -> Result<Vec<StringPart>, Error> {
        let mut resolved_parts = Vec::new();
        for part in string_parts {
            let resolved_string_part = match part {
                swamp_ast::StringPart::Literal(string_node, processed_string) => {
                    StringPart::Literal(self.to_node(string_node), processed_string.to_string())
                }
                swamp_ast::StringPart::Interpolation(expression, format_specifier) => {
                    let any_context = TypeContext::new_anything_argument();
                    let expr = self.analyze_expression(expression, &any_context)?;
                    let resolved_format_specifier =
                        self.analyze_format_specifier(Option::from(format_specifier));
                    StringPart::Interpolation(expr, resolved_format_specifier)
                }
            };

            resolved_parts.push(resolved_string_part);
        }

        Ok(resolved_parts)
    }

     */

    fn analyze_interpolated_string_lowering(
        &mut self,
        node: &swamp_ast::Node,
        string_parts: &[swamp_ast::StringPart],
    ) -> Result<Expression, Error> {
        let mut last_expression: Option<Expression> = None;
        for part in string_parts {
            let created_expression = match part {
                swamp_ast::StringPart::Literal(string_node, processed_string) => {
                    let string_literal = Literal::StringLiteral(processed_string.to_string());
                    let basic_literal = ExpressionKind::Literal(string_literal);
                    self.create_expr(basic_literal, Type::String, string_node)
                }
                swamp_ast::StringPart::Interpolation(expression, format_specifier) => {
                    let any_context = TypeContext::new_anything_argument();

                    let expr = self.analyze_expression(expression, &any_context)?;

                    let ty = expr.ty.underlying().clone();

                    let maybe_to_string = self
                        .shared
                        .state
                        .instantiator
                        .associated_impls
                        .get_internal_member_function(&ty, "to_string");

                    if matches!(ty, Type::String) {
                        expr
                    } else {
                        let underlying = if let Type::Optional(inner_type) = ty {
                            *inner_type.clone()
                        } else {
                            ty.underlying().clone()
                        };
                        if maybe_to_string.is_none() {
                            return Err(self.create_err(
                                ErrorKind::MissingToString(underlying.clone()),
                                &expression.node,
                            ));
                        }

                        let expr_as_param = ArgumentExpression::Expression(expr);
                        let call_expr_kind = self.create_static_member_call(
                            "to_string",
                            &[expr_as_param.clone()],
                            &expression.node,
                            &underlying,
                        )?;

                        /*
                        TODO: SUPPORT FORMAT SPEC
                        let resolved_format_specifier =
                            self.analyze_format_specifier(Option::from(format_specifier));

                         */

                        self.create_expr(call_expr_kind, Type::String, &expression.node)
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

                self.create_expr_resolved(ExpressionKind::BinaryOp(op), Type::String, &node)
            } else {
                created_expression
            };

            last_expression = Some(x_last_expr);
        }

        let last = last_expression.unwrap();

        Ok(last)
    }

    pub(crate) fn analyze_identifier(
        &self,
        qualified_func_name: &swamp_ast::QualifiedIdentifier,
    ) -> Result<Expression, Error> {
        // Must check variable first, since that is more intuitive for the user.
        // local variables before other functions
        if qualified_func_name.module_path.is_none()
            && qualified_func_name.generic_params.is_empty()
        {
            if let Some(found_variable) = self.try_find_variable(&qualified_func_name.name) {
                return Ok(self.create_expr(
                    ExpressionKind::VariableAccess(found_variable.clone()),
                    found_variable.resolved_type.clone(),
                    &qualified_func_name.name,
                ));
            }
        }

        let text = self.get_text(&qualified_func_name.name);
        Err(self.create_err(
            ErrorKind::UnknownIdentifier(text.to_string()),
            &qualified_func_name.name,
        ))
    }

    // The ast assumes it is something similar to a variable, but it can be a function reference as well.
    fn analyze_variable_reference(&self, var_node: &swamp_ast::Node) -> Result<Expression, Error> {
        if let Some(found_variable) = self.try_find_variable(var_node) {
            return Ok(self.create_expr(
                ExpressionKind::VariableAccess(found_variable.clone()),
                found_variable.resolved_type.clone(),
                var_node,
            ));
        }
        let text = self.get_text(var_node);
        Err(self.create_err(ErrorKind::UnknownIdentifier(text.to_string()), var_node))
    }
    fn analyze_slice_pair_literal(
        &mut self,
        node: &swamp_ast::Node,
        entries: &[(swamp_ast::Expression, swamp_ast::Expression)],
    ) -> Result<(Vec<(Expression, Expression)>, Type, Type), Error> {
        if entries.is_empty() {
            return Ok((vec![], Type::Unit, Type::Unit));
        }

        // Resolve first entry to determine map types
        let (first_key, first_value) = &entries[0];
        let anything_context = TypeContext::new_anything_argument();
        let resolved_first_key = self.analyze_expression(first_key, &anything_context)?;
        let resolved_first_value = self.analyze_expression(first_value, &anything_context)?;
        let key_type = resolved_first_key.ty.clone();
        let value_type = resolved_first_value.ty.clone();

        let key_context = TypeContext::new_argument(&key_type);
        let value_context = TypeContext::new_argument(&value_type);

        // Check all entries match the types
        let mut resolved_entries = Vec::new();
        resolved_entries.push((resolved_first_key, resolved_first_value));

        for (key, value) in entries.iter().skip(1) {
            let resolved_key = self.analyze_expression(key, &key_context)?;
            let resolved_value = self.analyze_expression(value, &value_context)?;

            if !resolved_key.ty.compatible_with(&key_type) {
                return Err(self.create_err(
                    ErrorKind::MapKeyTypeMismatch {
                        expected: key_type,
                        found: resolved_key.ty,
                    },
                    node,
                ));
            }

            if !resolved_value.ty.compatible_with(&value_type) {
                return Err(self.create_err(
                    ErrorKind::MapValueTypeMismatch {
                        expected: value_type,
                        found: resolved_value.ty,
                    },
                    node,
                ));
            }

            resolved_entries.push((resolved_key, resolved_value));
        }

        Ok((resolved_entries, key_type, value_type))
    }

    fn analyze_slice_pair_key_and_value_types(
        &mut self,
        node: &swamp_ast::Node,
        items: &[(swamp_ast::Expression, swamp_ast::Expression)],
        context: &TypeContext,
    ) -> Result<(Type, Type, Vec<(Expression, Expression)>), Error> {
        let maybe_key_and_value_type = if let Some(expected_type) = context.expected_type {
            if let Type::DynamicSlicePair(key_type, value_type) = expected_type {
                Some((*key_type.clone(), *value_type.clone()))
            } else {
                match expected_type {
                    Type::MapStorage(key, value, _) => Some((*key.clone(), *value.clone())),
                    Type::Map(key, value) => Some((*key.clone(), *value.clone())),
                    _ => return Err(self.create_err(ErrorKind::ExpectedSlice, node)),
                }
            }
        } else {
            None
        };

        if items.is_empty() {
            context.expected_type.map_or_else(
                || Ok((Type::Unit, Type::Unit, vec![])),
                |expected_type| {
                    if let Type::DynamicSlicePair(key_type, value_type) = expected_type {
                        Ok((*key_type.clone(), *value_type.clone(), vec![]))
                    } else if let Type::NamedStruct(named) = expected_type {
                        Ok((
                            named.instantiated_type_parameters[0].clone(),
                            named.instantiated_type_parameters[1].clone(),
                            vec![],
                        ))
                    } else {
                        Err(self.create_err(ErrorKind::ExpectedSlice, node))
                    }
                },
            )
        } else {
            let maybe_key_type = maybe_key_and_value_type
                .clone()
                .map(|key_and_value| key_and_value.0);
            let maybe_key_context = TypeContext::new_unsure_argument(maybe_key_type.as_ref());

            let first_key_expression = self.analyze_expression(&items[0].0, &maybe_key_context)?;

            let maybe_value_type = maybe_key_and_value_type.map(|key_and_value| key_and_value.1);
            let maybe_value_context = TypeContext::new_unsure_argument(maybe_value_type.as_ref());
            let first_value_expression =
                self.analyze_expression(&items[0].1, &maybe_value_context)?;

            let required_key_type = first_key_expression.ty.clone();
            let required_key_context = TypeContext::new_argument(&required_key_type);

            let required_value_type = first_value_expression.ty.clone();
            let required_value_context = TypeContext::new_argument(&required_value_type);

            let mut resolved_items = Vec::new();

            for (key_expr, value_expr) in items {
                let analyzed_key_expr = self.analyze_expression(key_expr, &required_key_context)?;
                let analyzed_value_expr =
                    self.analyze_expression(value_expr, &required_value_context)?;
                resolved_items.push((analyzed_key_expr, analyzed_value_expr));
            }
            Ok((
                first_key_expression.ty,
                first_value_expression.ty,
                resolved_items,
            ))
        }
    }
    fn analyze_slice_type_helper(
        &mut self,
        node: &swamp_ast::Node,
        items: &[swamp_ast::Expression],
        context: &TypeContext,
    ) -> Result<(Type, Vec<Expression>), Error> {
        let maybe_expected_element_type = if let Some(expected_type) = context.expected_type {
            if let Type::DynamicSlice(inner_type) = expected_type {
                Some(*inner_type.clone())
            } else {
                match &expected_type {
                    // We can infer the slice type from target
                    Type::VecStorage(element_type, _) => Some(*element_type.clone()),
                    _ => return Err(self.create_err(ErrorKind::ExpectedSlice, node)),
                }
            }
        } else {
            None
        };

        if items.is_empty() {
            context.expected_type.map_or_else(
                || Ok((Type::Unit, vec![])),
                |expected_type| {
                    if let Type::DynamicSlice(inner_type) = expected_type {
                        Ok((*inner_type.clone(), vec![]))
                    } else if let Type::VecStorage(inner_type, _fixed_size) = expected_type {
                        Ok((*inner_type.clone(), vec![]))
                    } else if let Type::NamedStruct(named) = expected_type {
                        Ok((named.instantiated_type_parameters[0].clone(), vec![]))
                    } else {
                        Err(self.create_err(ErrorKind::ExpectedSlice, node))
                    }
                },
            )
        } else {
            let maybe_context =
                TypeContext::new_unsure_argument(maybe_expected_element_type.as_ref());
            let first = self.analyze_expression(&items[0], &maybe_context)?;
            let required_type = first.ty.clone();
            let required_context = TypeContext::new_argument(&required_type);
            let mut resolved_items = Vec::new();
            for item in items {
                let resolved_expr = self.analyze_expression(item, &required_context)?;
                resolved_items.push(resolved_expr);
            }
            Ok((first.ty, resolved_items))
        }
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

    fn analyze_enum_variant_ref(
        &self,
        qualified_type_identifier: &swamp_ast::QualifiedTypeIdentifier,
        variant_name: &swamp_ast::LocalTypeIdentifier,
    ) -> Result<EnumVariantType, Error> {
        let variant_name_string = self.get_text(&variant_name.0).to_string();
        self.get_enum_variant_type(qualified_type_identifier, &variant_name_string)
    }

    fn analyze_match(
        &mut self,
        scrutinee: &swamp_ast::Expression,
        default_context: &TypeContext,
        arms: &Vec<swamp_ast::MatchArm>,
    ) -> Result<(Match, Type), Error> {
        let mut known_type = default_context.expected_type.cloned();
        let own_context = default_context.clone();
        // Analyze the scrutinee with no specific expected type
        let scrutinee_context = TypeContext::new_anything_argument();
        let resolved_scrutinee = self.analyze_expression(scrutinee, &scrutinee_context)?;
        let scrutinee_type = resolved_scrutinee.ty.clone();

        // Ensure we have at least one arm
        if arms.is_empty() {
            return Err(self.create_err(ErrorKind::EmptyMatch, &scrutinee.node));
        }

        let mut resolved_arms = Vec::with_capacity(arms.len());

        for arm in arms {
            let (resolved_arm, _anyone_wants_mutable) = self.analyze_arm(
                arm,
                &resolved_scrutinee,
                &own_context.with_expected_type(known_type.as_ref()),
                &scrutinee_type,
            )?;

            if known_type.is_none() && !matches!(resolved_arm.expression.ty, Type::Never) {
                known_type = Some(resolved_arm.expression.ty.clone());
            }
            resolved_arms.push(resolved_arm);
        }

        known_type.map_or_else(
            || Err(self.create_err(ErrorKind::MatchArmsMustHaveTypes, &scrutinee.node)),
            |encountered_type| {
                if matches!(encountered_type, Type::Never) {
                    Err(self.create_err(
                        ErrorKind::IncompatibleTypes {
                            expected: Type::Never,
                            found: encountered_type,
                        },
                        &scrutinee.node,
                    ))
                } else {
                    Ok((
                        Match {
                            expression: Box::new(resolved_scrutinee),
                            arms: resolved_arms,
                        },
                        encountered_type,
                    ))
                }
            },
        )
    }

    fn analyze_arm(
        &mut self,
        arm: &swamp_ast::MatchArm,
        _expression: &Expression,
        type_context: &TypeContext,
        expected_condition_type: &Type,
    ) -> Result<(MatchArm, bool), Error> {
        let (resolved_pattern, scope_was_pushed, anyone_wants_mutable) =
            self.analyze_pattern(&arm.pattern, expected_condition_type)?;

        let resolved_expression = self.analyze_expression(&arm.expression, type_context)?;
        if scope_was_pushed {
            self.pop_block_scope("analyze_arm");
        }

        let resolved_type = resolved_expression.ty.clone();

        Ok((
            MatchArm {
                pattern: resolved_pattern,
                expression: Box::from(resolved_expression),
                expression_type: resolved_type,
            },
            anyone_wants_mutable,
        ))
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

        /*
        if node_text == "false" {
            false
        } else if node_text == "true" {
            true
        } else {
            return Err(self.create_err(ErrorKind::BoolConversionError, ast_node));
        };

             */
    }

    fn analyze_pattern_literal(
        &mut self,
        node: &swamp_ast::Node,
        ast_literal: &swamp_ast::LiteralKind,
        expected_condition_type: &Type,
    ) -> Result<NormalPattern, Error> {
        let required_condition_type_context = TypeContext::new_argument(expected_condition_type);
        let (literal, ty) =
            self.analyze_literal(node, ast_literal, &required_condition_type_context)?;

        if !ty.compatible_with(expected_condition_type) {
            return Err(self.create_err(
                ErrorKind::IncompatibleTypes {
                    expected: expected_condition_type.clone(),
                    found: ty,
                },
                node,
            ));
        }

        Ok(NormalPattern::Literal(literal))
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
        &self,
        qualified_type_identifier: &swamp_ast::QualifiedTypeIdentifier,
        variant_name: &str,
    ) -> Result<EnumVariantType, Error> {
        let (symbol_table, enum_name) =
            self.get_symbol_table_and_name(qualified_type_identifier)?;
        symbol_table
            .get_enum_variant_type(&enum_name, variant_name)
            .ok_or_else(|| {
                self.create_err(
                    ErrorKind::UnknownEnumVariantType,
                    &qualified_type_identifier.name.0,
                )
            })
    }

    pub(crate) fn get_symbol_table_and_name(
        &self,
        type_identifier: &swamp_ast::QualifiedTypeIdentifier,
    ) -> Result<(&SymbolTable, String), Error> {
        let path = self.get_module_path(type_identifier.module_path.as_ref());
        let name = self.get_text(&type_identifier.name.0).to_string();

        let maybe_symbol_table = self.shared.get_symbol_table(&path);
        maybe_symbol_table.map_or_else(
            || Err(self.create_err(ErrorKind::UnknownModule, &type_identifier.name.0)),
            |symbol_table| Ok((symbol_table, name)),
        )
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
    ) -> Result<Expression, Error> {
        let mut variable_expressions = Vec::new();

        for variable in variables {
            let any_context = TypeContext::new_anything_argument();

            /*

            */

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
            )?;

            variable_expressions.push(var);
        }

        self.push_closed_block_scope();
        let mut expressions = Vec::new();
        for (variable_binding, resolved_expression) in variables.iter().zip(variable_expressions) {
            let initialize_variable_expression = self.create_variable_binding_for_with(
                &variable_binding.variable,
                resolved_expression,
            )?;
            expressions.push(initialize_variable_expression);
        }

        let resolved_expression = self.analyze_expression(expression, context)?;
        let block_type = resolved_expression.ty.clone();
        expressions.push(resolved_expression);

        let block_expression_kind = ExpressionKind::Block(expressions);
        self.pop_closed_block_scope();

        let block_expr = self.create_expr(block_expression_kind, block_type, &expression.node);
        Ok(block_expr)
    }

    fn analyze_when_expr(
        &mut self,
        context: &TypeContext,
        variables: &[swamp_ast::VariableBinding],
        true_expr: &swamp_ast::Expression,
        else_expr: Option<&swamp_ast::Expression>,
    ) -> Result<Expression, Error> {
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
                )?
            } else {
                let same_var = self.find_variable(&variable_binding.variable)?;

                if same_var.is_mutable() {
                    let loc = SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(&variable_binding.variable.name),
                        ty: Type::MutableReference(Box::from(same_var.resolved_type.clone())),
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

            let ty = mut_expr.ty();

            if let Type::Optional(found_ty) = ty {
                let variable_ref = self.create_variable(&variable_binding.variable, &found_ty)?;

                let binding = WhenBinding {
                    variable: variable_ref,
                    expr: mut_expr,
                };
                bindings.push(binding);
            } else {
                return Err(self.create_err(ErrorKind::ExpectedOptional, &true_expr.node));
            }
        }

        let resolved_true = self.analyze_expression(true_expr, context)?;
        let block_type = resolved_true.ty.clone();

        self.pop_block_scope("when");

        let maybe_resolved_else = if let Some(found_else) = else_expr {
            let block_type_for_true_context = context.we_know_expected_type(&block_type);
            Some(Box::new(self.analyze_expression(
                found_else,
                &block_type_for_true_context,
            )?))
        } else {
            None
        };

        let when_kind =
            ExpressionKind::When(bindings, Box::from(resolved_true), maybe_resolved_else);

        let block_expr = self.create_expr(when_kind, block_type, &true_expr.node);
        Ok(block_expr)
    }

    fn analyze_guard(
        &mut self,
        node: &swamp_ast::Node,
        context: &TypeContext,
        guard_expressions: &Vec<swamp_ast::GuardExpr>,
    ) -> Result<Expression, Error> {
        let mut guards = Vec::new();
        let mut found_wildcard = None;
        let mut detected_type = context.expected_type.cloned();

        for guard in guard_expressions {
            let resolved_condition = match &guard.clause {
                swamp_ast::GuardClause::Wildcard(x) => {
                    if found_wildcard.is_some() {
                        return Err(
                            self.create_err(ErrorKind::GuardCanNotHaveMultipleWildcards, node)
                        );
                    }
                    found_wildcard = Some(x);
                    None
                }
                swamp_ast::GuardClause::Expression(clause_expr) => {
                    if found_wildcard.is_some() {
                        return Err(self.create_err(ErrorKind::WildcardMustBeLastInGuard, node));
                    }
                    Some(self.analyze_bool_argument_expression(clause_expr)?)
                }
            };

            let resolved_result = self.analyze_expression(
                &guard.result,
                &context.with_expected_type(detected_type.as_ref()),
            )?;
            let ty = resolved_result.ty.clone();
            if detected_type.is_none() && !matches!(ty, Type::Never) {
                detected_type = Some(ty.clone());
            }

            guards.push(Guard {
                condition: resolved_condition,
                result: resolved_result,
            });
        }

        if found_wildcard.is_none() {
            return Err(self.create_err(ErrorKind::GuardMustHaveWildcard, node));
        }

        let kind = ExpressionKind::Guard(guards);

        detected_type.map_or_else(
            || Err(self.create_err(ErrorKind::GuardHasNoType, node)),
            |found_expecting_type| {
                let expr = self.create_expr(kind, found_expecting_type, node);
                Ok(expr)
            },
        )
    }

    fn analyze_lambda(
        &mut self,
        node: &swamp_ast::Node,
        variables: &[swamp_ast::Variable],
        ast_expr: &swamp_ast::Expression,
        context: &TypeContext,
    ) -> Result<Expression, Error> {
        let Type::Function(signature) = &context.expected_type.unwrap() else {
            return Err(self.create_err(ErrorKind::ExpectedLambda, node));
        };

        let return_block_type = TypeContext::new_argument(&signature.return_type);

        self.push_block_scope("lambda");

        let arity_required = signature.parameters.len();
        let variable_types_to_create = if variables.len() == arity_required {
            &signature.parameters
        } else if variables.len() + 1 == arity_required {
            &signature.parameters[1..].to_vec()
        } else {
            return Err(self.create_err(ErrorKind::WrongNumberOfArguments(0, 0), node));
        };

        let mut resolved_variables = Vec::new();
        for (variable, variable_type) in variables.iter().zip(variable_types_to_create) {
            let variable_ref = self.create_local_variable(
                &variable.name,
                Some(node),
                &variable_type.resolved_type,
            )?;
            resolved_variables.push(variable_ref);
        }

        let analyzed_expression = self.analyze_expression(ast_expr, &return_block_type)?;

        self.pop_block_scope("lambda");

        Ok(self.create_expr(
            ExpressionKind::Lambda(resolved_variables, Box::new(analyzed_expression)),
            Type::Function(signature.clone()),
            node,
        ))
    }

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
                PostfixKind::SliceSubscript(_, _) => {
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
                PostfixKind::VecSubscript(_, _) => todo!(),
                PostfixKind::MapSubscript(_, _) => todo!(),
            }
        }

        (is_owned_result, is_mutable)
    }

    pub fn check_assignment_mode(
        &mut self,
        lhs_is_mutable: bool,
        source_expression: &Expression,
        ty: &Type,
    ) -> AssignmentMode {
        if matches!(
            &source_expression.kind,
            ExpressionKind::Literal(_) | ExpressionKind::IntrinsicCallEx(_, _)
        ) {
            return AssignmentMode::OwnedValue;
        }

        if ty.is_direct() {
            return AssignmentMode::OwnedValue;
        }

        if let ExpressionKind::PostfixChain(start_chain, postfix) = &source_expression.kind {
            let (chain_is_owned, chain_is_mutable) =
                Self::chain_is_owned_result(start_chain, postfix);
            if lhs_is_mutable {
                if chain_is_mutable {
                    return AssignmentMode::CopyBlittable;
                } else if lhs_is_mutable {
                    if chain_is_owned {
                        return AssignmentMode::OwnedValue;
                    } else {
                        return AssignmentMode::CopyBlittable;
                    }
                } else {
                    return AssignmentMode::CopySharedPtr;
                }
            } else {
                // if not mutable, it is always ok
            }
        }

        AssignmentMode::CopyBlittable
    }

    pub fn check_mutable_assignment(
        &mut self,
        assignment_mode: AssignmentMode,
        node: &Node,
    ) -> Result<(), Error> {
        Ok(())
    }

    pub fn check_mutable_variable_assignment(
        &mut self,
        source_expression: &swamp_ast::Expression,
        ty: &Type,
        variable: &swamp_ast::Variable,
    ) -> Result<(), Error> {
        //let is_allowed_to_assign = Self::check_mutable_assignment_is_valid(ty);
        //if !is_allowed_to_assign {
        //  error!(?ty, "variable is wrong");
        //return Err(self.create_err(ErrorKind::ArgumentIsNotMutable, &variable.name));
        //}

        Ok(())
    }

    /// # Errors
    ///
    pub fn analyze_variable_assignment(
        &mut self,
        variable: &swamp_ast::Variable,
        source_expression: &swamp_ast::Expression,
    ) -> Result<Expression, Error> {
        let maybe_found_variable = self.try_find_variable(&variable.name);

        let required_type = maybe_found_variable
            .as_ref()
            .map(|found_variable| found_variable.resolved_type.clone());

        let source_expr = if let Some(target_type) = &required_type {
            self.analyze_expression_for_assignment_with_target_type(target_type, source_expression)?
        } else {
            let any_type_context = TypeContext::new_anything_argument();
            self.analyze_expression(source_expression, &any_type_context)?
        };
        let ty = source_expr.ty.clone();
        if !ty.can_be_stored_in_variable() {
            let debug_text = self.get_text(&variable.name);
            if !debug_text.starts_with('_') {
                return Err(self.create_err(ErrorKind::VariableTypeMustBeConcrete, &variable.name));
            }
        }

        let kind: ExpressionKind = if let Some(found_var) = maybe_found_variable {
            if !found_var.is_mutable() {
                return Err(self.create_err(ErrorKind::VariableIsNotMutable, &variable.name));
            }
            if !found_var
                .resolved_type
                .underlying()
                .assignable_type(&ty.underlying())
            {
                return Err(self.create_err(
                    ErrorKind::IncompatibleTypes {
                        expected: ty,
                        found: found_var.resolved_type.clone(),
                    },
                    &variable.name,
                ));
            }
            self.check_mutable_variable_assignment(source_expression, &ty, variable)?;
            ExpressionKind::VariableReassignment(found_var, Box::from(source_expr))
        } else {
            if !ty.can_be_stored_in_variable() {
                let text = self.get_text(&variable.name);
                error!(?text, ?required_type, ?source_expr, "variable is wrong");
            }

            // If it is mutable, we might need to clone the source
            // so make some extra verification
            if variable.is_mutable.is_some() {
                self.check_mutable_variable_assignment(source_expression, &ty, variable)?;
            }
            let new_var = self.create_variable(variable, &ty)?;
            ExpressionKind::VariableDefinition(new_var, Box::from(source_expr))
        };

        Ok(self.create_expr(kind, Type::Unit, &variable.name))
    }

    fn analyze_create_variable(
        &mut self,
        var: &swamp_ast::Variable,
        annotation_type: Option<&swamp_ast::Type>,
        source_expression: &swamp_ast::Expression,
    ) -> Result<Expression, Error> {
        let ty = if let Some(found_ast_type) = annotation_type {
            Some(self.analyze_type(found_ast_type)?)
        } else {
            None
        };

        let unsure_arg_context = TypeContext::new_unsure_argument(ty.as_ref());

        let resolved_source = self.analyze_expression(source_expression, &unsure_arg_context)?;

        let var_ref = self.create_local_variable(
            &var.name,
            Option::from(&var.is_mutable),
            &resolved_source.ty,
        )?;

        let resolved_type = resolved_source.ty.clone();
        assert_ne!(resolved_type, Type::Unit);
        let kind = ExpressionKind::VariableDefinition(var_ref, Box::from(resolved_source));

        let resolved_expr = self.create_expr(kind, Type::Unit, &var.name);

        Ok(resolved_expr)
    }

    fn add_location_item(
        &mut self,
        vec: &mut Vec<LocationAccess>,
        kind: LocationAccessKind,
        ty: Type,
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
    ) -> Result<SingleLocationExpression, Error> {
        let mut items = Vec::new();

        let nothing_context = TypeContext::new(None);

        let base_expr = self.analyze_expression(&chain.base, &nothing_context)?;
        let ExpressionKind::VariableAccess(start_variable) = base_expr.kind else {
            return Err(self.create_err(ErrorKind::NotValidLocationStartingPoint, &chain.base.node));
        };

        if !start_variable.is_mutable() {
            return Err(self.create_err(ErrorKind::VariableIsNotMutable, &chain.base.node));
        }

        let mut ty = start_variable.resolved_type.clone();
        for (i, item) in chain.postfixes.iter().enumerate() {
            match &item {
                swamp_ast::Postfix::FieldAccess(field_name_node) => {
                    //let field_name_resolved = self.to_node(field_name_node)
                    let (struct_type_ref, index, return_type) =
                        self.analyze_struct_field(field_name_node, &ty)?;

                    self.add_location_item(
                        &mut items,
                        LocationAccessKind::FieldIndex(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name_node,
                    );

                    ty = return_type.clone();
                }
                swamp_ast::Postfix::Subscript(key_expression) => {
                    let unsigned_int_context = TypeContext::new_argument(&Type::Int);
                    let unsigned_int_expr =
                        self.analyze_expression(key_expression, &unsigned_int_context)?;

                    match &ty.underlying() {
                        Type::DynamicSlice(element_type) => {
                            let slice_type = SliceType {
                                element: Box::new(*element_type.clone()),
                            };
                            self.add_location_item(
                                &mut items,
                                LocationAccessKind::Subscript(slice_type, unsigned_int_expr),
                                *element_type.clone(),
                                &key_expression.node,
                            );

                            ty = *element_type.clone();
                        }

                        Type::Vec(element_type) | Type::VecStorage(element_type, _) => {
                            let mut_type = Box::from(Type::MutableReference(element_type.clone()));
                            self.add_location_item(
                                &mut items,
                                LocationAccessKind::SubscriptVec(
                                    mut_type.clone(),
                                    unsigned_int_expr,
                                ),
                                *mut_type,
                                &key_expression.node,
                            );
                            ty = *element_type.clone();
                        }

                        _ => {
                            eprintln!("what is this: {ty}");
                            todo!()
                        }
                    }

                    /*
                    let is_last_in_chain = i == chain.postfixes.len() - 1;
                    let create_if_not_exists =
                        is_last_in_chain && location_side == LocationSide::Lhs;

                    let subscript_member_function_name = if create_if_not_exists {
                        "subscript_mut_create_if_needed"
                    } else {
                        "subscript_mut"
                    };

                    if let Some(found) = self
                        .shared
                        .state
                        .instantiator
                        .associated_impls
                        .get_internal_member_function(&ty, subscript_member_function_name)
                        .cloned()
                    {
                        let (intrinsic_to_call, original_arguments) =
                            Self::extract_single_intrinsic_call(&found.body)
                                .expect(&format!("must exist {subscript_member_function_name}"));

                        let create_if_not_exists_bool_expr = self.create_expr(
                            ExpressionKind::Literal(Literal::BoolLiteral(create_if_not_exists)),
                            Type::Bool,
                            &chain.base.node,
                        );

                        let required_type = &found.signature.signature.parameters[1].resolved_type;
                        let subscript_lookup_context = TypeContext::new_argument(required_type);
                        let analyzed_key_expression =
                            self.analyze_expression(key_expression, &subscript_lookup_context)?;
                        let return_type = *found.signature.signature.return_type.clone();
                        ty = return_type.clone();

                        self.add_location_item(
                            &mut items,
                            LocationAccessKind::IntrinsicSubscript(
                                intrinsic_to_call,
                                vec![analyzed_key_expression, create_if_not_exists_bool_expr],
                            ),
                            return_type,
                            &key_expression.node,
                        );
                    } else {
                        return Err(self
                            .create_err(ErrorKind::MissingSubscriptMember, &key_expression.node));
                    }

                     */
                }

                swamp_ast::Postfix::MemberCall(node, _generic_arguments, _regular_args) => {
                    return Err(self.create_err(ErrorKind::CallsCanNotBePartOfChain, node));
                }

                swamp_ast::Postfix::FunctionCall(node, _generic_arguments, _regular_args) => {
                    return Err(self.create_err(ErrorKind::CallsCanNotBePartOfChain, node));
                }
                swamp_ast::Postfix::OptionalChainingOperator(node) => {
                    return Err(self.create_err(ErrorKind::UnwrapCanNotBePartOfChain, node));
                }
                swamp_ast::Postfix::NoneCoalescingOperator(expr) => {
                    return Err(
                        self.create_err(ErrorKind::NoneCoalesceCanNotBePartOfChain, &expr.node)
                    );
                }
            }
        }

        if let Some(found_expected_type) = context.expected_type {
            if !ty.compatible_with(found_expected_type) {
                return Err(self.create_err(
                    ErrorKind::IncompatibleTypes {
                        expected: found_expected_type.clone(),
                        found: ty,
                    },
                    &chain.base.node,
                ));
            }
        }

        let location = SingleLocationExpression {
            kind: MutableReferenceKind::MutVariableRef,
            node: self.to_node(&chain.base.node),
            ty,
            starting_variable: start_variable,
            access_chain: items,
        };
        Ok(location)
    }

    fn analyze_to_location(
        &mut self,
        expr: &swamp_ast::Expression,
        context: &TypeContext,
        location_type: LocationSide,
    ) -> Result<SingleLocationExpression, Error> {
        match &expr.kind {
            swamp_ast::ExpressionKind::PostfixChain(chain) => {
                self.analyze_chain_to_location(chain, context, location_type)
            }
            swamp_ast::ExpressionKind::VariableReference(variable) => {
                let var = self.find_variable(variable)?;
                if var.is_mutable() {
                    Ok(SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(&variable.name),
                        ty: Type::MutableReference(Box::from(var.resolved_type.clone())),
                        starting_variable: var,
                        access_chain: vec![],
                    })
                } else {
                    Err(self.create_err(ErrorKind::VariableIsNotMutable, &expr.node))
                }
            }
            swamp_ast::ExpressionKind::IdentifierReference(qualified_identifier) => {
                let generated_var = swamp_ast::Variable {
                    name: qualified_identifier.name.clone(),
                    is_mutable: None,
                };
                let var = self.find_variable(&generated_var)?;
                if var.is_mutable() {
                    Ok(SingleLocationExpression {
                        kind: MutableReferenceKind::MutVariableRef,
                        node: self.to_node(&generated_var.name),
                        ty: Type::MutableReference(Box::from(var.resolved_type.clone())),
                        starting_variable: var,
                        access_chain: vec![],
                    })
                } else {
                    Err(self.create_err(ErrorKind::VariableIsNotMutable, &expr.node))
                }
            }
            _ => Err(self.create_err(ErrorKind::NotValidLocationStartingPoint, &expr.node)),
        }
    }

    fn analyze_expression_for_assignment_compound(
        &mut self,
        target_expression: &swamp_ast::Expression,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Result<(TargetAssignmentLocation, Expression), Error> {
        let any_argument_context = TypeContext::new_anything_argument();
        let source_expr = self.analyze_expression(ast_source_expression, &any_argument_context)?;
        let source_expr_type_context = TypeContext::new_argument(&source_expr.ty);

        let resolved_location = TargetAssignmentLocation(self.analyze_to_location(
            target_expression,
            &source_expr_type_context,
            LocationSide::Rhs,
        )?);

        Ok((resolved_location, source_expr))
    }

    /*
    fn try_convert_for_assignment(
        &mut self,
        expr: &Expression,
        target_type: &Type,
        ast_node: &swamp_ast::Expression,
    ) -> Result<Option<Expression>, Error> {
        match (target_type, &expr.ty) {
            // Conversion cases that require transformation
            (Type::VecStorage(target_elem, capacity), Type::FixedSlice(source_elem, _)) => {
                // Create conversion with checks
                // ...
            }

            (Type::String, Type::Int) => {
                // Create int-to-string conversion
                // ...
            }

            // No conversion possible
            _ => Ok(None),
        }
    }

     */

    fn is_type_assignment_compatible(target: &Type, source: &Type) -> bool {
        if target.compatible_with(source) {
            true
        } else {
            match (target, source) {
                (
                    Type::VecStorage(vec_element_type, capacity),
                    Type::DynamicSlice(slice_element_type),
                ) => vec_element_type.compatible_with(slice_element_type),
                _ => false,
            }
        }
    }

    fn analyze_expression_for_assignment_with_target_type(
        &mut self,
        target_type: &Type,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Result<Expression, Error> {
        assert!(target_type.is_concrete());

        let base_context = TypeContext::new_anything_argument();
        let source_expr = self.analyze_expression(ast_source_expression, &base_context)?;

        /*
        // previously checked for exact type
        let lhs_argument_context = TypeContext::new_argument(&ty);
        let source_expr = self.analyze_expression(ast_source_expression, &lhs_argument_context)?;
         */

        let final_expr = if Self::is_type_assignment_compatible(
            target_type.underlying(),
            &source_expr.ty.underlying(),
        ) {
            source_expr
        }
        /*else if let Some(converted) =
            self.try_convert_for_assignment(&source_expr, &target_type, ast_source_expression)?

        {
            converted
        }         */
        else {
            return Err(self.create_err(
                ErrorKind::IncompatibleTypesForAssignment {
                    expected: target_type.clone(),
                    found: source_expr.ty,
                },
                &ast_source_expression.node,
            ));
        };

        let assignment_mode = self.check_assignment_mode(true, &final_expr, target_type); // TODO: Fill in correct lhs_is_mutable

        self.check_mutable_assignment(assignment_mode, &final_expr.node)?;

        Ok(final_expr)
    }

    fn analyze_expression_for_assignment(
        &mut self,
        ast_target_location_expression: &swamp_ast::Expression,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Result<(TargetAssignmentLocation, Expression), Error> {
        let any_argument_context = TypeContext::new_anything_argument();
        let resolved_location = self.analyze_to_location(
            ast_target_location_expression,
            &any_argument_context,
            LocationSide::Lhs,
        )?;

        let target_type = resolved_location.ty.clone();
        let mut_type = Type::MutableReference(Box::from(target_type));
        let mut_location = TargetAssignmentLocation(resolved_location);

        let final_expr = self
            .analyze_expression_for_assignment_with_target_type(&mut_type, ast_source_expression)?;

        Ok((mut_location, final_expr))
    }

    fn analyze_assignment_compound(
        &mut self,
        target_expression: &swamp_ast::Expression,
        ast_op: &swamp_ast::CompoundOperator,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Result<Expression, Error> {
        let resolved_op = self.analyze_compound_operator(ast_op);

        let (resolved_location, source_expr) = self
            .analyze_expression_for_assignment_compound(target_expression, ast_source_expression)?;

        let kind = ExpressionKind::CompoundAssignment(
            resolved_location,
            resolved_op.kind,
            Box::from(source_expr),
        );

        let expr = self.create_expr(kind, Type::Unit, &target_expression.node);

        Ok(expr)
    }

    fn analyze_assignment_mode(lhs: SingleLocationExpression) {}

    fn analyze_assignment(
        &mut self,
        target_location: &swamp_ast::Expression,
        ast_source_expression: &swamp_ast::Expression,
    ) -> Result<Expression, Error> {
        let (mut_location, source_expr) =
            self.analyze_expression_for_assignment(target_location, ast_source_expression)?;
        let kind = ExpressionKind::Assignment(Box::from(mut_location), Box::from(source_expr));

        let expr = self.create_expr(kind, Type::Unit, &target_location.node); // Assignments are always of type Unit

        Ok(expr)
    }

    #[must_use]
    pub const fn create_expr(
        &self,
        kind: ExpressionKind,
        ty: Type,
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
        ty: Type,
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
    ) -> Result<Expression, Error> {
        let any_context = TypeContext::new_anything_argument();
        let tuple_resolved = self.analyze_expression(tuple_expression, &any_context)?;
        let tuple_expr_type = &tuple_resolved.ty;

        let mut variable_refs = Vec::new();
        if let Type::Tuple(tuple) = tuple_expr_type.clone() {
            if target_ast_variables.len() > tuple.len() {
                return Err(self.create_err(ErrorKind::TooManyDestructureVariables, node));
            }
            for (ast_variable, tuple_type) in target_ast_variables.iter().zip(tuple.clone()) {
                let variable_ref = self.create_local_variable(
                    &ast_variable.name,
                    ast_variable.is_mutable.as_ref(),
                    &tuple_type,
                )?;
                variable_refs.push(variable_ref);
            }
            let expr_kind =
                ExpressionKind::TupleDestructuring(variable_refs, tuple, Box::from(tuple_resolved));

            Ok(self.create_expr(expr_kind, Type::Unit, node))
        } else {
            Err(self.create_err(ErrorKind::CanNotDestructure, node))
        }
    }

    pub fn instantiate_signature_if_needed(
        &mut self,
        node: &Node,
        type_that_member_is_on: &Type,
        found_function: &FunctionRef,
        generic_arguments: &[Type],
    ) -> Result<Signature, Error> {
        let (maybe_generic, alternative_signature) = found_function.signatures();

        let signature = if let Some(found_generic) = maybe_generic {
            let mut seq_map = SeqMap::new();
            if generic_arguments.len() != found_generic.generic_type_variables.len() {
                return Err(self.create_err_resolved(ErrorKind::WrongNumberOfArguments(0, 0), node));
            }
            for (variable, generic_argument_type) in found_generic
                .generic_type_variables
                .iter()
                .zip(generic_arguments)
            {
                info!(?variable, ?generic_argument_type, "SETTING VAR");
                seq_map
                    .insert(variable.0.to_string(), generic_argument_type.clone())
                    .unwrap();
            }
            let mut scope = TypeVariableScope::new(seq_map);

            scope = scope.with_variables(&found_generic.generic_type_variables)?;

            let instantiated_signature = self
                .shared
                .state
                .instantiator
                .instantiate_signature(type_that_member_is_on, &found_generic.signature, &scope)?
                .clone();

            instantiated_signature.clone()
        } else {
            alternative_signature.clone()
        };

        Ok(signature)
    }

    pub fn instantiate_signature_without_self_if_needed(
        &mut self,
        node: &Node,
        found_function: FunctionRef,
        generic_arguments: &[Type],
    ) -> Result<Signature, Error> {
        let (maybe_generic, alternative_signature) = found_function.signatures();

        let signature = if let Some(found_generic) = maybe_generic {
            let mut seq_map = SeqMap::new();
            if generic_arguments.len() != found_generic.generic_type_variables.len() {
                return Err(self.create_err_resolved(ErrorKind::WrongNumberOfArguments(0, 0), node));
            }
            for (variable, generic_argument_type) in found_generic
                .generic_type_variables
                .iter()
                .zip(generic_arguments)
            {
                info!(?variable, ?generic_argument_type, "SETTING VAR");
                seq_map
                    .insert(variable.0.to_string(), generic_argument_type.clone())
                    .unwrap();
            }
            let mut scope = TypeVariableScope::new(seq_map);

            scope = scope.with_variables(&found_generic.generic_type_variables)?;

            self.shared
                .state
                .instantiator
                .instantiate_signature_without_self(&found_generic.signature, &scope)?
        } else {
            alternative_signature.clone()
        };

        Ok(signature)
    }

    fn analyze_normal_member_call(
        &mut self,
        type_that_member_is_on: &Type,
        found_function: &FunctionRef,
        generic_arguments: Vec<Type>,
        ast_arguments: &[swamp_ast::Expression],
        is_mutable: bool,
        node: &swamp_ast::Node,
    ) -> Result<Signature, Error> {
        let resolved_node = self.to_node(node);
        let signature = self.instantiate_signature_if_needed(
            &resolved_node,
            type_that_member_is_on,
            found_function,
            &generic_arguments,
        )?;

        Ok(signature)
    }

    fn vec_member_signature(
        &mut self,
        self_type: &Type,
        element_type: &Type,
        field_name_str: &str,
        node: &swamp_ast::Node,
    ) -> Result<(IntrinsicFunction, Signature), Error> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: self_type.clone(),
            is_mutable: true,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "push" => (
                IntrinsicFunction::VecPush,
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
                    return_type: Box::new(Type::Unit),
                },
            ),
            _ => { self.slice_member_signature(element_type, field_name_str, node) }?,
        };
        Ok(intrinsic_and_signature)
    }

    fn slice_member_signature(
        &mut self,
        element_type: &Type,
        field_name_str: &str,
        node: &swamp_ast::Node,
    ) -> Result<(IntrinsicFunction, Signature), Error> {
        let self_type_param = TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::DynamicSlice(Box::from(element_type.clone())).clone(),
            is_mutable: false,
            node: None,
        };
        let intrinsic_and_signature = match field_name_str {
            "for" => {
                let signature = Signature {
                    parameters: vec![TypeForParameter {
                        name: "element".to_string(),
                        resolved_type: element_type.clone(),
                        is_mutable: false,
                        node: None,
                    }],
                    return_type: Box::new(Type::Unit),
                };
                let lambda_function_type = Type::Function(signature);
                (
                    IntrinsicFunction::VecFor,
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
                        return_type: Box::new(Type::Unit), // VecFor is only used for side effects
                    },
                )
            }
            "filter" => {
                let signature = Signature {
                    parameters: vec![TypeForParameter {
                        name: "element".to_string(),
                        resolved_type: element_type.clone(),
                        is_mutable: false,
                        node: None,
                    }],
                    return_type: Box::new(Type::Bool),
                };
                let lambda_function_type = Type::Function(signature);
                (
                    IntrinsicFunction::VecFilter,
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
                        return_type: Box::new(Type::Vec(Box::from(element_type.clone()))),
                    },
                )
            }
            "len" => {
                let signature = Signature {
                    parameters: vec![self_type_param],
                    return_type: Box::new(Type::Int),
                };
                (IntrinsicFunction::VecLen, signature)
            }
            _ => {
                return Err(self.create_err(
                    ErrorKind::UnknownMemberFunction(Type::Vec(Box::from(element_type.clone()))),
                    node,
                ));
            }
        };
        Ok(intrinsic_and_signature)
    }

    fn check_intrinsic_member_signature(
        &mut self,
        type_that_member_is_on: &Type,
        field_name_str: &str,
        node: &swamp_ast::Node,
    ) -> Result<(IntrinsicFunction, Signature), Error> {
        let ty = type_that_member_is_on.underlying();
        match ty {
            Type::VecStorage(element_type, ..) => self.vec_member_signature(
                type_that_member_is_on,
                element_type,
                field_name_str,
                node,
            ),
            Type::Vec(element_type) => self.vec_member_signature(
                type_that_member_is_on,
                element_type,
                field_name_str,
                node,
            ),
            Type::DynamicSlice(element_type) => {
                self.slice_member_signature(element_type, field_name_str, node)
            }
            _ => Err(self.create_err(
                ErrorKind::UnknownMemberFunction(type_that_member_is_on.clone()),
                node,
            )),
        }
    }

    fn analyze_member_call(
        &mut self,
        type_that_member_is_on: &Type,
        field_name_str: &str,
        ast_maybe_generic_arguments: Option<Vec<swamp_ast::GenericParameter>>,
        ast_arguments: &[swamp_ast::Expression],
        is_mutable: bool,
        node: &swamp_ast::Node,
    ) -> Result<(PostfixKind, Type), Error> {
        let generic_arguments = if let Some(ast_generic_arguments) = ast_maybe_generic_arguments {
            let mut resolved_types = Vec::new();
            for ast_type in ast_generic_arguments {
                resolved_types.push(self.analyze_type(ast_type.get_type())?);
            }
            resolved_types
        } else {
            vec![]
        };

        let maybe_function = self
            .shared
            .state
            .instantiator
            .associated_impls
            .get_member_function(type_that_member_is_on, field_name_str)
            .cloned();

        let (function_ref, instantiated_signature) = if let Some(found_function) = maybe_function {
            let signature = self.analyze_normal_member_call(
                type_that_member_is_on,
                &found_function,
                generic_arguments,
                ast_arguments,
                is_mutable,
                node,
            )?;
            (found_function, signature)
        } else {
            let (intrinsic_fn, signature) = self.check_intrinsic_member_signature(
                type_that_member_is_on,
                field_name_str,
                node,
            )?;
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

        let self_type = &instantiated_signature.parameters[0];

        if !self_type
            .resolved_type
            .compatible_with(type_that_member_is_on)
            || self_type.is_mutable && !is_mutable
        {
            info!(?self_type.resolved_type, ?type_that_member_is_on, "debugging");
            return Err(self.create_err(ErrorKind::SelfNotCorrectType, node));
        }

        let resolved_arguments = self.analyze_and_verify_parameters(
            node,
            &instantiated_signature.parameters[1..],
            ast_arguments,
        )?;

        Ok((
            PostfixKind::MemberCall(function_ref, resolved_arguments),
            *instantiated_signature.return_type.clone(),
        ))
    }

    fn analyze_postfix_member_call(
        &mut self,
        type_that_member_is_on: &Type,
        is_mutable: bool,
        member_name: &swamp_ast::Node,
        ast_maybe_generic_arguments: Option<Vec<swamp_ast::GenericParameter>>,
        ast_arguments: &[swamp_ast::Expression],
        suffixes: &mut Vec<Postfix>,
    ) -> Result<Type, Error> {
        let field_name_str = self.get_text(member_name).to_string();

        let resolved_node = self.to_node(member_name);

        let (kind, return_type) = self.analyze_member_call(
            type_that_member_is_on,
            &field_name_str,
            ast_maybe_generic_arguments,
            ast_arguments,
            is_mutable,
            member_name,
        )?;
        let postfix = Postfix {
            node: resolved_node.clone(),
            ty: return_type.clone(),
            kind,
        };

        let last_type = postfix.ty.clone();
        suffixes.push(postfix);

        Ok(last_type)
    }

    /*
    fn late_coerce_slice_pair(
        &mut self,
        ast_node: &swamp_ast::Node,
        found_expected_type: &Type,
        hopefully_slice_pair_expr: &Expression,
    ) -> Result<Expression, Error> {
        let return_type = if let Some(found) = self
            .shared
            .state
            .instantiator
            .associated_impls
            .get_internal_member_function(found_expected_type, "new_from_slice_pair")
        {
            found.signature.signature.return_type.clone()
        } else {
            return Err(self.create_err(
                ErrorKind::MissingMemberFunction(
                    "new_from_slice".to_string(),
                    found_expected_type.clone(),
                ),
                ast_node,
            ));
        };

        let Type::SlicePair(key_type, value_type) = &hopefully_slice_pair_expr.ty else {
            panic!("must be slice pair")
        };

        assert!(key_type.is_concrete_or_unit(), "{key_type:?}");
        assert!(value_type.is_concrete_or_unit());

        let mut_or_immute =
            MutRefOrImmutableExpression::Expression(hopefully_slice_pair_expr.clone());
        let call_kind = self.create_static_member_intrinsic_call(
            "new_from_slice_pair",
            &[mut_or_immute],
            ast_node,
            found_expected_type,
        )?;
        Ok(self.create_expr(call_kind, *return_type.clone(), ast_node))
    }


    fn late_coerce_slice(
        &mut self,
        ast_node: &swamp_ast::Node,
        found_expected_type: &Type,
        hopefully_slice_expr: &Expression,
    ) -> Result<Expression, Error> {
        let return_type = if let Some(found) = self
            .shared
            .state
            .instantiator
            .associated_impls
            .get_internal_member_function(found_expected_type, "new_from_slice")
        {
            found.signature.signature.return_type.clone()
        } else {
            return Err(self.create_err(
                ErrorKind::MissingMemberFunction(
                    "new_from_slice".to_string(),
                    found_expected_type.clone(),
                ),
                ast_node,
            ));
        };
        let Type::Slice(found_slice_element_type) = &hopefully_slice_expr.ty else {
            panic!("must be slice")
        };

        assert!(found_slice_element_type.is_concrete());

        let mut_or_immute = MutRefOrImmutableExpression::Expression(hopefully_slice_expr.clone());
        let call_kind = self.create_static_member_intrinsic_call(
            "new_from_slice",
            &[mut_or_immute],
            ast_node,
            found_expected_type,
        )?;
        Ok(self.create_expr(call_kind, *return_type.clone(), ast_node))
    }

    */

    fn types_did_not_match_try_late_coerce_expression(
        &mut self,
        expr: Expression,
        special_expected_type: &Type,
        special_encountered_type: &Type,
        node: &swamp_ast::Node,
    ) -> Result<Expression, Error> {
        let expected_type = special_expected_type.underlying();
        let encountered_type = special_encountered_type.underlying();
        if let Type::Optional(expected_inner_type) = expected_type {
            // If an optional is expected, we can wrap it if this type has the exact same
            // inner type
            assert!(
                expected_inner_type
                    .inner_optional_mut_or_immutable()
                    .is_none()
            );

            // First make sure it is not already an optional type. we can not wrap an option with an option
            if encountered_type.inner_optional_mut_or_immutable().is_none() {
                // good it isn't, lets see if they share inner types
                if expected_inner_type.compatible_ignore_mutability_of(encountered_type) {
                    // they share inner types as well, lets wrap it up
                    let wrapped = self.create_expr(
                        ExpressionKind::Option(Option::from(Box::new(expr))),
                        expected_type.clone(),
                        node,
                    );
                    return Ok(wrapped);
                }
            }
        }
        if let Type::VecStorage(vec_element_type, vec_capacity) = expected_type {
            if let Type::DynamicSlice(slice_element_type) = &encountered_type {
                if vec_element_type.compatible_with(slice_element_type) {
                    return Ok(expr);
                } else {
                    panic!(
                        "{}",
                        format!(
                            "types are not the same {vec_element_type} slice {slice_element_type}"
                        )
                    );
                }
                //  TODO : Add better error message
            }
        }

        if let Type::VecStorage(vec_element_type, vec_capacity) = encountered_type {
            if let Type::DynamicSlice(slice_element_type) = &expected_type {
                if vec_element_type.compatible_with(slice_element_type) {
                    return Ok(expr);
                } else {
                    panic!(
                        "{}",
                        format!(
                            "types are not the same {vec_element_type} slice {slice_element_type}"
                        )
                    );
                }
                //  TODO : Add better error message
            }
        }

        if let Type::MapStorage(storage_key, storage_value, _) = expected_type {
            if let Type::DynamicSlicePair(slice_key, slice_value) = &encountered_type {
                if slice_key.compatible_with(storage_key)
                    && slice_value.compatible_with(storage_value)
                {
                    return Ok(expr);
                } else {
                    panic!(
                        "{}",
                        format!(
                            "types are not the same {storage_key} {storage_value} slice {slice_key} {slice_value}"
                        )
                    );
                }
                //  TODO : Add better error message
            }
        }
        /*else if let Type::FixedSlice(encountered_type_slice_type, ..) = encountered_type {
            return self.late_coerce_slice(node, expected_type, encountered_type_slice_type, &expr);
        }
        */

        /*
        else if let Type::SlicePair(first_type, second_type) = encountered_type {
            return self.late_coerce_slice_pair(node, expected_type, &expr);

        } */
        else if matches!(expected_type, &Type::Bool) {
            // if it has a mut or immutable optional, then it works well to wrap it
            if encountered_type.inner_optional_mut_or_immutable().is_some() {
                let wrapped = self.create_expr(
                    ExpressionKind::CoerceOptionToBool(Box::from(expr)),
                    Type::Bool,
                    node,
                );
                return Ok(wrapped);
            }
        }

        error!(?expected_type, ?encountered_type, "incompatible");
        Err(self.create_err(
            ErrorKind::IncompatibleTypes {
                expected: expected_type.clone(),
                found: encountered_type.clone(),
            },
            node,
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_generic_type(
        &mut self,
        symbol: &Symbol,
        analyzed_type_parameters: &[Type],
        node: &swamp_ast::Node,
    ) -> Result<Type, Error> {
        let ty = match symbol {
            //            Symbol::Type(base_type) => base_type.clone(),
            //          Symbol::Alias(alias_type) => alias_type.referenced_type.clone(),
            Symbol::Blueprint(blueprint) => {
                if all_types_are_concrete_or_unit(analyzed_type_parameters) {
                    self.shared
                        .state
                        .instantiator
                        .instantiate_blueprint_and_members(blueprint, analyzed_type_parameters)?
                } else {
                    Type::Generic(blueprint.clone(), analyzed_type_parameters.to_vec())
                }
            }
            Symbol::TypeGenerator(type_gen) => match type_gen.kind {
                TypeGeneratorKind::Slice => {
                    //assert!(analyzed_type_parameters[0].is_concrete());
                    Type::DynamicSlice(Box::new(analyzed_type_parameters[0].clone()))
                }
                TypeGeneratorKind::SlicePair => {
                    //assert!(analyzed_type_parameters[0].is_concrete());
                    //assert!(analyzed_type_parameters[1].is_concrete());
                    Type::DynamicSlicePair(
                        Box::new(analyzed_type_parameters[0].clone()),
                        Box::new(analyzed_type_parameters[1].clone()),
                    )
                }
            },
            _ => return Err(self.create_err(ErrorKind::UnknownSymbol, node)),
        };

        Ok(ty)
    }

    fn late_coerce_slice(
        &self,
        node: &swamp_ast::Node,
        expected: &Type,
        encountered: &Type,
        expr: &Expression,
    ) -> Result<Expression, Error> {
        Err(self.create_err(
            ErrorKind::IncompatibleTypes {
                expected: expected.clone(),
                found: encountered.clone(),
            },
            node,
        ))
    }
}
