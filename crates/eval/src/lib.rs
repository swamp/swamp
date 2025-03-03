/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::block::BlockScopes;
use crate::err::ExecuteErrorKind;
use crate::prelude::{ValueReference, VariableValue};
use err::ExecuteError;
use seq_map::SeqMap;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use swamp_script_core_extra::extra::{SparseValueId, SparseValueMap};
use swamp_script_core_extra::prelude::ValueError;
use swamp_script_core_extra::value::ValueRef;
use swamp_script_core_extra::value::{
    SourceMapLookup, Value, convert_vec_to_rc_refcell, format_value, to_rust_value,
};
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{ArgumentExpressionOrLocation, LocationAccess, LocationAccessKind};
use swamp_script_semantic::{
    BinaryOperatorKind, CompoundOperatorKind, ConstantId, ForPattern, Function,
    MutOrImmutableExpression, NormalPattern, PatternElement, PostfixKind, SingleLocationExpression,
    SingleLocationExpressionKind, UnaryOperatorKind, same_struct_ref,
};
use swamp_script_semantic::{Postfix, SingleMutLocationExpression, same_array_ref};
use tracing::{error, info};

pub mod err;

mod block;
pub mod prelude;
pub mod value_both;
pub mod value_ref;

impl From<ValueError> for ExecuteError {
    fn from(value: ValueError) -> Self {
        Self {
            kind: ExecuteErrorKind::ValueError(value),
            node: Default::default(),
        }
    }
}

type RawFunctionFn<C> = dyn FnMut(&[VariableValue], &mut C) -> Result<Value, ExecuteError>;

type FunctionFn<C> = Box<RawFunctionFn<C>>;

#[derive(Debug)]
pub enum FunctionData {
    Internal(InternalFunctionDefinitionRef),
    External(ExternalFunctionId),
}

pub struct EvalExternalFunction<C> {
    pub func: FunctionFn<C>,
    pub id: ExternalFunctionId,
}

impl<C> Debug for EvalExternalFunction<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "external_fn {}", self.id)
    }
}

pub type EvalExternalFunctionRef<C> = Rc<RefCell<EvalExternalFunction<C>>>;

#[derive(Debug)]
pub enum ValueWithSignal {
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

impl TryFrom<ValueWithSignal> for Value {
    type Error = String;

    fn try_from(value: ValueWithSignal) -> Result<Self, Self::Error> {
        match value {
            ValueWithSignal::Value(v) => Ok(v),
            ValueWithSignal::Return(v) => Ok(v),
            ValueWithSignal::Break => Err("break can not be converted".to_string()),
            ValueWithSignal::Continue => Err("continue can not be converted".to_string()),
        }
    }
}

#[derive(Default)]
struct FunctionScope {
    saved_block_scope: BlockScopes,
}

#[derive(Debug, Default)]
pub struct ExternalFunctions<C> {
    //    external_functions: HashMap<String, EvalExternalFunctionRef<C>>,
    external_functions_by_id: HashMap<ExternalFunctionId, EvalExternalFunctionRef<C>>,
}

#[derive(Debug)]
pub struct Constants {
    pub values: Vec<Value>,
}

impl Default for Constants {
    fn default() -> Self {
        Self::new()
    }
}

impl Constants {
    #[must_use]
    pub fn lookup_constant_value(&self, id: ConstantId) -> &Value {
        let x = &self.values[id as usize];
        assert_ne!(*x, Value::Unit, "illegal constant");
        x
    }

    pub fn set(&mut self, id: ConstantId, value: Value) {
        self.values[id as usize] = value;
    }

    #[must_use]
    pub fn new() -> Self {
        let arr: [Value; 1024] = core::array::from_fn(|_| Value::Unit);
        Self {
            values: arr.to_vec(),
        }
    }
}

impl<C> ExternalFunctions<C> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            external_functions_by_id: HashMap::new(),
        }
    }

    pub fn register_external_function(
        &mut self,
        function_id: ExternalFunctionId,
        handler: impl FnMut(&[VariableValue], &mut C) -> Result<Value, ExecuteError> + 'static,
    ) -> Result<(), String> {
        let external_func = EvalExternalFunction {
            func: Box::new(handler),
            id: function_id,
        };

        let external_func_ref = Rc::new(RefCell::new(external_func));

        self.external_functions_by_id
            .insert(function_id, external_func_ref.clone());

        Ok(())
    }
}

pub fn eval_module<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    root_expression: &Expression,
    debug_source_map: Option<&dyn SourceMapLookup>,
    context: &mut C,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    let value_with_signal = interpreter.evaluate_expression_with_signal(root_expression)?;
    Ok(value_with_signal.try_into().map_err(|_| ExecuteError {
        node: Default::default(),
        kind: ExecuteErrorKind::CouldNotConvertFromSignal,
    })?)
}

pub fn eval_constants<C>(
    externals: &ExternalFunctions<C>,
    eval_constants: &mut Constants,
    program_state: &ProgramState,
    context: &mut C,
) -> Result<(), ExecuteError> {
    for constant in &program_state.constants_in_dependency_order {
        let mut interpreter = Interpreter::<C>::new(externals, eval_constants, context);
        let value = interpreter.evaluate_expression(&constant.expr)?;
        eval_constants.set(constant.id, value);
    }

    Ok(())
}

pub fn util_execute_function<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    func: &InternalFunctionDefinitionRef,
    arguments: &[VariableValue],
    context: &mut C,
    debug_source_map: Option<&dyn SourceMapLookup>,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    interpreter.bind_parameters(&func.body.node, &func.signature.parameters, &arguments)?;
    let value = interpreter.evaluate_expression(&func.body)?;
    interpreter.current_block_scopes.clear();
    interpreter.function_scope_stack.clear();
    Ok(value)
}

pub fn util_execute_expression<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    expr: &Expression,
    context: &mut C,
    debug_source_map: Option<&dyn SourceMapLookup>,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    let value = interpreter.evaluate_expression(expr)?;
    interpreter.current_block_scopes.clear();
    interpreter.function_scope_stack.clear();
    Ok(value)
}

pub fn util_execute_member_function_mut<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    fn_def: &InternalFunctionDefinitionRef,
    self_value_ref: ValueRef,
    arguments: &[Value],
    context: &mut C,
    debug_source_map: Option<&dyn SourceMapLookup>,
) -> Result<Value, ExecuteError> {
    let mut complete_arguments = Vec::new();
    complete_arguments.push(VariableValue::Reference(self_value_ref));
    for arg in arguments {
        complete_arguments.push(VariableValue::Value(arg.clone()));
    }

    let value = util_execute_function(
        externals,
        constants,
        fn_def,
        &complete_arguments,
        context,
        debug_source_map,
    )?;

    Ok(value)
}

pub struct Interpreter<'a, C> {
    function_scope_stack: Vec<FunctionScope>,
    current_block_scopes: BlockScopes,
    constants: &'a Constants,
    externals: &'a ExternalFunctions<C>,
    context: &'a mut C,
    debug_source_map: Option<&'a dyn SourceMapLookup>,
    depth: usize,
}

impl<'a, C> Interpreter<'a, C> {
    pub fn new(
        externals: &'a ExternalFunctions<C>,
        constants: &'a Constants,
        context: &'a mut C,
    ) -> Self {
        Self {
            function_scope_stack: vec![FunctionScope::default()],
            current_block_scopes: BlockScopes::default(),
            externals,
            context,
            debug_source_map: None,
            constants,
            depth: 0,
        }
    }

    #[inline]
    fn push_function_scope(&mut self) {
        self.function_scope_stack.push(FunctionScope {
            saved_block_scope: self.current_block_scopes.clone(),
        });

        self.current_block_scopes.clear();
        self.push_block_scope();
    }

    #[inline]
    fn push_block_scope(&mut self) {
        self.current_block_scopes.push();
    }

    #[inline]
    fn pop_block_scope(&mut self) {
        self.current_block_scopes.pop();
    }

    #[inline]
    fn pop_function_scope(&mut self) {
        assert_ne!(self.function_scope_stack.len(), 1, "you popped too far");
        let last_one = self.function_scope_stack.pop().expect("pop function scope");
        self.current_block_scopes = last_one.saved_block_scope;
    }

    fn bind_parameters(
        &mut self,
        node: &Node,
        params: &[TypeForParameter],
        args: &[VariableValue],
    ) -> Result<(), ExecuteError> {
        for (index, (param, arg)) in params.iter().zip(args).enumerate() {
            let complete_value = if param.is_mutable {
                match arg {
                    VariableValue::Reference(_r) => {
                        // For mutable parameters, use the SAME reference
                        arg.clone()
                    }
                    _ => return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, node)),
                }
            } else {
                match arg {
                    VariableValue::Reference(r) => VariableValue::Value(r.borrow().clone()),
                    VariableValue::Value(v) => VariableValue::Value(v.clone()),
                }
            };

            self.current_block_scopes
                .set_local_var_ex(index, complete_value, param.is_mutable)?;
        }

        Ok(())
    }
    fn evaluate_function_call(
        &mut self,
        function_expression: &Expression,
        arguments: &[ArgumentExpressionOrLocation],
    ) -> Result<Value, ExecuteError> {
        let func_val = self.evaluate_expression(function_expression)?;
        let evaluated_args = self.evaluate_args(arguments)?;

        match &func_val {
            Value::InternalFunction(internal_func_ref) => {
                self.push_function_scope();

                self.bind_parameters(
                    &internal_func_ref.body.node,
                    &internal_func_ref.signature.parameters,
                    &evaluated_args,
                )?;

                let result = self.evaluate_expression(&internal_func_ref.body)?;

                self.pop_function_scope();

                Ok(result)
            }

            Value::ExternalFunction(external_function_ref) => {
                let external_function_id = &external_function_ref.id;
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external_function_id)
                    .ok_or(self.create_err(
                        ExecuteErrorKind::MissingExternalFunction(*external_function_id),
                        &function_expression.node,
                    ))?
                    .borrow_mut();

                (func.func)(&evaluated_args, self.context)
            }
            _ => Err(self.create_err(
                ExecuteErrorKind::ExpectedFunction,
                &function_expression.node,
            )),
        }
    }

    fn evaluate_location_chain(
        &mut self,
        node: &Node,
        start_value_reference: ValueRef,
        chain_items: &Vec<LocationAccess>,
    ) -> Result<ValueRef, ExecuteError> {
        let mut value_ref = start_value_reference;
        for chain in chain_items {
            value_ref = {
                match &chain.kind {
                    LocationAccessKind::FieldIndex(_resolved_node, index) => {
                        let borrowed = value_ref.borrow();

                        let (_struct_ref, fields) = borrowed
                            .expect_struct()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedStruct, node))?;
                        fields[*index].clone()
                    }
                    LocationAccessKind::ArrayIndex(_array_type_ref, index_expr) => {
                        let index = self
                            .evaluate_expression(index_expr)?
                            .expect_int()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedArray, node))?;

                        let borrowed = value_ref.borrow();

                        let (_array_ref, fields) = borrowed
                            .expect_array()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedArray, node))?;
                        fields[index as usize].clone()
                    }

                    LocationAccessKind::ArrayRange(_, _) => todo!(),
                    LocationAccessKind::StringIndex(_) => todo!(),
                    LocationAccessKind::StringRange(_) => todo!(),

                    LocationAccessKind::MapIndex(_map_type_ref, key_expr) => {
                        let key_expr_value = self.evaluate_expression(key_expr)?;

                        let borrowed = value_ref.borrow();

                        let (_map_type_ref, seq_map) = borrowed
                            .expect_map()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedMap, node))?;

                        let maybe_found = seq_map.get(&key_expr_value);

                        wrap_in_option(maybe_found)
                    }

                    LocationAccessKind::MapIndexInsertIfNonExisting(_map_type_ref, key_expr) => {
                        let key_expr_value = self.evaluate_expression(key_expr)?;
                        let key = key_expr_value.clone();
                        let mut borrowed_mut = value_ref.borrow_mut();
                        let found_memory = {
                            let (_, seq_map_mutable) =
                                borrowed_mut.expect_map_mut().map_err(|_| {
                                    self.create_err(ExecuteErrorKind::ExpectedMap, node)
                                })?;
                            seq_map_mutable.get(&key).cloned()
                        };
                        match found_memory {
                            Some(found) => found,
                            _ => {
                                let (_, seq_map_mutable) =
                                    borrowed_mut.expect_map_mut().map_err(|_| {
                                        self.create_err(ExecuteErrorKind::ExpectedMap, node)
                                    })?;
                                let default_value = Rc::new(RefCell::new(Value::Unit));
                                seq_map_mutable
                                    .insert(key, default_value.clone())
                                    .expect("insert should work");
                                default_value
                            }
                        }
                    }

                    LocationAccessKind::ExternalTypeIndex(_rust_type_ref, key_expr) => {
                        let key_expr_value = self.evaluate_expression(key_expr)?;
                        match key_expr_value.downcast_rust::<SparseValueId>() {
                            Some(found_sparse_id) => {
                                match value_ref.borrow_mut().downcast_rust::<SparseValueMap>() {
                                    Some(sparse_value_map) => {
                                        let inner_map = sparse_value_map.borrow_mut();
                                        wrap_in_option(
                                            inner_map.get(&found_sparse_id.borrow()).clone(),
                                        )
                                    }
                                    _ => {
                                        return Err(
                                            self.create_err(ExecuteErrorKind::ExpectedArray, node)
                                        )?;
                                    }
                                }
                            }
                            _ => {
                                return Err(self.create_err(ExecuteErrorKind::ExpectedArray, node))?;
                            }
                        }
                    }
                }
            };
        }

        Ok(value_ref)
    }

    fn evaluate_location(
        &mut self,
        found_location_expr: &SingleLocationExpression,
    ) -> Result<ValueRef, ExecuteError> {
        let variable_ref = self
            .current_block_scopes
            .lookup_variable_mut_ref(&found_location_expr.starting_variable)?;

        let value_ref = self.evaluate_location_chain(
            &found_location_expr.node,
            variable_ref.clone(),
            &found_location_expr.access_chain,
        )?;

        let converted_value_ref = match &found_location_expr.kind {
            SingleLocationExpressionKind::MutVariableRef => value_ref,
            SingleLocationExpressionKind::MutStructFieldRef(_base_expression, _resolved_access) => {
                value_ref
            }
            SingleLocationExpressionKind::MutArrayIndexRef(_resolved_array_ref) => {
                //info!(?base_expression, "base expression for field access");
                let (_struct, _values) = value_ref.borrow().expect_array().map_err(|_| {
                    self.create_err(ExecuteErrorKind::ExpectedArray, &found_location_expr.node)
                })?;
                value_ref
            }
            _ => {
                panic!("not sure what this is")
            }
        };

        Ok(converted_value_ref)
    }

    fn evaluate_mut_or_immutable_expression(
        &mut self,
        expr: &MutOrImmutableExpression,
    ) -> Result<VariableValue, ExecuteError> {
        let var_value = match &expr.expression_or_location {
            ArgumentExpressionOrLocation::Location(loc) => {
                VariableValue::Reference(self.evaluate_location(loc)?)
            }
            ArgumentExpressionOrLocation::Expression(expr) => {
                VariableValue::Value(self.evaluate_expression(expr)?)
            }
        };
        Ok(var_value)
    }

    fn evaluate_argument(
        &mut self,
        expr: &ArgumentExpressionOrLocation,
    ) -> Result<VariableValue, ExecuteError> {
        let var_value = match expr {
            ArgumentExpressionOrLocation::Location(mutable_location) => {
                VariableValue::Reference(self.evaluate_location(mutable_location)?)
            }
            ArgumentExpressionOrLocation::Expression(expr) => {
                let value = self.evaluate_expression(expr)?;
                VariableValue::Value(value)
            }
        };

        Ok(var_value)
    }

    fn evaluate_args(
        &mut self,
        args: &[ArgumentExpressionOrLocation],
    ) -> Result<Vec<VariableValue>, ExecuteError> {
        let mut evaluated = Vec::with_capacity(args.len());

        for argument_expression in args {
            let mem_value = self.evaluate_argument(argument_expression)?;
            evaluated.push(mem_value);
        }

        Ok(evaluated)
    }

    fn evaluate_expressions(&mut self, exprs: &[Expression]) -> Result<Vec<Value>, ExecuteError> {
        let mut values = vec![];
        for expr in exprs {
            let value = self.evaluate_expression(expr)?;
            values.push(value);
        }

        Ok(values)
    }

    fn evaluate_while_loop(
        &mut self,
        condition: &BooleanExpression,
        body: &Expression,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;
        while self
            .evaluate_expression(&condition.expression)?
            .is_truthy()
            .unwrap()
        // TODO: improve error handling
        {
            match self.evaluate_expression_with_signal(body) {
                Err(e) => return Err(e),
                Ok(signal) => match signal {
                    ValueWithSignal::Value(v) => result = v,
                    ValueWithSignal::Break => {
                        break;
                    }
                    ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                    ValueWithSignal::Continue => {}
                },
            }
        }

        Ok(ValueWithSignal::Value(result))
    }

    fn evaluate_block(
        &mut self,
        expressions: &Vec<Expression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        self.push_block_scope();
        for expression in expressions {
            match self.evaluate_expression_with_signal(&expression)? {
                ValueWithSignal::Value(v) => result = v,
                ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                ValueWithSignal::Break => return Ok(ValueWithSignal::Break),
                ValueWithSignal::Continue => return Ok(ValueWithSignal::Continue),
            }
        }
        self.pop_block_scope();
        Ok(ValueWithSignal::Value(result))
    }

    fn evaluate_for_loop_mutable(
        &mut self,
        pattern: &ForPattern,
        iterator_expr: &Iterable,
        body: &Box<Expression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        let iterator_value_mem =
            self.evaluate_mut_or_immutable_expression(&iterator_expr.resolved_expression)?;
        let iterator_value = match iterator_value_mem {
            VariableValue::Value(_) => {
                return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, &body.node));
            }
            VariableValue::Reference(value_ref) => value_ref,
        };

        match pattern {
            ForPattern::Single(var_ref) => {
                self.push_block_scope();

                for value in ValueReference(iterator_value).into_iter_mut().unwrap() {
                    // TODO: Improve error handling
                    self.current_block_scopes.initialize_var_mut(var_ref, value);

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }

            ForPattern::Pair(first_ref, second_ref) => {
                self.push_block_scope();

                for (key, value_reference) in ValueReference(iterator_value)
                    .into_iter_mut_pairs()
                    .unwrap()
                {
                    // TODO: error handling
                    // Set both variables
                    self.current_block_scopes.initialize_var(
                        first_ref.scope_index,
                        first_ref.variable_index,
                        key,
                        false,
                    );
                    self.current_block_scopes
                        .initialize_var_mut(second_ref, value_reference.0);

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }
        }

        Ok(ValueWithSignal::Value(result))
    }

    fn evaluate_for_loop(
        &mut self,
        pattern: &ForPattern,
        iterator_expr: &Iterable,
        body: &Box<Expression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        let iterator_value =
            self.evaluate_mut_or_immutable_expression(&iterator_expr.resolved_expression)?;

        match pattern {
            ForPattern::Single(var_ref) => {
                self.push_block_scope();

                for value in iterator_value.into_iter().unwrap() {
                    // TODO: Error handling
                    self.current_block_scopes.initialize_var(
                        var_ref.scope_index,
                        var_ref.variable_index,
                        value,
                        var_ref.is_mutable(),
                    );

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => {
                            self.pop_block_scope();
                            return Ok(ValueWithSignal::Return(v));
                        }
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }

            ForPattern::Pair(first_ref, second_ref) => {
                self.push_block_scope();

                // iterator_expr.is_mutable() should select reference

                for (key, value) in iterator_value.into_iter_pairs().unwrap() {
                    // TODO: Error handling
                    // Set both variables
                    self.current_block_scopes.initialize_var(
                        first_ref.scope_index,
                        first_ref.variable_index,
                        key,
                        false,
                    );
                    self.current_block_scopes.initialize_var(
                        second_ref.scope_index,
                        second_ref.variable_index,
                        value,
                        false,
                    );

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }
        }

        Ok(ValueWithSignal::Value(result))
    }

    #[allow(unused)]
    fn debug_expr(&self, expr: &Expression) {
        if let Some(debug_source_map) = self.debug_source_map {
            let source_line = debug_source_map.get_text(&expr.node);
            eprintln!("{:?}:\n  {}", expr.kind, source_line);
            //io::stderr().flush().expect("Failed to flush stdout");
        }
    }

    #[inline]
    #[allow(clippy::too_many_lines)]
    fn evaluate_expression_with_signal(
        &mut self,
        expr: &Expression,
    ) -> Result<ValueWithSignal, ExecuteError> {
        match &expr.kind {
            ExpressionKind::Break => Ok(ValueWithSignal::Break),
            ExpressionKind::Continue => Ok(ValueWithSignal::Continue),

            ExpressionKind::Return(maybe_expr) => {
                let value = match maybe_expr {
                    None => Value::Unit,
                    Some(expr) => self.evaluate_expression(expr)?,
                };
                Ok(ValueWithSignal::Return(value))
            }

            ExpressionKind::WhileLoop(condition, body) => self.evaluate_while_loop(condition, body),

            ExpressionKind::ForLoop(pattern, iterator_expr, body) => {
                if pattern.is_mutable() {
                    self.evaluate_for_loop_mutable(pattern, iterator_expr, body)
                } else {
                    self.evaluate_for_loop(pattern, iterator_expr, body)
                }
            }

            ExpressionKind::If(condition, consequences, optional_alternative) => {
                let cond_value = self.evaluate_expression(&condition.expression)?;
                if cond_value.is_truthy()? {
                    self.evaluate_expression_with_signal(consequences)
                } else if let Some(alternative) = optional_alternative {
                    self.evaluate_expression_with_signal(alternative)
                } else {
                    Ok(ValueWithSignal::Value(Value::Unit))
                }
            }

            ExpressionKind::Block(expressions) => self.evaluate_block(expressions),

            ExpressionKind::When(bindings, true_block, maybe_else_block) => {
                let mut all_are_some = true;
                let mut all_expressions = Vec::new();
                for binding in bindings {
                    let source = self.evaluate_mut_or_immutable_expression(&binding.expr)?;
                    match source.to_value() {
                        Value::Option(boxed_val) => {
                            match boxed_val {
                                Some(found_val) => {
                                    let variable_value = match source {
                                        VariableValue::Value(_) => {
                                            // Source was not created mut, so copy value
                                            VariableValue::Value(found_val.borrow().clone())
                                        }
                                        VariableValue::Reference(_var_ref) => {
                                            // It was `mut`
                                            VariableValue::Reference(found_val)
                                        }
                                    };
                                    all_expressions.push(variable_value);
                                }
                                _ => {
                                    all_are_some = false;
                                    //warn!(?source, "Not ALL ARE SOME!");
                                    break;
                                }
                            }
                        }
                        _ => {
                            return Err(self
                                .create_err(ExecuteErrorKind::ExpectedOptional, &true_block.node));
                        }
                    }
                }

                if all_are_some {
                    self.push_block_scope();

                    for (binding, value) in bindings.iter().zip(all_expressions) {
                        self.current_block_scopes
                            .initialize_var_mem(&binding.variable, value)?;
                    }

                    let result = self.evaluate_expression_with_signal(true_block)?;
                    self.pop_block_scope();

                    Ok(result)
                } else if let Some(else_block) = maybe_else_block {
                    self.evaluate_expression_with_signal(else_block)
                } else {
                    Ok(ValueWithSignal::Value(Value::Unit))
                }
            }

            _ => Ok(ValueWithSignal::Value(self.evaluate_expression(expr)?)),
        }
    }

    // ---------------
    #[allow(clippy::too_many_lines)]
    #[inline]
    fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value, ExecuteError> {
        self.depth += 1;
        let value = match &expr.kind {
            // Illegal in this context
            ExpressionKind::Continue => {
                return Err(self.create_err(ExecuteErrorKind::ContinueNotAllowedHere, &expr.node));
            }
            ExpressionKind::Break => {
                return Err(self.create_err(ExecuteErrorKind::BreakNotAllowedHere, &expr.node));
            }
            ExpressionKind::Return(_maybe_expr) => {
                return Err(self.create_err(ExecuteErrorKind::ReturnNotAllowedHere, &expr.node));
            }

            ExpressionKind::WhileLoop(_condition, _body) => {
                panic!("should have been handled earlier")
            }

            ExpressionKind::ForLoop(_pattern, _iterator_expr, _body) => {
                panic!("should have been handled earlier")
            }

            // Constructing
            ExpressionKind::Literal(lit) => self.evaluate_literal(&expr.node, lit)?,

            ExpressionKind::Array(array_instantiation) => {
                let mut values = Vec::new();
                for element in &array_instantiation.expressions {
                    values.push(self.evaluate_expression(element)?);
                }

                Value::Array(
                    array_instantiation.array_type_ref.clone(),
                    convert_vec_to_rc_refcell(values),
                )
            }

            ExpressionKind::StructInstantiation(struct_instantiation) => {
                // Evaluate all field expressions and validate types
                let mut field_values =
                    Vec::with_capacity(struct_instantiation.source_order_expressions.len());
                field_values.resize_with(
                    struct_instantiation.source_order_expressions.len(),
                    Default::default,
                );

                // They are evaluated in source order, but an array_index is provided for the definition order
                for (array_index, field_expr) in &struct_instantiation.source_order_expressions {
                    let value = self.evaluate_expression(field_expr)?;
                    field_values[*array_index] = value;
                }

                Value::Struct(
                    struct_instantiation.struct_type_ref.clone(),
                    convert_vec_to_rc_refcell(field_values),
                )
            }

            ExpressionKind::Range(start, end, range_mode) => {
                let start_val = self.evaluate_expression(start)?;
                let end_val = self.evaluate_expression(end)?;
                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(e)) => {
                        Value::Range(Box::new(s), Box::new(e), range_mode.clone())
                    }
                    _ => Err(self.create_err(ExecuteErrorKind::RangeItemMustBeInt, &expr.node))?,
                }
            }

            // ==================== ASSIGNMENT ====================
            ExpressionKind::VariableDefinition(target_var, source_expr) => {
                let source_value_or_reference =
                    self.evaluate_mut_or_immutable_expression(source_expr)?;

                self.current_block_scopes
                    .initialize_var_mem(target_var, source_value_or_reference.clone())?;

                source_value_or_reference.to_value().clone()
            }

            ExpressionKind::VariableReassignment(variable_ref, source_expr) => {
                let new_value = self.evaluate_mut_or_immutable_expression(source_expr)?;

                let value_ref = self
                    .current_block_scopes
                    .lookup_variable_mut_ref(variable_ref)?;

                let mut was_assigned = false;
                if let Value::Option(inner_value) = &*value_ref.borrow() {
                    if let Some(inner) = inner_value {
                        *inner.borrow_mut() = new_value.to_value();
                        was_assigned = true;
                    }
                }

                if !was_assigned {
                    self.current_block_scopes
                        .overwrite_existing_var_mem(variable_ref, new_value.clone())?;
                }

                Value::Unit
            }

            ExpressionKind::IntrinsicCallMut(intrinsic, location, arguments) => {
                self.evaluate_intrinsic_mut(&expr.node, intrinsic, location, arguments)?
            }

            ExpressionKind::MapAssignment(map, index, value) => {
                let map_val = self.evaluate_location(&map.0)?;
                let index_val = self.evaluate_expression(index)?;
                let new_val = self.evaluate_expression(value)?;

                match &mut *map_val.borrow_mut() {
                    Value::Map(_type_id, elements) => {
                        elements
                            .insert(index_val, Rc::new(RefCell::new(new_val)))
                            .map_err(|_| {
                                self.create_err(ExecuteErrorKind::MapKeyAlreadyExists, &expr.node)
                            })?;
                    }
                    _ => {
                        Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, &expr.node))?;
                    }
                }

                Value::Unit
            }

            // ------------- LOOKUP ---------------------
            ExpressionKind::ConstantAccess(constant) => {
                self.constants.lookup_constant_value(constant.id).clone()
            }

            ExpressionKind::AssignmentSlice(_mut_location, _source) => todo!(),

            ExpressionKind::Assignment(mut_location_expr, source_expr) => {
                let value_ref = self.evaluate_location(&mut_location_expr.0)?;
                let source_value = self.evaluate_expression(source_expr)?;

                *value_ref.borrow_mut() = source_value;

                Value::Unit
            }

            ExpressionKind::CompoundAssignment(mut_location_expr, op, source_expr) => {
                let value_ref = self.evaluate_location(&mut_location_expr.0)?;
                let source_value = self.evaluate_expression(source_expr)?;

                self.apply_compound_operator(
                    &expr.node,
                    &mut value_ref.borrow_mut(),
                    op,
                    &source_value,
                )?;

                Value::Unit
            }

            // Operators
            ExpressionKind::BinaryOp(binary_operator) => {
                let left_val = self.evaluate_expression(&binary_operator.left)?;
                let right_val = self.evaluate_expression(&binary_operator.right)?;
                self.evaluate_binary_op(&expr.node, left_val, &binary_operator.kind, right_val)?
            }

            ExpressionKind::UnaryOp(unary_operator) => {
                let left_val = self.evaluate_expression(&unary_operator.left)?;
                self.evaluate_unary_op(&expr.node, &unary_operator.kind, left_val)?
            }

            // Calling
            ExpressionKind::FunctionCall(_signature, expr, arguments) => {
                self.evaluate_function_call(expr, arguments)?
            }

            ExpressionKind::MemberCall(resolved_member_call) => {
                let parameters = match &*resolved_member_call.function {
                    Function::Internal(function_data) => &function_data.signature.parameters,
                    Function::External(external_data) => &external_data.signature.parameters,
                };

                let mut member_call_arguments = Vec::new();
                member_call_arguments.extend(self.evaluate_args(&resolved_member_call.arguments)?);

                // Check total number of parameters (including self)
                if member_call_arguments.len() != parameters.len() {
                    return Err(self.create_err(
                        ExecuteErrorKind::WrongNumberOfArguments(
                            parameters.len(),
                            member_call_arguments.len(),
                        ),
                        &expr.node,
                    ));
                }

                match &*resolved_member_call.function {
                    Function::Internal(internal_function) => {
                        self.push_function_scope();
                        self.bind_parameters(
                            &expr.node,
                            &internal_function.signature.parameters,
                            &member_call_arguments,
                        )?;
                        let result = self.evaluate_expression(&internal_function.body)?;
                        self.pop_function_scope();

                        result
                    }
                    Function::External(external_func) => {
                        let mut func = self
                            .externals
                            .external_functions_by_id
                            .get(&external_func.id)
                            .expect("member call: external function missing")
                            .borrow_mut();
                        (func.func)(&member_call_arguments, self.context)?
                    }
                }
            }

            ExpressionKind::Block(statements) => {
                self.evaluate_block(statements)?.try_into().unwrap() // TODO: Error handling
            }

            ExpressionKind::InterpolatedString(parts) => {
                let mut result = String::new();

                for part in parts {
                    match part {
                        StringPart::Literal(_resolved_node, text) => {
                            result.push_str(text);
                        }
                        StringPart::Interpolation(expr, format_spec) => {
                            let value = self.evaluate_expression(expr)?;
                            let formatted = match format_spec {
                                Some(spec) => format_value(&value, &spec.kind).unwrap(), // TODO: Error handling
                                None => value.convert_to_string_if_needed(),
                            };
                            result.push_str(&formatted);
                        }
                    }
                }

                Value::String(result)
            }

            ExpressionKind::Match(resolved_match) => self.eval_match(resolved_match)?,
            ExpressionKind::Guard(guards) => self.eval_guard(&expr.node, guards)?,

            ExpressionKind::InternalFunctionAccess(fetch_function) => {
                Value::InternalFunction(fetch_function.clone())
            }

            ExpressionKind::ExternalFunctionAccess(fetch_function) => {
                self.externals
                    .external_functions_by_id
                    .get(&fetch_function.id)
                    .expect("should have external function ref");
                Value::ExternalFunction(fetch_function.clone())
            }

            //ExpressionKind::MutMemberCall(_, _) => todo!(),
            ExpressionKind::Tuple(_) => todo!(),
            ExpressionKind::Option(inner) => match inner {
                None => Value::Option(None),
                Some(expression) => {
                    let v = self.evaluate_expression(expression)?;
                    match v {
                        Value::Option(_) => {
                            panic!("unnecessary wrap!, should be investigated");
                        }
                        _ => Value::Option(Some(Rc::new(RefCell::new(v)))),
                    }
                }
            },

            // --------------- SPECIAL FUNCTIONS
            ExpressionKind::SparseNew(sparse_id_rust_type_ref, resolved_value_item_type) => {
                let sparse_value_map = SparseValueMap::new(
                    sparse_id_rust_type_ref.clone(),
                    resolved_value_item_type.clone(),
                );
                to_rust_value(sparse_id_rust_type_ref.clone(), sparse_value_map)
            }

            ExpressionKind::CoerceOptionToBool(expression) => {
                let value = self.evaluate_expression(expression)?;
                match value {
                    Value::Option(inner) => Value::Bool(inner.is_some()),
                    _ => {
                        return Err(
                            self.create_err(ExecuteErrorKind::CoerceOptionToBoolFailed, &expr.node)
                        );
                    }
                }
            }

            ExpressionKind::If(condition, consequences, optional_alternative) => {
                let cond_value = self.evaluate_expression(&condition.expression)?;
                if cond_value.is_truthy().unwrap() {
                    // TODO: ERROR HANDLING
                    self.evaluate_expression(consequences)?
                } else if let Some(alternative) = optional_alternative {
                    self.evaluate_expression(alternative)?
                } else {
                    Value::Unit
                }
            }

            ExpressionKind::When(bindings, true_block, maybe_else_block) => {
                let mut all_are_some = true;
                let mut all_expressions = Vec::new();
                for binding in bindings {
                    let source = self.evaluate_mut_or_immutable_expression(&binding.expr)?;
                    match source.to_value() {
                        Value::Option(boxed_val) => match boxed_val {
                            Some(found_val) => {
                                all_expressions.push(found_val.borrow().clone());
                            }
                            _ => {
                                all_are_some = false;
                                break;
                            }
                        },
                        _ => {
                            return Err(self
                                .create_err(ExecuteErrorKind::ExpectedOptional, &true_block.node));
                        }
                    }
                }

                if all_are_some {
                    self.push_block_scope();

                    for (binding, value) in bindings.iter().zip(all_expressions) {
                        info!(var=?binding.variable, "binding as mutable");
                        self.current_block_scopes.initialize_var(
                            binding.variable.scope_index,
                            binding.variable.variable_index,
                            value,
                            binding.variable.is_mutable(),
                        );
                    }

                    let result = self.evaluate_expression(true_block)?;
                    self.pop_block_scope();

                    result
                } else if let Some(else_block) = maybe_else_block {
                    self.evaluate_expression(else_block)?
                } else {
                    Value::Unit
                }
            }

            ExpressionKind::TupleDestructuring(variable_refs, _, expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Tuple(_tuple_ref, values) = value {
                    if variable_refs.len() > values.len() {
                        return Err(self.create_err(ExecuteErrorKind::NotAnArray, &expr.node));
                    }
                    for (index, variable_ref) in variable_refs.iter().enumerate() {
                        let value = &values[index].borrow().clone();
                        self.current_block_scopes.initialize_var(
                            variable_ref.scope_index,
                            variable_ref.variable_index,
                            value.clone(),
                            false,
                        );
                    }
                }
                Value::Unit
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                let temp = self.current_block_scopes.lookup_var_value(variable_ref);
                assert_ne!(temp, Value::Unit);
                temp
            }
            ExpressionKind::FieldAccess(expr, index) => {
                let resolved_expr = self.evaluate_expression(expr)?;
                let (_struct_type, values) = resolved_expr
                    .expect_struct()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedStruct, &expr.node))?;

                let x = values[*index].borrow().clone();
                x
            }

            ExpressionKind::ArrayAccess(expr, _array, index_expr) => {
                let resolved_expr = self.evaluate_expression(expr)?;
                let (_array_type, values) = resolved_expr
                    .expect_array()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedArray, &expr.node))?;

                let index = self
                    .evaluate_expression(index_expr)?
                    .expect_int()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedInt, &expr.node))?
                    as usize;

                let x = values[index].borrow().clone();
                x
            }

            ExpressionKind::MapIndexAccess(expr, _map_type_ref, key_expr) => {
                let resolved_expr = self.evaluate_expression(expr)?;
                let (_map_type, seq_map) = resolved_expr
                    .expect_map()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedMap, &expr.node))?;

                let key_val = self.evaluate_expression(key_expr)?;

                let value_val_maybe = seq_map.get(&key_val);
                Value::Option(value_val_maybe.cloned())
            }
            ExpressionKind::StringRangeAccess(_, _) => todo!(),
            ExpressionKind::ArrayRangeAccess(_, _) => todo!(),
            ExpressionKind::PostfixChain(start, parts) => {
                let value_ref = self.eval_chain(&expr.node, start, parts)?;
                let x = value_ref.borrow().clone();
                x
            }
        };

        self.depth -= 1;
        //self.debug_expr(expr);
        //info!(?value, "resulted in value");
        Ok(value)
    }

    fn evaluate_literal(&mut self, node: &Node, lit: &Literal) -> Result<Value, ExecuteError> {
        let v = match lit {
            Literal::IntLiteral(n) => Value::Int(*n),
            Literal::FloatLiteral(f) => Value::Float(*f),
            Literal::StringLiteral(s) => Value::String(s.clone()),
            Literal::BoolLiteral(b) => Value::Bool(*b),

            Literal::EnumVariantLiteral(enum_variant_type, data) => {
                let variant_container_value: Value = match &**enum_variant_type {
                    EnumVariantType::Tuple(tuple_type) => match data {
                        EnumLiteralData::Tuple(tuple_expressions) => {
                            let eval_expressions = self.evaluate_expressions(tuple_expressions)?;
                            let value_refs = values_to_value_refs_owned(eval_expressions);
                            Value::EnumVariantTuple(tuple_type.clone(), value_refs)
                        }
                        _ => panic!("wrong container type"),
                    },

                    EnumVariantType::Struct(struct_type_ref) => match data {
                        EnumLiteralData::Struct(source_order_field_values) => {
                            let mut field_values =
                                Vec::with_capacity(source_order_field_values.len());
                            field_values
                                .resize_with(source_order_field_values.len(), Default::default);
                            for (index, resolved_expression) in source_order_field_values {
                                let value = self.evaluate_expression(resolved_expression)?;
                                field_values[*index] = Rc::new(RefCell::new(value));
                            }
                            Value::EnumVariantStruct(struct_type_ref.clone(), field_values)
                        }
                        _ => panic!("wrong container type"),
                    },

                    EnumVariantType::Nothing(data) => Value::EnumVariantSimple(data.clone()),
                };
                variant_container_value
            }

            Literal::TupleLiteral(tuple_type, resolved_expressions) => {
                let values = self.evaluate_expressions(resolved_expressions)?;
                Value::Tuple(tuple_type.clone(), convert_vec_to_rc_refcell(values))
            }

            Literal::Array(array_type, expressions) => {
                let values = self.evaluate_expressions(expressions)?;
                Value::Array(array_type.clone(), convert_vec_to_rc_refcell(values))
            }
            Literal::Map(map_type_ref, expressions) => {
                let mut items = SeqMap::new();
                for (key, value) in expressions {
                    let key_val = self.evaluate_expression(key)?;
                    let value_val = self.evaluate_expression(value)?;
                    items
                        .insert(key_val, Rc::new(RefCell::new(value_val)))
                        .map_err(|_err| {
                            self.create_err(
                                ExecuteErrorKind::NonUniqueKeysInMapLiteralDetected,
                                &node,
                            )
                        })?;
                }
                Value::Map(map_type_ref.clone(), items)
            }
            Literal::NoneLiteral => Value::Option(None),
        };
        Ok(v)
    }

    #[allow(clippy::too_many_lines)]
    fn eval_intrinsic_postfix(
        &mut self,
        node: &Node,
        value_ref: &ValueRef,
        //        resolved_postfix: &Postfix,
        intrinsic_function: &IntrinsicFunction,
        arguments: &[Expression],
    ) -> Result<Value, ExecuteError> {
        //let node = &resolved_postfix.node;
        let val = match &intrinsic_function {
            IntrinsicFunction::VecRemoveIndex => {
                let index_val = self.evaluate_expression(&arguments[0])?;
                let Value::Int(index) = index_val else {
                    return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, node));
                };

                match &mut *value_ref.borrow_mut() {
                    Value::Array(_type_id, vector) => {
                        vector.remove(index as usize);
                    }
                    _ => {
                        Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, node))?;
                    }
                }

                value_ref.borrow().clone()
            }

            IntrinsicFunction::VecClear => {
                match &mut *value_ref.borrow_mut() {
                    Value::Array(_type_id, vector) => {
                        vector.clear();
                    }
                    _ => {
                        Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, node))?;
                    }
                }
                Value::Unit
            }

            IntrinsicFunction::VecPush => {
                match &mut *value_ref.borrow_mut() {
                    Value::Array(_type_id, vector) => {
                        let value_to_add = self.evaluate_expression(&arguments[0])?;
                        vector.push(Rc::new(RefCell::new(value_to_add)));
                    }
                    _ => {
                        Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, node))?;
                    }
                }
                Value::Unit
            }

            IntrinsicFunction::VecLen => match &mut *value_ref.borrow_mut() {
                Value::Array(_type_id, vector) => {
                    let length = vector.len();
                    Value::Int(length as i32)
                }
                _ => Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, node))?,
            },

            IntrinsicFunction::VecPop => match &mut *value_ref.borrow_mut() {
                Value::Array(_type_id, vector) => {
                    let maybe_val = vector.pop();
                    Value::Option(maybe_val)
                }
                _ => Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, node))?,
            },

            IntrinsicFunction::MapHas => {
                let index_val = self.evaluate_expression(&arguments[0])?;

                match value_ref.borrow().clone() {
                    Value::Map(_type_id, ref seq_map) => {
                        let has_key = seq_map.contains_key(&index_val);
                        Value::Bool(has_key)
                    }
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::NotAMap, node));
                    }
                }
            }

            IntrinsicFunction::MapRemove => {
                let index_val = self.evaluate_expression(&arguments[0])?;

                let result = {
                    let mut borrowed = value_ref.borrow_mut();
                    match &mut *borrowed {
                        Value::Map(_type_id, seq_map) => {
                            let x = seq_map.remove(&index_val);
                            x.map_or_else(
                                || Value::Option(None),
                                |v| Value::Option(Some(v.clone())),
                            )
                        }
                        _ => {
                            return Err(self.create_err(ExecuteErrorKind::NotAMap, node));
                        }
                    }
                };
                result
            }

            IntrinsicFunction::SparseAdd => {
                let borrowed = value_ref.borrow();

                let sparse_value_map = borrowed.downcast_rust::<SparseValueMap>();
                match sparse_value_map {
                    Some(found) => {
                        let resolved_value = self.evaluate_expression(&arguments[0])?;
                        let id_value = found.borrow_mut().add(resolved_value);

                        id_value
                    }
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::NotSparseValue, node));
                    }
                }
            }

            IntrinsicFunction::SparseRemove => {
                let borrowed = value_ref.borrow();

                let sparse_value_map = borrowed.downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let id_value = self.evaluate_expression(&arguments[0])?;
                    match id_value.downcast_rust::<SparseValueId>() {
                        Some(found_id) => {
                            found.borrow_mut().remove(&found_id.borrow());
                        }
                        _ => {
                            return Err(self.create_err(ExecuteErrorKind::NotSparseValue, node));
                        }
                    }
                }

                Value::Unit
            }
            IntrinsicFunction::SparseSubscript => {
                let sparse_value_map = value_ref.borrow_mut().downcast_rust::<SparseValueMap>();
                match sparse_value_map {
                    Some(found) => {
                        let id_value = self.evaluate_expression(&arguments[0])?; // id
                        match id_value.downcast_rust::<SparseValueId>() {
                            Some(found_id) => match found.borrow_mut().get(&found_id.borrow()) {
                                Some(found_value) => Value::Option(Some(found_value.clone())),
                                _ => Value::Option(None),
                            },
                            _ => {
                                return Err(self.create_err(ExecuteErrorKind::NotSparseId, node));
                            }
                        }
                    }
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::NotSparseId, node));
                    }
                }
            }

            IntrinsicFunction::FloatRound => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Int(f.round().into()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },
            IntrinsicFunction::FloatFloor => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Int(f.floor().into()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatSign => match value_ref.borrow().clone() {
                Value::Float(f) => {
                    let signum = if f.inner() < 0 {
                        -1
                    } else if f.inner() > 0 {
                        1
                    } else {
                        0
                    };
                    Value::Float(Fp::from(signum as i16))
                }
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },
            IntrinsicFunction::FloatAbs => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.abs()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatCos => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.cos()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatAcos => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.acos()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatSin => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.sin()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatAsin => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.asin()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatSqrt => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.sqrt()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatMin => {
                let min_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), min_value) {
                    (Value::Float(f), Value::Float(min_f)) => Value::Float(f.min(min_f)),
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatMax => {
                let max_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), max_value) {
                    (Value::Float(f), Value::Float(max_f)) => Value::Float(f.max(max_f)),
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatAtan2 => {
                let x_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), x_value) {
                    (Value::Float(_y_f), Value::Float(_x_f)) => {
                        Value::Float(Fp::from(-9999)) //y_f.atan2(x_f)) // TODO: Implement atan2
                    }
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatClamp => {
                let min_value = self.evaluate_expression(&arguments[0])?;
                let max_value = self.evaluate_expression(&arguments[1])?;
                match (value_ref.borrow().clone(), min_value, max_value) {
                    (Value::Float(f), Value::Float(min_f), Value::Float(max_f)) => {
                        Value::Float(f.clamp(min_f, max_f))
                    }
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatRnd => match value_ref.borrow().clone() {
                Value::Float(f) => {
                    let new_raw = squirrel_prng::squirrel_noise5(f.inner() as u32, 0);
                    Value::Int(new_raw as i32)
                }
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::IntAbs => match value_ref.borrow().clone() {
                Value::Int(i) => Value::Int(i.abs()),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::IntClamp => {
                let min_value = self.evaluate_expression(&arguments[0])?;
                let max_value = self.evaluate_expression(&arguments[1])?;
                match (value_ref.borrow().clone(), min_value, max_value) {
                    (Value::Int(i), Value::Int(min_i), Value::Int(max_i)) => {
                        Value::Int(i.clamp(min_i, max_i))
                    }
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                    }
                }
            }

            IntrinsicFunction::IntMin => {
                let max_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), max_value) {
                    (Value::Int(i), Value::Int(min_i)) => Value::Int(i.min(min_i)),
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                    }
                }
            }

            IntrinsicFunction::IntMax => {
                let max_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), max_value) {
                    (Value::Int(i), Value::Int(max_i)) => Value::Int(i.max(max_i)),
                    _ => {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                    }
                }
            }

            IntrinsicFunction::IntRnd => match value_ref.borrow().clone() {
                Value::Int(i) => Value::Int(squirrel_prng::squirrel_noise5(i as u32, 0) as i32),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                }
            },

            IntrinsicFunction::IntToFloat => match value_ref.borrow().clone() {
                Value::Int(i) => Value::Float(Fp::from(i as i16)),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                }
            },

            IntrinsicFunction::StringLen => match value_ref.borrow().clone() {
                Value::String(s) => Value::Int(s.len().try_into().expect("string len overflow")),
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedString, node));
                }
            },

            IntrinsicFunction::Float2Magnitude => match value_ref.borrow().clone() {
                Value::Tuple(_tuple_ref, values) => {
                    if values.len() != 2 {
                        return Err(self.create_err(
                            ExecuteErrorKind::WrongNumberOfArguments(2, values.len()),
                            &node,
                        ));
                    }
                    match (
                        values[0].as_ref().borrow().clone(),
                        values[1].as_ref().borrow().clone(),
                    ) {
                        (Value::Float(a), Value::Float(b)) => {
                            let a_raw: i64 = a.inner() as i64;
                            let b_raw: i64 = b.inner() as i64;

                            let i64_magnitude = i64_sqrt(a_raw * a_raw + b_raw * b_raw);

                            let new_fp = Fp::from_raw(
                                i32::try_from(i64_magnitude).expect("wrong with i64_sqrt"),
                            );
                            Value::Float(new_fp)
                        }
                        _ => {
                            return Err(
                                self.create_err(ExecuteErrorKind::ExpectedTwoFloatTuple, node)
                            );
                        }
                    }
                }
                _ => {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedTwoFloatTuple, node));
                }
            },

            _ => todo!("{intrinsic_function:?} not implemented"),
        };

        Ok(val)
    }

    #[allow(clippy::too_many_lines)]
    fn eval_chain(
        &mut self,
        node: &Node,
        start: &Expression,
        parts: &[Postfix],
    ) -> Result<ValueRef, ExecuteError> {
        let (mut val_ref, mut is_mutable) = match &start.kind {
            ExpressionKind::VariableAccess(start_var) => {
                let start_variable_value = self.current_block_scopes.get_var(&start_var);

                match start_variable_value {
                    VariableValue::Value(value) => {
                        assert_ne!(*value, Value::Unit);
                        (Rc::new(RefCell::new(value.clone())), false)
                    }
                    VariableValue::Reference(value_ref) => {
                        assert_ne!(value_ref.borrow().clone(), Value::Unit);
                        (value_ref.clone(), true)
                    }
                }
            }
            _ => (
                Rc::new(RefCell::new(self.evaluate_expression(start)?)),
                false,
            ),
        };

        let mut is_uncertain = false;
        let mut is_undefined = false;

        for part in parts {
            if let PostfixKind::NoneCoalesce(default_expression) = &part.kind {
                val_ref = {
                    let borrowed = val_ref.borrow();

                    match borrowed.clone() {
                        Value::Option(found_option) => match found_option {
                            Some(some_value) => some_value,
                            _ => {
                                let default_value = self.evaluate_expression(default_expression)?;
                                Rc::new(RefCell::new(default_value))
                            }
                        },
                        _ => {
                            return Err(
                                self.create_err(ExecuteErrorKind::ExpectedOptional, &part.node)
                            );
                        }
                    }
                };

                is_mutable = false;
                is_uncertain = false;
                is_undefined = false;
            } else if is_undefined {
                continue;
            }
            match &part.kind {
                PostfixKind::NoneCoalesce(_default_expression) => {
                    // Handled earlier
                }
                PostfixKind::StructField(expected_struct_type, index) => {
                    let (encountered_struct_type, fields) = {
                        let brw = val_ref.borrow();
                        let (struct_ref, fields_ref) = brw.expect_struct().map_err(|_| {
                            self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                        })?;
                        (struct_ref.clone(), fields_ref.clone())
                    };

                    assert!(same_struct_ref(
                        &encountered_struct_type,
                        expected_struct_type
                    ));
                    val_ref = fields[*index].clone();
                }
                PostfixKind::ArrayIndex(expected_array_type, index_expr) => {
                    let (encountered_array_type, fields) = {
                        let brw = val_ref.borrow();
                        let (array_ref, fields_ref) = brw.expect_array().map_err(|_| {
                            self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                        })?;
                        (array_ref.clone(), fields_ref.clone())
                    };
                    assert!(same_array_ref(&encountered_array_type, expected_array_type));

                    let index =
                        self.evaluate_expression(index_expr)?
                            .expect_int()
                            .map_err(|_| {
                                self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                            })? as usize;
                    if index >= fields.len() {
                        return Err(self.create_err(ExecuteErrorKind::IndexOutOfBounds, &part.node));
                    }
                    val_ref = fields[index].clone();
                }
                PostfixKind::MapIndex(_expected_map_type_ref, key_expr) => {
                    let (_encountered_map_type, seq_map) = {
                        let brw = val_ref.borrow();
                        let (array_ref, seq_map) = brw.expect_map().map_err(|_| {
                            self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                        })?;
                        (array_ref.clone(), seq_map.clone())
                    };
                    let key_val = self.evaluate_expression(key_expr)?;

                    val_ref = Rc::new(RefCell::new(Value::Option(seq_map.get(&key_val).cloned())));
                }
                PostfixKind::ExternalTypeIndexRef(_rust_type_ref, map_expr) => {
                    let key_expr_value = self.evaluate_expression(map_expr)?;
                    val_ref = {
                        match key_expr_value.downcast_rust::<SparseValueId>() {
                            Some(found_sparse_id) => {
                                match val_ref.borrow_mut().downcast_rust::<SparseValueMap>() {
                                    Some(sparse_value_map) => wrap_in_option(
                                        sparse_value_map
                                            .borrow_mut()
                                            .get(&found_sparse_id.borrow()),
                                    ),
                                    _ => {
                                        panic!("internal error");
                                    }
                                }
                            }
                            _ => {
                                panic!("todo");
                            }
                        }
                    };
                }
                PostfixKind::MemberCall(function_ref, arguments) => {
                    let val =
                        self.eval_member_call(node, &val_ref, is_mutable, function_ref, arguments)?;

                    val_ref = Rc::new(RefCell::new(val));
                    is_mutable = false;
                }
                PostfixKind::FunctionCall(arguments) => {
                    let val = self.eval_function_call(node, &val_ref, arguments)?;

                    val_ref = Rc::new(RefCell::new(val));
                    is_mutable = false;
                }
                PostfixKind::OptionUnwrap => {
                    val_ref = {
                        let borrowed = val_ref.borrow();

                        match borrowed.clone() {
                            Value::Option(found_option) => match found_option {
                                Some(some_value) => some_value,
                                _ => {
                                    is_undefined = true;

                                    Rc::new(RefCell::new(Value::Option(None)))
                                }
                            },
                            _ => {
                                return Err(
                                    self.create_err(ExecuteErrorKind::ExpectedOptional, &part.node)
                                );
                            }
                        }
                    };

                    is_mutable = false;
                    is_uncertain = true;
                }
                PostfixKind::IntrinsicCall(intrinsic_fn, arguments) => {
                    val_ref = Rc::new(RefCell::new(self.eval_intrinsic_postfix(
                        &part.node,
                        &val_ref,
                        intrinsic_fn,
                        arguments,
                    )?));
                    is_mutable = false;
                }
                PostfixKind::IntrinsicCallEx(_intrinsic_fn, _arguments) => {
                    //val_ref = Rc::new(RefCell::new(self.eval_intrinsic_postfix_ex(&val_ref, part, intrinsic_fn, arguments)?));
                    is_mutable = false;
                }
                _ => {}
            }
        }

        if is_uncertain {
            let binding = val_ref.borrow().clone();
            match binding {
                Value::Option(_) => {}
                _ => {
                    val_ref = Rc::new(RefCell::new(Value::Option(Some(val_ref))));
                }
            }
        }

        Ok(val_ref)
    }

    fn eval_function_call(
        &mut self,
        node: &Node,
        function_val: &ValueRef,
        arguments: &[ArgumentExpressionOrLocation],
    ) -> Result<Value, ExecuteError> {
        let resolved_fn = match function_val.borrow().clone() {
            Value::InternalFunction(x) => Function::Internal(x.clone()),
            Value::ExternalFunction(external_fn) => Function::External(external_fn.clone()),
            _ => panic!("no function to call"),
        };

        let parameters = &resolved_fn.signature().parameters;
        // Check total number of parameters (including self)
        assert_eq!(
            arguments.len(),
            parameters.len(),
            "wrong number of arguments"
        );

        let resolved_arguments = self.evaluate_args(&arguments)?;

        let result_val = match &resolved_fn {
            Function::Internal(internal_function) => {
                self.push_function_scope();

                self.bind_parameters(node, &parameters, &resolved_arguments)?;
                let result = self.evaluate_expression(&internal_function.body)?;
                self.pop_function_scope();

                result
            }
            Function::External(external_func) => {
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external_func.id)
                    .expect("member call: external function missing")
                    .borrow_mut();
                (func.func)(&resolved_arguments, self.context)?
            }
        };

        Ok(result_val)
    }

    #[inline]
    fn eval_member_call(
        &mut self,
        node: &Node,
        self_value_ref: &ValueRef,
        is_mutable: bool,
        function_ref: &FunctionRef,
        arguments: &[ArgumentExpressionOrLocation],
    ) -> Result<Value, ExecuteError> {
        let parameters = &function_ref.signature().parameters;

        let self_var_value = if parameters[0].is_mutable {
            if !is_mutable {
                return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, &node));
            }
            VariableValue::Reference(self_value_ref.clone())
        } else {
            VariableValue::Value(self_value_ref.borrow().clone())
        };

        let mut member_call_arguments = Vec::new();
        member_call_arguments.push(self_var_value); // Add self as first argument
        member_call_arguments.extend(self.evaluate_args(&arguments)?);

        // Check total number of parameters (including self)
        if member_call_arguments.len() != parameters.len() {
            panic!("wrong number of arguments")
        }

        let result_val = match &**function_ref {
            Function::Internal(internal_function) => {
                self.push_function_scope();
                self.bind_parameters(node, &parameters, &member_call_arguments)?;
                let result = self.evaluate_expression(&internal_function.body)?;
                self.pop_function_scope();

                result
            }
            Function::External(external_func) => {
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external_func.id)
                    .expect("member call: external function missing")
                    .borrow_mut();
                (func.func)(&member_call_arguments, self.context)?
            }
        };

        Ok(result_val)
    }

    fn eval_guard(&mut self, node: &Node, guards: &[Guard]) -> Result<Value, ExecuteError> {
        for guard in guards {
            let should_evaluate = if let Some(found_clause) = &guard.condition {
                self.evaluate_expression(&found_clause.expression)?
                    .is_truthy()?
            } else {
                true
            };

            if should_evaluate {
                return self.evaluate_expression(&guard.result);
            }
        }

        Err(self.create_err(ExecuteErrorKind::MustHaveGuardArmThatMatches, &node))
    }

    #[inline(always)]
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn eval_match(&mut self, resolved_match: &Match) -> Result<Value, ExecuteError> {
        let actual_value = self.evaluate_mut_or_immutable_expression(&resolved_match.expression)?;
        let value_ref = actual_value.to_value_ref();

        for arm in &resolved_match.arms {
            match &arm.pattern {
                Pattern::Wildcard(_node) => return self.evaluate_expression(&arm.expression),
                Pattern::Normal(normal_pattern, maybe_guard) => {
                    if let Some(found_guard) = maybe_guard {
                        if !self
                            .evaluate_expression(&found_guard.expression)?
                            .is_truthy()?
                        // TODO: ERROR HANDLING
                        {
                            continue;
                        }
                    }

                    let immutable_value = actual_value.to_value();

                    match &normal_pattern {
                        NormalPattern::PatternList(elements) => {
                            return Ok(self.eval_normal_pattern_list(
                                elements,
                                &arm.expression,
                                value_ref.clone(),
                            )?);
                        }
                        NormalPattern::EnumPattern(enum_variant_ref, pattern_elements) => {
                            let maybe_found_match = self.eval_normal_pattern_enum(
                                pattern_elements.as_ref(),
                                &arm.expression,
                                enum_variant_ref,
                                value_ref.clone(),
                            )?;

                            if let Some(found_match) = maybe_found_match {
                                return Ok(found_match);
                            }
                        }

                        NormalPattern::Literal(lit) => match (lit, &immutable_value) {
                            (Literal::IntLiteral(a), Value::Int(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (Literal::FloatLiteral(a), Value::Float(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (Literal::StringLiteral(a), Value::String(b)) if *a == *b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (Literal::BoolLiteral(a), Value::Bool(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (
                                Literal::TupleLiteral(_a_type_ref, a_values),
                                Value::Tuple(_b_type_ref, b_values),
                            ) if self.expressions_equal_to_values(&a_values, &b_values)? => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            _ => {}
                        },
                    }
                }
            }
        }

        panic!("must match one of the match arms!");
    }

    fn eval_normal_pattern_list(
        &mut self,
        elements: &[PatternElement],
        expression_to_evaluate: &Expression,
        value_ref: ValueRef,
    ) -> Result<Value, ExecuteError> {
        // Handle single variable/wildcard patterns that match any value
        if elements.len() == 1 {
            return match &elements[0] {
                PatternElement::Variable(var_ref)
                | PatternElement::VariableWithFieldIndex(var_ref, _) => {
                    self.push_block_scope();
                    self.current_block_scopes
                        .initialize_var_mut(var_ref, value_ref);
                    let result = self.evaluate_expression(expression_to_evaluate);
                    self.pop_block_scope();
                    result
                }
                PatternElement::Wildcard(_) => {
                    // Wildcard matches anything
                    self.evaluate_expression(expression_to_evaluate)
                }
            };
        }

        if let Value::Tuple(_tuple_type_ref, values) = value_ref.borrow_mut().clone() {
            assert_eq!(
                elements.len(),
                values.len(),
                "must use all elements in tuple"
            );
            self.push_block_scope();

            for (element, _inside_value) in elements.iter().zip(values.iter()) {
                match element {
                    PatternElement::Variable(var_ref) => {
                        self.current_block_scopes
                            .initialize_var_mut(var_ref, value_ref.clone());
                    }
                    PatternElement::VariableWithFieldIndex(var_ref, _) => {
                        self.current_block_scopes
                            .initialize_var_mut(var_ref, value_ref.clone());
                    }
                    PatternElement::Wildcard(_) => {
                        // Skip wildcards
                        continue;
                    }
                }
            }

            let result = self.evaluate_expression(expression_to_evaluate);
            self.pop_block_scope();

            return result;
        }
        panic!("should not get here")
    }

    fn eval_normal_pattern_enum(
        &mut self,
        maybe_elements: Option<&Vec<PatternElement>>,
        expression_to_evaluate: &Expression,
        variant_ref: &EnumVariantTypeRef,
        value_ref: ValueRef,
    ) -> Result<Option<Value>, ExecuteError> {
        match value_ref.borrow_mut().clone() {
            Value::EnumVariantTuple(value_tuple_type, values) => {
                // First check if the variant types match
                if variant_ref.common().number != value_tuple_type.common.number {
                    return Ok(None); // Try next pattern
                }

                if let Some(elements) = maybe_elements {
                    assert_eq!(elements.len(), values.len());
                    self.push_block_scope();

                    for (element, value) in elements.iter().zip(values.iter()) {
                        match element {
                            PatternElement::Variable(var_ref) => {
                                self.current_block_scopes
                                    .initialize_var_mut(var_ref, value.clone());
                            }
                            PatternElement::VariableWithFieldIndex(var_ref, _) => {
                                self.current_block_scopes
                                    .initialize_var_mut(var_ref, value.clone());
                            }
                            PatternElement::Wildcard(_) => continue,
                        }
                    }

                    let result = self.evaluate_expression(&expression_to_evaluate);
                    self.pop_block_scope();
                    return Ok(Option::from(result?));
                } else {
                    panic!("not work");
                }
            }
            Value::EnumVariantStruct(value_enum_struct_type, values) => {
                info!(
                    ?value_enum_struct_type,
                    ?variant_ref,
                    "comparing enum variant struct match arm"
                );
                if value_enum_struct_type.common.number == variant_ref.common().number {
                    info!(?value_enum_struct_type, ?variant_ref, "FOUND!");
                    if let Some(elements) = maybe_elements {
                        self.push_block_scope();

                        for element in elements {
                            if let PatternElement::VariableWithFieldIndex(var_ref, field_index) =
                                element
                            {
                                let value = &values[*field_index];
                                info!(?value, "setting match arm variable");
                                self.current_block_scopes.init_var_ref(var_ref, value);
                            }
                        }

                        let result = self.evaluate_expression(&expression_to_evaluate);
                        self.pop_block_scope();
                        return Ok(Some(result?));
                    }
                }
            }

            Value::EnumVariantSimple(value_variant_ref) => {
                if value_variant_ref.common.number == variant_ref.common().number
                    && maybe_elements.is_none()
                {
                    return Ok(Some(self.evaluate_expression(&expression_to_evaluate)?));
                }
            }
            _ => {
                panic!("could not find it")
            }
        }

        Ok(None)
    }

    #[inline(always)]
    const fn modulo(a: i32, b: i32) -> i32 {
        ((a % b) + b) % b
    }

    #[inline(always)]
    const fn modulo_fp(a: Fp, b: Fp) -> Fp {
        let raw = ((a.inner() % b.inner()) + b.inner()) % b.inner();
        Fp::from_raw(raw)
    }

    #[allow(clippy::too_many_lines)]
    fn evaluate_binary_op(
        &self,
        node: &Node,
        left_val: Value,
        op: &BinaryOperatorKind,
        right_val: Value,
    ) -> Result<Value, ExecuteError> {
        let result: Value = match (&left_val, op, &right_val) {
            // Integer operations
            (Value::Int(a), BinaryOperatorKind::Add, Value::Int(b)) => Value::Int(a + b),
            (Value::Int(a), BinaryOperatorKind::Subtract, Value::Int(b)) => Value::Int(a - b),
            (Value::Int(a), BinaryOperatorKind::Multiply, Value::Int(b)) => Value::Int(a * b),
            (Value::Int(a), BinaryOperatorKind::Divide, Value::Int(b)) => {
                if *b == 0 {
                    return Err(self.create_err(ExecuteErrorKind::DivideByZero, node));
                }
                Value::Int(a / b)
            }
            (Value::Int(a), BinaryOperatorKind::Modulo, Value::Int(b)) => {
                Value::Int(Self::modulo(*a, *b))
            }
            (Value::Int(a), BinaryOperatorKind::Equal, Value::Int(b)) => Value::Bool(a == b),
            (Value::Int(a), BinaryOperatorKind::NotEqual, Value::Int(b)) => Value::Bool(a != b),
            (Value::Int(a), BinaryOperatorKind::LessThan, Value::Int(b)) => Value::Bool(a < b),
            (Value::Int(a), BinaryOperatorKind::GreaterThan, Value::Int(b)) => Value::Bool(a > b),
            (Value::Int(a), BinaryOperatorKind::LessEqual, Value::Int(b)) => Value::Bool(a <= b),
            (Value::Int(a), BinaryOperatorKind::GreaterEqual, Value::Int(b)) => Value::Bool(a >= b),

            // Float operations
            (Value::Float(a), BinaryOperatorKind::Equal, Value::Float(b)) => Value::Bool(a == b),
            (Value::Float(a), BinaryOperatorKind::NotEqual, Value::Float(b)) => Value::Bool(a != b),

            (Value::Float(a), BinaryOperatorKind::Add, Value::Float(b)) => Value::Float(*a + *b),
            (Value::Float(a), BinaryOperatorKind::Subtract, Value::Float(b)) => {
                Value::Float(*a - *b)
            }
            (Value::Float(a), BinaryOperatorKind::Multiply, Value::Float(b)) => {
                Value::Float(*a * *b)
            }
            (Value::Float(a), BinaryOperatorKind::Divide, Value::Float(b)) => {
                if b.abs().inner() <= 400 {
                    return Err(self.create_err(ExecuteErrorKind::DivideByZero, node));
                }
                Value::Float(*a / *b)
            }
            (Value::Float(a), BinaryOperatorKind::Modulo, Value::Float(b)) => {
                Value::Float(Self::modulo_fp(*a, *b))
            }

            (Value::Float(a), BinaryOperatorKind::GreaterThan, Value::Float(b)) => {
                Value::Bool(a > b)
            }
            (Value::Float(a), BinaryOperatorKind::GreaterEqual, Value::Float(b)) => {
                Value::Bool(a >= b)
            }
            (Value::Float(a), BinaryOperatorKind::LessThan, Value::Float(b)) => Value::Bool(a < b),
            (Value::Float(a), BinaryOperatorKind::LessEqual, Value::Float(b)) => {
                Value::Bool(a <= b)
            }

            // Boolean operations
            (Value::Bool(a), BinaryOperatorKind::LogicalAnd, Value::Bool(b)) => {
                Value::Bool(*a && *b)
            }
            (Value::Bool(a), BinaryOperatorKind::LogicalOr, Value::Bool(b)) => {
                Value::Bool(*a || *b)
            }

            // Comparison operations

            // RustType
            (Value::RustValue(_, left), BinaryOperatorKind::Equal, Value::RustValue(_, right)) => {
                let left_borrow = left.borrow();
                let right_borrow = right.borrow();
                let equal = left_borrow.eq_dyn(&**right_borrow);
                Value::Bool(equal)
            }
            (
                Value::RustValue(_, left),
                BinaryOperatorKind::NotEqual,
                Value::RustValue(_, right),
            ) => {
                let left_borrow = left.borrow();
                let right_borrow = right.borrow();
                let equal = left_borrow.eq_dyn(&**right_borrow);
                Value::Bool(!equal)
            }

            // String operations
            (Value::String(a), BinaryOperatorKind::Add, Value::String(b)) => {
                Value::String(a.to_owned() + b)
            }
            (Value::String(a), BinaryOperatorKind::Equal, Value::String(b)) => Value::Bool(a == b),

            (Value::String(a), BinaryOperatorKind::Add, Value::Int(b)) => {
                Value::String(a.to_owned() + &(*b).to_string())
            }
            (Value::Int(a), BinaryOperatorKind::Add, Value::String(b)) => {
                Value::String(a.to_string() + b)
            }

            // Enum
            (
                Value::EnumVariantSimple(a),
                BinaryOperatorKind::Equal,
                Value::EnumVariantSimple(b),
            ) => Value::Bool(a == b),
            (
                Value::EnumVariantSimple(a),
                BinaryOperatorKind::NotEqual,
                Value::EnumVariantSimple(b),
            ) => Value::Bool(a != b),

            // Bool
            (Value::Bool(a), BinaryOperatorKind::Equal, Value::Bool(b)) => Value::Bool(a == b),
            (Value::Bool(a), BinaryOperatorKind::NotEqual, Value::Bool(b)) => Value::Bool(a != b),

            (Value::Option(a), BinaryOperatorKind::Equal, Value::Option(b)) => Value::Bool(a == b),

            _ => {
                error!(?op, "invalid binary operation!!");
                panic!("invalid binary operation"); // TODO: improve error handling
            }
        };

        Ok(result)
    }

    fn evaluate_unary_op(
        &self,
        node: &Node,
        op: &UnaryOperatorKind,
        val: Value,
    ) -> Result<Value, ExecuteError> {
        match (op, val) {
            (UnaryOperatorKind::Negate, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnaryOperatorKind::Negate, Value::Float(n)) => Ok(Value::Float(-n)),
            (UnaryOperatorKind::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(self.create_err(ExecuteErrorKind::DivideByZero, node)),
        }
    }

    fn expressions_equal_to_values(
        &mut self,
        p0: &[Expression],
        p1: &[ValueRef],
    ) -> Result<bool, ExecuteError> {
        for (a, b_value) in p0.iter().zip(p1.iter()) {
            let a_value = self.evaluate_expression(a)?;

            if a_value != *b_value.borrow() {
                return Ok(false);
            }
        }

        Ok(true)
    }

    #[inline(always)]
    fn apply_compound_operator(
        &self,
        node: &Node,
        target: &mut Value,
        operator: &CompoundOperatorKind,
        source: &Value,
    ) -> Result<(), ExecuteError> {
        match operator {
            CompoundOperatorKind::Mul => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Multiply,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Div => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Divide,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Add => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Add,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Sub => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Subtract,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Modulo => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Modulo,
                    source.clone(),
                )?;
            }
        }
        Ok(())
    }

    /*
        fn evaluate_range(
            &mut self,
            min_expr: &Expression,
            max_expr: &Expression,
        ) -> Result<(i32, i32), ExecuteError> {
            let min_value = self.evaluate_expression_int(&min_expr)?;
            let max_value = self.evaluate_expression_int(&max_expr)?;

            Ok((min_value, max_value))
        }

        fn calculate_range(
            start_val: i32,
            end_val: i32,
            len: usize,
            mode: &RangeMode,
        ) -> (usize, usize) {
            let adjusted_min = if start_val < 0 {
                len + start_val as usize
            } else {
                start_val as usize
            };

            let mut adjusted_max = if end_val < 0 {
                len + end_val as usize
            } else {
                end_val as usize
            };
            if RangeMode::Inclusive == mode.clone() {
                adjusted_max += 1;
            }

            (adjusted_min, adjusted_max)
        }

        fn evaluate_and_calculate_range(
            &mut self,
            min_expr: &Expression,
            max_expr: &Expression,
            mode: &RangeMode,
            len: usize,
        ) -> Result<(usize, usize), ExecuteError> {
            let (start_val, end_val) = self.evaluate_range(min_expr, max_expr)?;

            Ok(Self::calculate_range(start_val, end_val, len, mode))
        }

        #[inline]
        fn evaluate_array_range_access(
            &mut self,
            base_expr: &Expression,
            array_type_ref: &ArrayTypeRef,
            min_expr: &Expression,
            max_expr: &Expression,
            mode: &RangeMode,
        ) -> Result<Value, ExecuteError> {
            let array_value = self.evaluate_expression(base_expr)?;

            if let Value::Array(_, values) = array_value {
                let (adjusted_start, adjusted_end) =
                    self.evaluate_and_calculate_range(min_expr, max_expr, mode, values.len())?;

                let slice = &values.as_slice()[adjusted_start..adjusted_end];
                Ok(Value::Array(array_type_ref.clone(), Vec::from(slice)))
            } else {
                Err(self.create_err(ExecuteErrorKind::NotAnArray, &base_expr.node))
            }
        }

        fn evaluate_expression_int(
            &mut self,
            int_expr: &Expression,
        ) -> Result<i32, ExecuteError> {
            let v = self.evaluate_expression(&int_expr)?;

            if let Value::Int(i) = v {
                Ok(i)
            } else {
                Err(self.create_err(ExecuteErrorKind::ExpectedInt, &int_expr.node))
            }
        }
    */

    /*
    fn evaluate_string_range_access(
        &mut self,
        string_expr: &Expression,
        start_expr: &Expression,
        end_expr: &Expression,
        mode: &RangeMode,
    ) -> Result<Value, ExecuteError> {
        let string_value = self.evaluate_expression(string_expr)?;

        if let Value::String(string) = string_value {
            let (adjusted_start, adjusted_end) =
                self.evaluate_and_calculate_range(start_expr, end_expr, mode, string.len())?;
            Ok(Value::String(
                string[adjusted_start..adjusted_end].to_string(),
            ))
        } else {
            Err(self.create_err(ExecuteErrorKind::ExpectedString, &string_expr.node))
        }
    }

     */

    fn create_err(&self, kind: ExecuteErrorKind, node: &Node) -> ExecuteError {
        ExecuteError {
            node: node.clone(),
            kind,
        }
    }

    fn evaluate_intrinsic_mut(
        &mut self,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        location: &SingleMutLocationExpression,
        arguments: &Vec<Expression>,
    ) -> Result<Value, ExecuteError> {
        let val = match intrinsic_fn {
            IntrinsicFunction::VecSelfPush => {
                let source_val = self.evaluate_expression(&arguments[0])?;
                let array_val_ref = self.evaluate_location(&location.0)?;

                match &mut *array_val_ref.borrow_mut() {
                    Value::Array(_type_id, vector) => {
                        vector.push(Rc::new(RefCell::new(source_val)));
                    }
                    _ => {
                        Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, &node))?;
                    }
                }
                //array_val_ref.borrow().clone()
                Value::Unit
            }
            IntrinsicFunction::VecSelfExtend => {
                let source_val = self.evaluate_expression(&arguments[0])?;

                let array_val_ref = self.evaluate_location(&location.0)?;
                match &mut *array_val_ref.borrow_mut() {
                    Value::Array(_type_id, vector) => match source_val {
                        Value::Array(_, items) => {
                            vector.extend(items);
                        }
                        _ => {
                            Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, &node))?;
                        }
                    },
                    _ => {
                        todo!("handle error")
                    }
                }

                // array_val_ref.borrow().clone()
                Value::Unit
            }
            _ => return Err(self.create_err(ExecuteErrorKind::UnknownMutIntrinsic, &node)),
        };

        Ok(val)
    }
}

#[inline]
#[must_use]
pub fn i64_sqrt(v: i64) -> i64 {
    assert!(v >= 0, "negative numbers are undefined for sqrt() {v}");

    if v == 0 {
        return v;
    }

    const MAX_ITERATIONS: usize = 40;
    const TOLERANCE: i64 = 2;

    let mut guess = v / 2;

    for _ in 0..MAX_ITERATIONS {
        let next_guess = (guess + v / guess) / 2;

        // Check if the change is within the tolerance level
        if (next_guess - guess).abs() <= TOLERANCE {
            return next_guess;
        }

        guess = next_guess;
    }

    guess // Return the last guess if convergence wasn't fully reached
}

#[allow(unused)]
pub fn values_to_value_refs(values: &[Value]) -> Vec<ValueRef> {
    let mut items = Vec::new();

    for x in values.iter().cloned() {
        items.push(Rc::new(RefCell::new(x)));
    }

    items
}

pub fn values_to_value_refs_owned(values: Vec<Value>) -> Vec<ValueRef> {
    values
        .into_iter()
        .map(|x| Rc::new(RefCell::new(x)))
        .collect()
}

pub fn wrap_in_option(maybe: Option<&ValueRef>) -> ValueRef {
    match maybe {
        None => Rc::new(RefCell::new(Value::Option(None))),
        Some(x) => Rc::new(RefCell::new(Value::Option(Some(x.clone())))),
    }
}
