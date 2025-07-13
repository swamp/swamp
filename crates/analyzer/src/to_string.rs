/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

//! Automatic generation of `to_string()`, `to_short_string()`, and `to_pretty_string()` functions.
//!
//! This module generates string representation functions for user-defined types in the Swamp language.
//! The generated functions are optimized for minimal virtual register usage through proper scope
//! management and register reuse.
//!
//! ## Generated Functions
//!
//! - **`to_string()`**: Full string representation with type names and structure
//! - **`to_short_string()`**: Compact representation without type names
//! - **`to_pretty_string(indentation: Int)`**: Multi-line formatted representation
//!
//! ## Key Features
//!
//! - **Scope-aware variable management**: Variables are properly scoped and virtual registers
//!   are reused when variables go out of scope
//! - **Efficient string building**: Uses mutable result variables and helper functions to
//!   minimize temporary variable creation
//! - **Recursive type handling**: Calls `to_string()` methods on child types rather than
//!   inlining their generation
//! - **Match arm scoping**: Each pattern match arm gets its own scope for pattern variables
//!
//! ## Implementation Notes
//!
//! This module intentionally avoids the normal analyzer helpers to prevent interference
//! with the main analysis phase. It maintains its own `GeneratedScope` with proper
//! virtual register tracking and cleanup.
use source_map_node::Node;
use std::collections::HashSet;
use swamp_attributes::Attributes;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    ArgumentExpression, AssociatedImpls, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    Expression, ExpressionKind, ForPattern, Function, FunctionRef, InternalFunctionDefinition,
    InternalFunctionIdAllocator, Iterable, LocalIdentifier, Match, MatchArm, NormalPattern,
    Pattern, PatternElement, Postfix, PostfixKind, StartOfChain, StartOfChainKind, UnaryOperator,
    UnaryOperatorKind, Variable, VariableRef, VariableScopes, VariableType, WhenBinding,
};
use swamp_types::prelude::{EnumType, NamedStructType, Signature, TypeCache, TypeForParameter};
use swamp_types::{TypeKind, TypeRef};

/// Helper to efficiently append a string to a mutable result variable
fn append_to_result(
    result_var: &VariableRef,
    string_to_append: Expression,
    string_type: &TypeRef,
    unit_type: &TypeRef,
    node: &Node,
) -> Expression {
    let result_access = create_var_access(result_var, string_type, node);
    let concat_expr = concat_expressions(result_access, string_to_append, string_type, node);
    create_var_reassignment(result_var, concat_expr, unit_type, node)
}

/// Helper to append a string literal to result
fn append_literal_to_result(
    result_var: &VariableRef,
    literal: &str,
    string_type: &TypeRef,
    unit_type: &TypeRef,
    node: &Node,
) -> Expression {
    let literal_expr = create_string_literal(literal, string_type, node);
    append_to_result(result_var, literal_expr, string_type, unit_type, node)
}

/// Helper to create a mutable string variable initialized with a literal
fn create_result_var_with_literal(
    scope: &mut GeneratedScope,
    var_name: &str,
    initial_value: &str,
    string_type: &TypeRef,
    unit_type: &TypeRef,
    node: &Node,
) -> (VariableRef, Expression) {
    let result_var = scope.create_local_mut_variable(var_name, string_type, node);
    let initial_literal = create_expr_resolved(
        ExpressionKind::StringLiteral(initial_value.to_string()),
        string_type.clone(),
        node,
    );
    let result_init = create_expr_resolved(
        ExpressionKind::VariableDefinition(result_var.clone(), Box::new(initial_literal)),
        unit_type.clone(),
        node,
    );
    (result_var, result_init)
}

/// Helper to create a binary operation expression
fn create_binary_op(
    kind: BinaryOperatorKind,
    left: Expression,
    right: Expression,
    result_type: TypeRef,
    node: &Node,
) -> Expression {
    create_expr_resolved(
        ExpressionKind::BinaryOp(BinaryOperator {
            kind,
            left: Box::new(left),
            right: Box::new(right),
            node: node.clone(),
        }),
        result_type,
        node,
    )
}

/// Helper to concatenate two expressions (left + right)
fn concat_expressions(
    left: Expression,
    right: Expression,
    string_type: &TypeRef,
    node: &Node,
) -> Expression {
    create_binary_op(
        BinaryOperatorKind::Add,
        left,
        right,
        string_type.clone(),
        node,
    )
}

/// Helper to create a string literal expression
fn create_string_literal(literal: &str, string_type: &TypeRef, node: &Node) -> Expression {
    create_expr_resolved(
        ExpressionKind::StringLiteral(literal.to_string()),
        string_type.clone(),
        node,
    )
}

/// Helper to create a variable access expression
fn create_var_access(var_ref: &VariableRef, var_type: &TypeRef, node: &Node) -> Expression {
    create_expr_resolved(
        ExpressionKind::VariableAccess(var_ref.clone()),
        var_type.clone(),
        node,
    )
}

/// Helper to create a variable reassignment expression
fn create_var_reassignment(
    var_ref: &VariableRef,
    new_value: Expression,
    unit_type: &TypeRef,
    node: &Node,
) -> Expression {
    create_expr_resolved(
        ExpressionKind::VariableReassignment(var_ref.clone(), Box::new(new_value)),
        unit_type.clone(),
        node,
    )
}

/// Helper to create "!`is_first`" condition for separators
fn create_not_first_condition(
    is_first_var: &VariableRef,
    bool_type: &TypeRef,
    node: &Node,
) -> Expression {
    let is_first_access = create_var_access(is_first_var, bool_type, node);
    create_expr_resolved(
        ExpressionKind::UnaryOp(UnaryOperator {
            kind: UnaryOperatorKind::Not,
            left: Box::new(is_first_access),
            node: node.clone(),
        }),
        bool_type.clone(),
        node,
    )
}

/// Helper to set a boolean variable to false
fn set_bool_var_false(
    var_ref: &VariableRef,
    bool_type: &TypeRef,
    unit_type: &TypeRef,
    node: &Node,
) -> Expression {
    let false_expr =
        create_expr_resolved(ExpressionKind::BoolLiteral(false), bool_type.clone(), node);
    create_var_reassignment(var_ref, false_expr, unit_type, node)
}

fn generate_to_string_for_named_struct(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    named: &NamedStructType,
    self_expression: &Expression,
    use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    if use_short_string_for_self {
        // For to_short_string, we skip the struct name and just show it as an anonymous struct
        generate_to_string_for_anon_struct(
            generator,
            scope,
            &named.anon_struct_type,
            self_expression,
            use_short_string_for_self,
            use_short_string_for_children,
        )
    } else {
        let node = self_expression.node.clone();
        let string_type = generator.types.string();
        let unit_type = generator.types.unit();

        // Create a mutable result variable to hold the string
        let result_var = scope.create_local_mut_variable("result", &string_type, &node);

        // Initialize with struct name
        let struct_name_string = create_expr_resolved(
            ExpressionKind::StringLiteral(format!("{} ", named.assigned_name.clone())),
            string_type.clone(),
            &node,
        );

        let result_init = create_expr_resolved(
            ExpressionKind::VariableDefinition(result_var.clone(), Box::new(struct_name_string)),
            unit_type.clone(),
            &node,
        );

        // Get the anonymous struct representation
        let anon_struct_string = generate_to_string_for_anon_struct(
            generator,
            scope,
            &named.anon_struct_type,
            self_expression,
            use_short_string_for_self,
            use_short_string_for_children,
        );

        // Add the anonymous struct representation to the result
        let result_access = create_expr_resolved(
            ExpressionKind::VariableAccess(result_var.clone()),
            string_type.clone(),
            &node,
        );

        let concat_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_access),
                right: Box::new(anon_struct_string),
                node: node.clone(),
            }),
            string_type.clone(),
            &node,
        );

        let add_anon_struct = create_expr_resolved(
            ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
            unit_type,
            &node,
        );

        // Return the result
        let result_access = create_expr_resolved(
            ExpressionKind::VariableAccess(result_var),
            string_type.clone(),
            &node,
        );

        create_expr_resolved(
            ExpressionKind::Block(vec![result_init, add_anon_struct, result_access]),
            string_type,
            &node,
        )
    }
}

#[must_use]
pub fn create_expr_resolved(kind: ExpressionKind, ty: TypeRef, ast_node: &Node) -> Expression {
    Expression {
        kind,
        ty,
        node: ast_node.clone(),
    }
}

#[allow(clippy::too_many_lines)]
fn generate_to_string_for_anon_struct(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    anonymous_struct_type_ref: &TypeRef,
    self_expression: &Expression,
    _use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let node = &self_expression.node;
    let string_type = generator.types.string();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();

    // Push a scope for struct generation variables
    scope.push_scope();

    // Create a mutable result variable initialized with opening brace
    let (result_var, result_init) =
        create_result_var_with_literal(scope, "result", "{ ", &string_type, &unit_type, node);

    // Create a variable to track if we're on the first field
    let is_first_var = scope.create_local_mut_variable("is_first", &bool_type, node);
    let is_first_init = create_expr_resolved(
        ExpressionKind::VariableDefinition(
            is_first_var.clone(),
            Box::new(create_expr_resolved(
                ExpressionKind::BoolLiteral(true),
                bool_type.clone(),
                node,
            )),
        ),
        unit_type.clone(),
        node,
    );

    let TypeKind::AnonymousStruct(anonymous_struct_type) = &*anonymous_struct_type_ref.kind else {
        panic!("internal error")
    };

    // Process each field
    let mut field_statements = Vec::new();

    for (field_index, (field_name, field_type)) in anonymous_struct_type
        .field_name_sorted_fields
        .iter()
        .enumerate()
    {
        // If not the first field, add a comma separator
        let add_separator = {
            let is_first_access = create_expr_resolved(
                ExpressionKind::VariableAccess(is_first_var.clone()),
                bool_type.clone(),
                node,
            );

            let not_is_first = create_expr_resolved(
                ExpressionKind::UnaryOp(UnaryOperator {
                    kind: UnaryOperatorKind::Not,
                    left: Box::new(is_first_access),
                    node: node.clone(),
                }),
                bool_type.clone(),
                node,
            );

            let add_separator_stmt =
                append_literal_to_result(&result_var, ", ", &string_type, &unit_type, node);

            create_expr_resolved(
                ExpressionKind::If(
                    BooleanExpression {
                        expression: Box::new(not_is_first),
                    },
                    Box::new(add_separator_stmt),
                    None,
                ),
                unit_type.clone(),
                node,
            )
        };

        // Set is_first to false after processing the first field
        let set_is_first_false = create_expr_resolved(
            ExpressionKind::VariableReassignment(
                is_first_var.clone(),
                Box::new(create_expr_resolved(
                    ExpressionKind::BoolLiteral(false),
                    bool_type.clone(),
                    node,
                )),
            ),
            unit_type.clone(),
            node,
        );

        // Add field name
        let add_field_name = append_literal_to_result(
            &result_var,
            &format!("{field_name}: "),
            &string_type,
            &unit_type,
            node,
        );

        // Get field value from the struct
        let postfix_kind = PostfixKind::StructField(anonymous_struct_type_ref.clone(), field_index);
        let postfix_lookup_field_in_self = Postfix {
            node: node.clone(),
            ty: field_type.field_type.clone(),
            kind: postfix_kind,
        };

        // Get field value and add it to result
        let field_value_expr = {
            let start_of_chain = StartOfChain {
                kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                node: node.clone(),
            };

            let field_access = create_expr_resolved(
                ExpressionKind::PostfixChain(start_of_chain, vec![postfix_lookup_field_in_self]),
                field_type.field_type.clone(),
                node,
            );

            let field_string = create_string_representation_of_expression(
                generator,
                field_access,
                node,
                use_short_string_for_children,
            );

            append_to_result(&result_var, field_string, &string_type, &unit_type, node)
        };

        let field_block = create_expr_resolved(
            ExpressionKind::Block(vec![
                add_separator,
                add_field_name,
                field_value_expr,
                set_is_first_false,
            ]),
            unit_type.clone(),
            node,
        );

        field_statements.push(field_block);
    }

    // Add closing brace
    let add_closing_brace =
        append_literal_to_result(&result_var, " }", &string_type, &unit_type, node);

    // Return the result
    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    // Combine all statements
    let mut all_statements = vec![result_init, is_first_init];
    all_statements.extend(field_statements);
    all_statements.push(add_closing_brace);
    all_statements.push(result_access);

    let final_result =
        create_expr_resolved(ExpressionKind::Block(all_statements), string_type, node);

    // Pop the scope to free up variables
    scope.pop_scope();

    final_result
}

fn generate_to_string_for_enum(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    enum_type: &EnumType,
    argument_expression: Expression,
    node: &Node,
    use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();

    // Create match arms for each variant
    let mut arms = Vec::new();

    for (variant_name, variant_type) in &enum_type.variants {
        let variant_str = if use_short_string_for_self {
            variant_name.clone()
        } else {
            format!("{}::{}", enum_type.assigned_name, variant_name)
        };

        let arm = if *variant_type.payload_type.kind == TypeKind::Unit {
            // Unit variant - simple enum pattern with no payload
            let pattern = NormalPattern::EnumPattern(variant_type.clone(), None);
            let result_expr = create_expr_resolved(
                ExpressionKind::StringLiteral(variant_str),
                string_type.clone(),
                node,
            );

            MatchArm {
                pattern: Pattern::Normal(pattern, None), // No guard
                expression: Box::new(result_expr.clone()),
                expression_type: result_expr.ty,
            }
        } else {
            // Push a scope for this match arm's pattern variables
            scope.push_scope();

            // Variant with payload - create a variable to capture the payload
            let payload_var =
                scope.create_local_variable_param_like("payload", &variant_type.payload_type, node);

            // Create pattern element for the payload variable
            let payload_pattern_element = PatternElement::Variable(payload_var.clone());

            // Create enum pattern with payload destructuring
            let pattern = NormalPattern::EnumPattern(
                variant_type.clone(),
                Some(vec![payload_pattern_element]),
            );

            // Create expression to access the captured payload
            let payload_access = create_expr_resolved(
                ExpressionKind::VariableAccess(payload_var),
                variant_type.payload_type.clone(),
                node,
            );

            // Get string representation of the payload
            let payload_string = create_string_representation_of_expression(
                generator,
                payload_access,
                node,
                use_short_string_for_children,
            );

            // Handle different payload types appropriately:
            // - Struct payloads: always use space + braces format
            // - Tuple payloads: don't add extra parentheses (tuples already have them)
            // - Other payloads: wrap in parentheses
            let final_result = match &*variant_type.payload_type.kind {
                TypeKind::AnonymousStruct(_) | TypeKind::NamedStruct(_) => {
                    // Struct payload: always variant_str + " " + payload (space before brace)
                    let prefix = create_string_literal(&format!("{variant_str} "), &string_type, node);
                    concat_expressions(prefix, payload_string, &string_type, node)
                }
                TypeKind::Tuple(_) => {
                    // Tuple payload: variant_str + payload (tuples already have parentheses)
                    let prefix = create_string_literal(&variant_str, &string_type, node);
                    concat_expressions(prefix, payload_string, &string_type, node)
                }
                _ => {
                    // Other payload: variant_str + "(" + payload + ")"
                    let prefix = create_string_literal(&format!("{variant_str}("), &string_type, node);
                    let suffix = create_string_literal(")", &string_type, node);
                    let temp_concat = concat_expressions(prefix, payload_string, &string_type, node);
                    concat_expressions(temp_concat, suffix, &string_type, node)
                }
            };

            scope.pop_scope(); // Clean up pattern variables

            MatchArm {
                pattern: Pattern::Normal(pattern, None), // No guard
                expression: Box::new(final_result.clone()),
                expression_type: final_result.ty,
            }
        };

        arms.push(arm);
    }

    // Create the match expression
    create_expr_resolved(
        ExpressionKind::Match(Match {
            expression: Box::new(argument_expression),
            arms,
        }),
        string_type,
        node,
    )
}

fn create_string_representation_of_expression(
    generator: &mut ExpressionGenerator,
    expression_to_convert: Expression,
    node: &Node,
    prefer_short_string: bool,
) -> Expression {
    let string_type = generator.types.string();
    let ty = expression_to_convert.ty.clone();

    if matches!(*ty.kind, TypeKind::String(..) | TypeKind::StringStorage(..)) {
        // For strings, wrap in quotes: "\"" + string + "\""
        let quote_expr = create_string_literal("\"", &string_type, node);
        let left_concat = concat_expressions(
            quote_expr.clone(),
            expression_to_convert,
            &string_type,
            node,
        );
        concat_expressions(left_concat, quote_expr, &string_type, node)
    } else {
        // Choose which function to use based on the prefer_short_string parameter
        let function_name = if prefer_short_string {
            "short_string"
        } else {
            "string"
        };

        // Simply call the appropriate string function
        let function_ref = if let Some(fn_def) = generator
            .associated_impls
            .get_internal_member_function(&ty, function_name)
        {
            Function::Internal(fn_def.clone())
        } else {
            // If the function doesn't exist yet, fall back to a simple representation
            let type_name = format!("({ty})");
            return create_expr_resolved(
                ExpressionKind::StringLiteral(type_name),
                string_type,
                node,
            );
        };

        let start_of_chain = StartOfChain {
            kind: StartOfChainKind::Expression(Box::from(expression_to_convert)),
            node: node.clone(),
        };
        let postfix_call_to_string = Postfix {
            node: node.clone(),
            ty: generator.types.string(),
            kind: PostfixKind::MemberCall(FunctionRef::from(function_ref), vec![]),
        };

        let lookup_kind =
            ExpressionKind::PostfixChain(start_of_chain, vec![postfix_call_to_string]);
        create_expr_resolved(lookup_kind, string_type, node)
    }
}

pub struct GeneratedScope {
    pub scope: VariableScopes,
    active_virtual_registers: HashSet<u8>,
    scope_stack: Vec<ScopeFrame>,
    next_unique_id: usize,
}

#[derive(Debug)]
struct ScopeFrame {
    variables_in_scope: Vec<VariableRef>,
    virtual_registers_in_scope: Vec<u8>,
}

impl GeneratedScope {
    pub fn new() -> Self {
        Self {
            scope: VariableScopes::default(),
            active_virtual_registers: HashSet::new(),
            scope_stack: Vec::new(),
            next_unique_id: 1, // Start at 1, 0 is reserved for return value
        }
    }

    /// Push a new scope frame for proper variable management
    pub fn push_scope(&mut self) {
        self.scope_stack.push(ScopeFrame {
            variables_in_scope: Vec::new(),
            virtual_registers_in_scope: Vec::new(),
        });
    }

    /// Pop the current scope and free up virtual registers
    pub fn pop_scope(&mut self) {
        if let Some(frame) = self.scope_stack.pop() {
            // Free up virtual registers used by variables in this scope
            for virtual_reg in &frame.virtual_registers_in_scope {
                self.active_virtual_registers.remove(virtual_reg);
            }
        }
    }

    /// Find a free virtual register, reusing if possible
    fn allocate_virtual_register(&mut self) -> Option<u8> {
        // First try to reuse a freed virtual register (1-48)
        for reg in 1..=48u8 {
            if !self.active_virtual_registers.contains(&reg) {
                self.active_virtual_registers.insert(reg);
                return Some(reg);
            }
        }
        None // All virtual registers are in use
    }

    /// Get the next unique ID (never reused)
    const fn allocate_unique_id(&mut self) -> usize {
        let id = self.next_unique_id;
        self.next_unique_id += 1;
        id
    }

    /// Create a variable and track it in the current scope
    fn create_variable_in_current_scope(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
        is_mutable: bool,
        var_type: VariableType,
    ) -> VariableRef {
        let virtual_register = self
            .allocate_virtual_register()
            .unwrap_or_else(||
                panic!("out of virtual registers for variable {assigned_name}")

            );

        let unique_id = self.allocate_unique_id();

        let var_ref = VariableRef::new(Variable {
            name: node.clone(),
            assigned_name: assigned_name.to_string(),
            resolved_type: variable_type.clone(),
            mutable_node: if is_mutable { Some(node.clone()) } else { None },
            variable_type: var_type,
            scope_index: 0,
            variable_index: 0,
            unique_id_within_function: unique_id,
            virtual_register,
            is_unused: false,
        });

        self.scope.all_variables.push(var_ref.clone());

        // Track this variable in the current scope
        if let Some(current_frame) = self.scope_stack.last_mut() {
            current_frame.variables_in_scope.push(var_ref.clone());
            current_frame
                .virtual_registers_in_scope
                .push(virtual_register);
        }

        var_ref
    }

    pub(crate) fn create_local_variable(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
    ) -> VariableRef {
        self.create_variable_in_current_scope(
            assigned_name,
            variable_type,
            node,
            false,
            VariableType::Local,
        )
    }


    pub(crate) fn create_local_variable_param_like(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
    ) -> VariableRef {
        self.create_variable_in_current_scope(
            assigned_name,
            variable_type,
            node,
            false,
            VariableType::Parameter,
        )
    }

    pub(crate) fn create_parameter(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
    ) -> VariableRef {
        self.create_variable_in_current_scope(
            assigned_name,
            variable_type,
            node,
            false,
            VariableType::Parameter,
        )
    }

    pub(crate) fn create_local_mut_variable(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
    ) -> VariableRef {
        self.create_variable_in_current_scope(
            assigned_name,
            variable_type,
            node,
            true,
            VariableType::Local,
        )
    }
}

pub struct ExpressionGenerator<'a> {
    pub types: &'a mut TypeCache,
    pub associated_impls: &'a AssociatedImpls,
}

impl<'a> ExpressionGenerator<'a> {
    pub const fn new(types: &'a mut TypeCache, associated_impls: &'a AssociatedImpls) -> Self {
        Self {
            types,
            associated_impls,
        }
    }
}

#[allow(clippy::too_many_lines)]
fn generate_to_string_for_sequence_like(
    generator: &mut ExpressionGenerator,
    block_scope: &mut GeneratedScope,
    self_expression: Expression,
    _iterable_type: &TypeRef,
    element_type: &TypeRef,
    node: &Node,
    _use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();
    let int_type = generator.types.int();

    // Push a scope for the sequence generation variables
    block_scope.push_scope();

    // let mut result = "["
    let (result_var, result_var_def) =
        create_result_var_with_literal(block_scope, "result", "[", &string_type, &unit_type, node);

    // let mut is_first = true
    let (is_first_var, is_first_var_def) = {
        let var = block_scope.create_local_mut_variable("is_first", &bool_type, node);
        let true_expr =
            create_expr_resolved(ExpressionKind::BoolLiteral(true), bool_type.clone(), node);
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(true_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    let for_loop = {
        let element_var = block_scope.create_local_variable_param_like("element", element_type, node);

        // if !is_first { result = result + ", " }
        let if_body = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let comma_expr = create_expr_resolved(
                ExpressionKind::StringLiteral(", ".to_string()),
                string_type.clone(),
                node,
            );
            let concat_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(comma_expr),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
                unit_type.clone(),
                node,
            )
        };

        let if_expr = {
            let is_first_access = create_expr_resolved(
                ExpressionKind::VariableAccess(is_first_var.clone()),
                bool_type.clone(),
                node,
            );
            let condition = create_expr_resolved(
                ExpressionKind::UnaryOp(UnaryOperator {
                    kind: UnaryOperatorKind::Not,
                    left: Box::new(is_first_access),
                    node: node.clone(),
                }),
                bool_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::If(
                    BooleanExpression {
                        expression: Box::new(condition),
                    },
                    Box::new(if_body),
                    None,
                ),
                unit_type.clone(),
                node,
            )
        };

        // result = result + element.to_string() or element.to_short_string()
        let append_element_expr = {
            let element_access = create_expr_resolved(
                ExpressionKind::VariableAccess(element_var.clone()),
                element_type.clone(),
                node,
            );
            let string_repr_expr = create_string_representation_of_expression(
                generator,
                element_access,
                node,
                use_short_string_for_children,
            );
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let concat_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(string_repr_expr),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
                unit_type.clone(),
                node,
            )
        };

        // is_first = false
        let set_is_first_to_false = {
            let false_expr =
                create_expr_resolved(ExpressionKind::BoolLiteral(false), bool_type.clone(), node);
            create_expr_resolved(
                ExpressionKind::VariableReassignment(is_first_var, Box::new(false_expr)),
                unit_type.clone(),
                node,
            )
        };

        let for_body = create_expr_resolved(
            ExpressionKind::Block(vec![if_expr, append_element_expr, set_is_first_to_false]),
            unit_type.clone(),
            node,
        );

        let iterable = Iterable {
            key_type: Some(int_type),
            value_type: element_type.clone(),
            resolved_expression: Box::new(self_expression),
        };

        let for_pattern = ForPattern::Single(element_var);
        create_expr_resolved(
            ExpressionKind::ForLoop(for_pattern, iterable, Box::new(for_body)),
            unit_type.clone(),
            node,
        )
    };

    // result = result + "]"
    let closing_bracket_def = {
        let result_access = create_expr_resolved(
            ExpressionKind::VariableAccess(result_var.clone()),
            string_type.clone(),
            node,
        );
        let closing_bracket = create_expr_resolved(
            ExpressionKind::StringLiteral("]".to_string()),
            string_type.clone(),
            node,
        );
        let concat_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_access),
                right: Box::new(closing_bracket),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        );
        create_expr_resolved(
            ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
            unit_type,
            node,
        )
    };

    let result_access_expr = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    let final_result = create_expr_resolved(
        ExpressionKind::Block(vec![
            result_var_def,
            is_first_var_def,
            for_loop,
            closing_bracket_def,
            result_access_expr,
        ]),
        string_type,
        node,
    );

    // Pop the scope to free up variables
    block_scope.pop_scope();

    final_result
}

#[allow(clippy::too_many_lines)]
fn generate_to_string_for_map_like(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    _map_type: &TypeRef,
    key_type: &TypeRef,
    value_type: &TypeRef,
    node: &Node,
    _use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();

    // let mut result = "[|"
    let (result_var, result_var_def) = {
        let var = scope.create_local_mut_variable("result", &string_type, node);
        let opening_brace = create_expr_resolved(
            ExpressionKind::StringLiteral("[|".to_string()),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(opening_brace)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // let mut is_first = true
    let (is_first_var, is_first_var_def) = {
        let var = scope.create_local_mut_variable("is_first", &bool_type, node);
        let true_expr =
            create_expr_resolved(ExpressionKind::BoolLiteral(true), bool_type.clone(), node);
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(true_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    let for_loop = {
        // For maps, we need two variables: key and value
        let key_var = scope.create_local_variable_param_like("key", key_type, node);
        let value_var = scope.create_local_variable_param_like("val", value_type, node);

        // if !is_first { result = result + ", " }
        let if_body = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let comma_expr = create_expr_resolved(
                ExpressionKind::StringLiteral(", ".to_string()),
                string_type.clone(),
                node,
            );
            let concat_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(comma_expr),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
                unit_type.clone(),
                node,
            )
        };

        let if_expr = {
            let is_first_access = create_expr_resolved(
                ExpressionKind::VariableAccess(is_first_var.clone()),
                bool_type.clone(),
                node,
            );
            let condition = create_expr_resolved(
                ExpressionKind::UnaryOp(UnaryOperator {
                    kind: UnaryOperatorKind::Not,
                    left: Box::new(is_first_access),
                    node: node.clone(),
                }),
                bool_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::If(
                    BooleanExpression {
                        expression: Box::new(condition),
                    },
                    Box::new(if_body),
                    None,
                ),
                unit_type.clone(),
                node,
            )
        };

        // result = result + key.to_string() + ": " + value.to_string()
        let append_key_value_expr = {
            let key_access = create_expr_resolved(
                ExpressionKind::VariableAccess(key_var.clone()),
                key_type.clone(),
                node,
            );
            let key_string_repr = create_string_representation_of_expression(
                generator,
                key_access,
                node,
                use_short_string_for_children,
            );

            let value_access = create_expr_resolved(
                ExpressionKind::VariableAccess(value_var.clone()),
                value_type.clone(),
                node,
            );
            let value_string_repr = create_string_representation_of_expression(
                generator,
                value_access,
                node,
                use_short_string_for_children,
            );

            let colon_expr = create_expr_resolved(
                ExpressionKind::StringLiteral(": ".to_string()),
                string_type.clone(),
                node,
            );

            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );

            // result + key_string + ": " + value_string
            let temp1 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(key_string_repr),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            let temp2 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp1),
                    right: Box::new(colon_expr),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            let final_concat = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp2),
                    right: Box::new(value_string_repr),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_concat)),
                unit_type.clone(),
                node,
            )
        };

        // is_first = false
        let set_is_first_to_false = {
            let false_expr =
                create_expr_resolved(ExpressionKind::BoolLiteral(false), bool_type.clone(), node);
            create_expr_resolved(
                ExpressionKind::VariableReassignment(is_first_var.clone(), Box::new(false_expr)),
                unit_type.clone(),
                node,
            )
        };

        let for_body = create_expr_resolved(
            ExpressionKind::Block(vec![if_expr, append_key_value_expr, set_is_first_to_false]),
            unit_type.clone(),
            node,
        );

        let iterable = Iterable {
            key_type: Some(key_type.clone()),
            value_type: value_type.clone(),
            resolved_expression: Box::new(self_expression),
        };

        let for_pattern = ForPattern::Pair(key_var, value_var);
        create_expr_resolved(
            ExpressionKind::ForLoop(for_pattern, iterable, Box::new(for_body)),
            unit_type.clone(),
            node,
        )
    };

    // Check if map is empty and handle special case for empty map syntax "[:]"
    let closing_brace_def = {
        // if is_first { result = "[:]" } else { result = result + "|]" }
        let is_first_access = create_expr_resolved(
            ExpressionKind::VariableAccess(is_first_var),
            bool_type.clone(),
            node,
        );

        // Empty map case: result = "[:]"
        let empty_map_case = {
            let empty_map_literal = create_expr_resolved(
                ExpressionKind::StringLiteral("[:]".to_string()),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(
                    result_var.clone(),
                    Box::new(empty_map_literal),
                ),
                unit_type.clone(),
                node,
            )
        };

        // Non-empty map case: result = result + "|]"
        let non_empty_map_case = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let closing_brace = create_expr_resolved(
                ExpressionKind::StringLiteral("|]".to_string()),
                string_type.clone(),
                node,
            );
            let concat_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(closing_brace),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
                unit_type.clone(),
                node,
            )
        };

        create_expr_resolved(
            ExpressionKind::If(
                BooleanExpression {
                    expression: Box::new(is_first_access),
                },
                Box::new(empty_map_case),
                Some(Box::new(non_empty_map_case)),
            ),
            unit_type,
            node,
        )
    };

    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    create_expr_resolved(
        ExpressionKind::Block(vec![
            result_var_def,
            is_first_var_def,
            for_loop,
            closing_brace_def,
            result_access,
        ]),
        string_type,
        node,
    )
}

fn generate_to_string_for_optional(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    inner_type: &TypeRef,
    node: &Node,
    use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();

    if use_short_string_for_self {
        // For to_short_string on optional, we return the inner value's string for Some, "-" for None
        // This creates: when value_var = self_expression { value_var.to_short_string() } else { "-" }
        // Create a variable to bind the unwrapped optional value
        let value_var = scope.create_local_variable_param_like("value", inner_type, node);

        // Get the variable access for the unwrapped value
        let value_access = create_expr_resolved(
            ExpressionKind::VariableAccess(value_var.clone()),
            inner_type.clone(),
            node,
        );

        // Get the to_short_string for the inner value (just return the inner value's representation)
        let value_string = create_string_representation_of_expression(
            generator,
            value_access,
            node,
            use_short_string_for_children,
        );

        // Create the None case: "-" for short representation
        let none_result = create_expr_resolved(
            ExpressionKind::StringLiteral("-".to_string()),
            string_type.clone(),
            node,
        );

        // Create the `when` binding
        let binding = WhenBinding {
            variable: value_var,
            expr: self_expression,
        };

        // Create the `when` expression
        create_expr_resolved(
            ExpressionKind::When(
                vec![binding],
                Box::new(value_string),
                Some(Box::new(none_result)),
            ),
            string_type,
            node,
        )
    } else {
        // For optional types, we'll use a when expression instead of complex pattern matching
        // This creates: when value_var = self_expression { "Some(" + value_var.to_string() + ")" } else { "None" }

        // Create a variable to bind the unwrapped optional value
        let value_var = scope.create_local_variable("value", inner_type, node);

        // Create the `Some` case: "Some(" + value.to_string() + ")"
        let some_prefix = create_string_literal("Some(", &string_type, node);
        let some_suffix = create_string_literal(")", &string_type, node);

        // Get the variable access for the unwrapped value
        let value_access = create_var_access(&value_var, inner_type, node);

        // Get the to_string for the inner value
        let value_string = create_string_representation_of_expression(
            generator,
            value_access,
            node,
            use_short_string_for_children,
        );

        // Concatenate: "Some(" + value.to_string() + ")"
        let prefix_plus_value = concat_expressions(some_prefix, value_string, &string_type, node);
        let some_result = concat_expressions(prefix_plus_value, some_suffix, &string_type, node);

        // Create the None case
        let none_result = create_string_literal("None", &string_type, node);

        let binding = WhenBinding {
            variable: value_var,
            expr: self_expression,
        };

        create_expr_resolved(
            ExpressionKind::When(
                vec![binding],
                Box::new(some_result),
                Some(Box::new(none_result)),
            ),
            string_type,
            node,
        )
    }
}

fn generate_to_string_for_tuple(
    generator: &mut ExpressionGenerator,
    self_expression: &Expression,
    tuple_types: &[TypeRef],
    node: &Node,
    _use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();

    // Create opening parenthesis
    let mut result_expr = create_expr_resolved(
        ExpressionKind::StringLiteral("(".to_string()),
        string_type.clone(),
        node,
    );

    // Generate string for each tuple element
    for (index, element_type) in tuple_types.iter().enumerate() {
        let start_of_chain = StartOfChain {
            kind: StartOfChainKind::Expression(Box::new(self_expression.clone())),
            node: node.clone(),
        };
        let postfix = Postfix {
            node: node.clone(),
            ty: element_type.clone(),
            kind: PostfixKind::StructField(self_expression.ty.clone(), index),
        };
        let element_access = create_expr_resolved(
            ExpressionKind::PostfixChain(start_of_chain, vec![postfix]),
            element_type.clone(),
            node,
        );

        let element_string = create_string_representation_of_expression(
            generator,
            element_access,
            node,
            use_short_string_for_children,
        );

        let concat_kind = BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(result_expr),
            right: Box::new(element_string),
            node: node.clone(),
        };
        result_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(concat_kind),
            string_type.clone(),
            node,
        );

        if index < tuple_types.len() - 1 {
            let comma_expr = create_expr_resolved(
                ExpressionKind::StringLiteral(", ".to_string()),
                string_type.clone(),
                node,
            );
            let comma_concat_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_expr),
                right: Box::new(comma_expr),
                node: node.clone(),
            };
            result_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(comma_concat_kind),
                string_type.clone(),
                node,
            );
        }
    }

    let closing_expr = create_expr_resolved(
        ExpressionKind::StringLiteral(")".to_string()),
        string_type.clone(),
        node,
    );

    let final_concat_kind = BinaryOperator {
        kind: BinaryOperatorKind::Add,
        left: Box::new(result_expr),
        right: Box::new(closing_expr),
        node: node.clone(),
    };

    create_expr_resolved(
        ExpressionKind::BinaryOp(final_concat_kind),
        string_type,
        node,
    )
}

#[allow(clippy::too_many_lines)]
pub fn internal_generate_to_string_function_for_type(
    generator: &mut ExpressionGenerator,
    id_gen: &mut InternalFunctionIdAllocator,
    module_path: &[String],
    ty: &TypeRef,
    resolved_node: &Node,
    is_short_string: bool,
) -> InternalFunctionDefinition {
    let mut block_scope_to_use = GeneratedScope::new();

    // Create the "self" parameter using the same method as normal functions
    let variable_ref = block_scope_to_use.create_parameter("self", ty, resolved_node);

    let first_self_param = create_expr_resolved(
        ExpressionKind::VariableAccess(variable_ref),
        ty.clone(),
        resolved_node,
    );

    let body_expr = match &*ty.kind {
        // Primitive types have their own to_string() implementations in core_text()
        TypeKind::Byte => panic!("Byte to_string() is handled in core_text(), not generated here"),
        TypeKind::Codepoint => {
            panic!("Codepoint to_string() is handled in core_text(), not generated here")
        }
        TypeKind::Int => panic!("Int to_string() is handled in core_text(), not generated here"),
        TypeKind::Float => {
            panic!("Float to_string() is handled in core_text(), not generated here")
        }
        TypeKind::Bool => panic!("Bool to_string() is handled in core_text(), not generated here"),
        TypeKind::Range(_) => {
            panic!("Range to_string() is handled in core_text(), not generated here")
        }
        TypeKind::String { .. } => first_self_param,
        TypeKind::StringStorage { .. } => first_self_param,
        // Unit and Function types cannot be stored in fields/collections in Swamp, so no to_string() needed
        TypeKind::Unit => panic!("Unit type cannot be stored in fields, no to_string() needed"),
        TypeKind::Tuple(tuple_types) => generate_to_string_for_tuple(
            generator,
            &first_self_param,
            tuple_types,
            resolved_node,
            is_short_string,
            is_short_string,
        ),
        TypeKind::NamedStruct(named) => generate_to_string_for_named_struct(
            generator,
            &mut block_scope_to_use,
            named,
            &first_self_param,
            is_short_string,
            is_short_string,
        ),
        TypeKind::AnonymousStruct(_anon_struct) => generate_to_string_for_anon_struct(
            generator,
            &mut block_scope_to_use,
            ty,
            &first_self_param,
            is_short_string,
            is_short_string,
        ),
        TypeKind::Enum(enum_type) => generate_to_string_for_enum(
            generator,
            &mut block_scope_to_use,
            enum_type,
            first_self_param,
            resolved_node,
            is_short_string,
            is_short_string,
        ),
        TypeKind::Function(_) => {
            panic!("Function types cannot be stored in fields, no to_string() needed")
        }
        TypeKind::Optional(inner_type) => generate_to_string_for_optional(
            generator,
            &mut block_scope_to_use,
            first_self_param,
            inner_type,
            resolved_node,
            is_short_string,
            is_short_string,
        ),
        TypeKind::FixedCapacityAndLengthArray(element_type, _)
        | TypeKind::SliceView(element_type)
        | TypeKind::DynamicLengthVecView(element_type)
        | TypeKind::VecStorage(element_type, _)
        | TypeKind::StackView(element_type)
        | TypeKind::QueueView(element_type)
        | TypeKind::StackStorage(element_type, _)
        | TypeKind::QueueStorage(element_type, _)
        | TypeKind::SparseView(element_type)
        | TypeKind::SparseStorage(element_type, _) => generate_to_string_for_sequence_like(
            generator,
            &mut block_scope_to_use,
            first_self_param,
            ty,
            element_type,
            resolved_node,
            is_short_string,
            is_short_string,
        ),
        TypeKind::GridView(element_type) | TypeKind::GridStorage(element_type, _, _) => {
            generate_to_string_for_grid(
                generator,
                &mut block_scope_to_use,
                first_self_param,
                element_type,
                resolved_node,
                is_short_string,
                is_short_string,
            )
        }
        TypeKind::MapStorage(key_type, value_type, _)
        | TypeKind::DynamicLengthMapView(key_type, value_type) => generate_to_string_for_map_like(
            generator,
            &mut block_scope_to_use,
            first_self_param,
            ty,
            key_type,
            value_type,
            resolved_node,
            is_short_string,
            is_short_string,
        ),
    };

    let unique_function_id = id_gen.alloc();

    // Finalize the scope to set the highest register watermark
    block_scope_to_use.scope.finalize();

    InternalFunctionDefinition {
        body: body_expr,
        name: LocalIdentifier(resolved_node.clone()),
        assigned_name: if is_short_string {
            "short_string"
        } else {
            "string"
        }
            .to_string(),
        associated_with_type: Option::from(ty.clone()),
        defined_in_module_path: module_path.to_vec(),
        signature: Signature {
            parameters: vec![TypeForParameter {
                name: "self".to_string(),
                resolved_type: ty.clone(),
                is_mutable: false,
                node: None,
            }],
            return_type: generator.types.string(),
        },
        function_variables: block_scope_to_use.scope.clone(),
        program_unique_id: unique_function_id,
        attributes: Attributes::default(),
    }
}

pub fn internal_generate_to_short_string_function_for_type(
    generator: &mut ExpressionGenerator,
    id_gen: &mut InternalFunctionIdAllocator,
    module_path: &[String],
    ty: &TypeRef,
    resolved_node: &Node,
) -> InternalFunctionDefinition {
    internal_generate_to_string_function_for_type(
        generator,
        id_gen,
        module_path,
        ty,
        resolved_node,
        true,
    )
}

pub fn internal_generate_to_pretty_string_function_for_type(
    generator: &mut ExpressionGenerator,
    id_gen: &mut InternalFunctionIdAllocator,
    module_path: &[String],
    ty: &TypeRef,
    resolved_node: &Node,
) -> InternalFunctionDefinition {
    let mut block_scope_to_use = GeneratedScope::new();

    // Create the "self" parameter
    let self_variable_ref = block_scope_to_use.create_parameter("self", ty, resolved_node);

    // Create the "indentation" parameter
    let int_type = generator.types.int();
    let indentation_variable_ref =
        block_scope_to_use.create_parameter("indentation", &int_type, resolved_node);

    let self_param = create_expr_resolved(
        ExpressionKind::VariableAccess(self_variable_ref),
        ty.clone(),
        resolved_node,
    );

    let indentation_param = create_expr_resolved(
        ExpressionKind::VariableAccess(indentation_variable_ref),
        int_type.clone(),
        resolved_node,
    );

    let body_expr = generate_to_pretty_string_for_type(
        generator,
        &mut block_scope_to_use,
        ty,
        &self_param,
        indentation_param,
        resolved_node,
        false, // use_short_string_for_self
        false, // use_short_string_for_children
    );

    let unique_function_id = id_gen.alloc();

    block_scope_to_use.scope.finalize();

    InternalFunctionDefinition {
        body: body_expr,
        name: LocalIdentifier(resolved_node.clone()),
        assigned_name: "pretty_string_with_indent".to_string(),
        associated_with_type: Option::from(ty.clone()),
        defined_in_module_path: module_path.to_vec(),
        signature: Signature {
            parameters: vec![
                TypeForParameter {
                    name: "self".to_string(),
                    resolved_type: ty.clone(),
                    is_mutable: false,
                    node: None,
                },
                TypeForParameter {
                    name: "indentation".to_string(),
                    resolved_type: int_type,
                    is_mutable: false,
                    node: None,
                },
            ],
            return_type: generator.types.string(),
        },
        function_variables: block_scope_to_use.scope.clone(),
        program_unique_id: unique_function_id,
        attributes: Attributes::default(),
    }
}

pub fn internal_generate_to_pretty_short_string_function_for_type(
    generator: &mut ExpressionGenerator,
    id_gen: &mut InternalFunctionIdAllocator,
    module_path: &[String],
    ty: &TypeRef,
    resolved_node: &Node,
) -> InternalFunctionDefinition {
    let mut block_scope_to_use = GeneratedScope::new();

    // Create the "self" parameter
    let self_variable_ref = block_scope_to_use.create_parameter("self", ty, resolved_node);

    // Create the "indentation" parameter
    let int_type = generator.types.int();
    let indentation_variable_ref =
        block_scope_to_use.create_parameter("indentation", &int_type, resolved_node);

    let self_param = create_expr_resolved(
        ExpressionKind::VariableAccess(self_variable_ref),
        ty.clone(),
        resolved_node,
    );

    let indentation_param = create_expr_resolved(
        ExpressionKind::VariableAccess(indentation_variable_ref),
        int_type.clone(),
        resolved_node,
    );

    let body_expr = generate_to_pretty_string_for_type(
        generator,
        &mut block_scope_to_use,
        ty,
        &self_param,
        indentation_param,
        resolved_node,
        true, // use_short_string_for_self (short format for top level)
        true, // use_short_string_for_children (short format for children)
    );

    let unique_function_id = id_gen.alloc();

    block_scope_to_use.scope.finalize();

    InternalFunctionDefinition {
        body: body_expr,
        name: LocalIdentifier(resolved_node.clone()),
        assigned_name: "pretty_short_string".to_string(),
        associated_with_type: Option::from(ty.clone()),
        defined_in_module_path: module_path.to_vec(),
        signature: Signature {
            parameters: vec![
                TypeForParameter {
                    name: "self".to_string(),
                    resolved_type: ty.clone(),
                    is_mutable: false,
                    node: None,
                },
                TypeForParameter {
                    name: "indentation".to_string(),
                    resolved_type: int_type,
                    is_mutable: false,
                    node: None,
                },
            ],
            return_type: generator.types.string(),
        },
        function_variables: block_scope_to_use.scope.clone(),
        program_unique_id: unique_function_id,
        attributes: Attributes::default(),
    }
}

pub fn internal_generate_to_pretty_string_parameterless_function_for_type(
    generator: &mut ExpressionGenerator,
    id_gen: &mut InternalFunctionIdAllocator,
    module_path: &[String],
    ty: &TypeRef,
    resolved_node: &Node,
) -> InternalFunctionDefinition {
    let mut block_scope_to_use = GeneratedScope::new();

    // Create the "self" parameter
    let self_variable_ref = block_scope_to_use.create_parameter("self", ty, resolved_node);

    let self_param = create_expr_resolved(
        ExpressionKind::VariableAccess(self_variable_ref),
        ty.clone(),
        resolved_node,
    );

    // Start with indentation level 0
    let zero_indentation = create_expr_resolved(
        ExpressionKind::IntLiteral(0),
        generator.types.int(),
        resolved_node,
    );

    let body_expr = generate_to_pretty_string_for_type(
        generator,
        &mut block_scope_to_use,
        ty,
        &self_param,
        zero_indentation,
        resolved_node,
        false, // use_short_string_for_self (pretty format for top level)
        true,  // use_short_string_for_children (short format for children)
    );

    let unique_function_id = id_gen.alloc();

    block_scope_to_use.scope.finalize();

    InternalFunctionDefinition {
        body: body_expr,
        name: LocalIdentifier(resolved_node.clone()),
        assigned_name: "pretty_string".to_string(),
        associated_with_type: Option::from(ty.clone()),
        defined_in_module_path: module_path.to_vec(),
        signature: Signature {
            parameters: vec![
                TypeForParameter {
                    name: "self".to_string(),
                    resolved_type: ty.clone(),
                    is_mutable: false,
                    node: None,
                },
            ],
            return_type: generator.types.string(),
        },
        function_variables: block_scope_to_use.scope.clone(),
        program_unique_id: unique_function_id,
        attributes: Attributes::default(),
    }
}

#[allow(clippy::too_many_lines)]
fn generate_to_pretty_string_for_type(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    ty: &TypeRef,
    self_expression: &Expression,
    indentation_expression: Expression,
    node: &Node,
    use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();

    // Step 1: compact = self.to_string() or to_short_string()
    let compact_var = scope.create_local_variable("compact", &string_type, node);
    let compact_string = if use_short_string_for_self {
        call_to_short_string_method(generator, self_expression.clone(), node)
    } else {
        call_to_string_method(generator, self_expression.clone(), node)
    };
    let compact_def = create_expr_resolved(
        ExpressionKind::VariableDefinition(compact_var.clone(), Box::new(compact_string)),
        unit_type,
        node,
    );

    // Step 2: if compact.len() < 80 { return compact } else { ... }
    // Call StringLen intrinsic directly - much safer and faster!
    let compact_access = create_expr_resolved(
        ExpressionKind::VariableAccess(compact_var.clone()),
        string_type.clone(),
        node,
    );

    let length_expr = create_expr_resolved(
        ExpressionKind::IntrinsicCallEx(
            IntrinsicFunction::StringLen,
            vec![ArgumentExpression::Expression(compact_access)],
        ),
        int_type.clone(),
        node,
    );

    // Check if length < 80
    let threshold = create_expr_resolved(ExpressionKind::IntLiteral(80), int_type, node);
    let threshold_check = create_expr_resolved(
        ExpressionKind::BinaryOp(BinaryOperator {
            kind: BinaryOperatorKind::LessThan,
            left: Box::new(length_expr),
            right: Box::new(threshold),
            node: node.clone(),
        }),
        bool_type,
        node,
    );

    let compact_access = create_expr_resolved(
        ExpressionKind::VariableAccess(compact_var.clone()),
        string_type.clone(),
        node,
    );

    // Step 3: Generate multi-line format for specific types
    let multi_line_result = match &*ty.kind {
        TypeKind::MapStorage(_, _, _) | TypeKind::DynamicLengthMapView(_, _) => {
            generate_map_pretty_string(
                generator,
                scope,
                self_expression.clone(),
                indentation_expression,
                ty,
                node,
                use_short_string_for_children,
            )
        }
        TypeKind::FixedCapacityAndLengthArray(_, _)
        | TypeKind::SliceView(_)
        | TypeKind::DynamicLengthVecView(_)
        | TypeKind::VecStorage(_, _)
        | TypeKind::StackView(_)
        | TypeKind::QueueView(_)
        | TypeKind::StackStorage(_, _)
        | TypeKind::QueueStorage(_, _)
        | TypeKind::SparseView(_)
        | TypeKind::SparseStorage(_, _) => {
            // For sequences, just use the multi-line format directly
            // The threshold checking happens at the higher level
            generate_multi_line_sequence_format(
                generator,
                scope,
                self_expression.clone(),
                indentation_expression,
                ty,
                node,
                use_short_string_for_children,
            )
        }
        TypeKind::NamedStruct(_) | TypeKind::AnonymousStruct(_) | TypeKind::Tuple(_) => {
            generate_struct_pretty_string(
                generator,
                scope,
                self_expression,
                &indentation_expression,
                ty,
                node,
                use_short_string_for_children,
            )
        }
        _ => {
            // For other types, just return compact
            compact_access
        }
    };

    // Create another access to compact for the `if` expression
    let compact_access_for_if = create_expr_resolved(
        ExpressionKind::VariableAccess(compact_var),
        string_type.clone(),
        node,
    );

    // if threshold_check { compact } else { multi_line_result }
    let if_expr = create_expr_resolved(
        ExpressionKind::If(
            BooleanExpression {
                expression: Box::new(threshold_check),
            },
            Box::new(compact_access_for_if),
            Some(Box::new(multi_line_result)),
        ),
        string_type.clone(),
        node,
    );

    create_expr_resolved(
        ExpressionKind::Block(vec![compact_def, if_expr]),
        string_type,
        node,
    )
}

/// Helper to call a method on an expression with optional arguments
fn call_method_on_expression(
    generator: &mut ExpressionGenerator,
    self_expression: Expression,
    method_name: &str,
    arguments: Vec<ArgumentExpression>,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let ty = self_expression.ty.clone();

    let method_fn = generator
        .associated_impls
        .get_internal_member_function(&ty, method_name)
        .unwrap_or_else(|| panic!("{method_name}() function must exist for type: {ty}"));

    let function_ref = Function::Internal(method_fn.clone());
    let start_of_chain = StartOfChain {
        kind: StartOfChainKind::Expression(Box::from(self_expression)),
        node: node.clone(),
    };
    let postfix_call = Postfix {
        node: node.clone(),
        ty: string_type.clone(),
        kind: PostfixKind::MemberCall(FunctionRef::from(function_ref), arguments),
    };

    let lookup_kind = ExpressionKind::PostfixChain(start_of_chain, vec![postfix_call]);
    create_expr_resolved(lookup_kind, string_type, node)
}

fn call_to_string_method(
    generator: &mut ExpressionGenerator,
    self_expression: Expression,
    node: &Node,
) -> Expression {
    call_method_on_expression(generator, self_expression, "string", vec![], node)
}

/// Helper to call `to_short_string` method
fn call_to_short_string_method(
    generator: &mut ExpressionGenerator,
    self_expression: Expression,
    node: &Node,
) -> Expression {
    call_method_on_expression(generator, self_expression, "short_string", vec![], node)
}

fn call_to_pretty_string_method(
    generator: &mut ExpressionGenerator,
    self_expression: Expression,
    indentation_expression: Expression,
    node: &Node,
) -> Expression {
    call_method_on_expression(
        generator,
        self_expression,
        "pretty_string_with_indent",
        vec![ArgumentExpression::Expression(indentation_expression)],
        node,
    )
}

fn generate_indentation_string(
    generator: &mut ExpressionGenerator,
    _scope: &mut GeneratedScope,
    indentation_var: &VariableRef,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();

    // Use string repeat: "  " * indentation
    let spaces_literal = create_expr_resolved(
        ExpressionKind::StringLiteral("  ".to_string()),
        string_type.clone(),
        node,
    );

    let indentation_access = create_expr_resolved(
        ExpressionKind::VariableAccess(indentation_var.clone()),
        int_type,
        node,
    );

    // Use the Multiply operator for string repeat
    create_expr_resolved(
        ExpressionKind::BinaryOp(BinaryOperator {
            kind: BinaryOperatorKind::Multiply,
            left: Box::new(spaces_literal),
            right: Box::new(indentation_access),
            node: node.clone(),
        }),
        string_type,
        node,
    )
}

#[allow(clippy::too_many_lines)]
fn generate_map_pretty_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    indentation_expression: Expression,
    ty: &TypeRef,
    node: &Node,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();

    let (TypeKind::MapStorage(key_type, value_type, _)
    | TypeKind::DynamicLengthMapView(key_type, value_type)) = &*ty.kind
    else {
        panic!("Expected map type")
    };

    // Start with just the opening bracket (no newline yet)
    let (result_var, result_def) = {
        let var = scope.create_local_mut_variable("result", &string_type, node);
        let initial = create_expr_resolved(
            ExpressionKind::StringLiteral("[|".to_string()),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(initial)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // Track whether we added any items (to detect empty maps)
    let (has_items_var, has_items_def) = {
        let var = scope.create_local_mut_variable("has_items", &generator.types.bool(), node);
        let false_expr = create_expr_resolved(
            ExpressionKind::BoolLiteral(false),
            generator.types.bool(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(false_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // Store original indentation in a variable
    let (indentation_var, indentation_var_def) = {
        let var = scope.create_local_variable("indentation", &int_type, node);
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(indentation_expression)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // next_indentation = indentation + 1
    let (next_indent_var, next_indent_def) = {
        let var = scope.create_local_variable("next_indentation", &int_type, node);
        let one = create_expr_resolved(ExpressionKind::IntLiteral(1), int_type.clone(), node);
        let indentation_access = create_expr_resolved(
            ExpressionKind::VariableAccess(indentation_var),
            int_type.clone(),
            node,
        );
        let add_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(indentation_access),
                right: Box::new(one),
                node: node.clone(),
            }),
            int_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(add_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // for key, value in self { ... }
    let for_loop = {
        let key_var = scope.create_local_variable_param_like("key", key_type, node);
        let value_var = scope.create_local_variable_param_like("value", value_type, node);

        // Set has_items = true (we found at least one item)
        let set_has_items = {
            let true_expr = create_expr_resolved(
                ExpressionKind::BoolLiteral(true),
                generator.types.bool(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(has_items_var.clone(), Box::new(true_expr)),
                unit_type.clone(),
                node,
            )
        };

        // Add the key-value line: result += "\n" + indentation_spaces + key + " : " + value
        let add_key_value_line = {
            let key_access = create_expr_resolved(
                ExpressionKind::VariableAccess(key_var.clone()),
                key_type.clone(),
                node,
            );
            let value_access = create_expr_resolved(
                ExpressionKind::VariableAccess(value_var.clone()),
                value_type.clone(),
                node,
            );
            let next_indent_access = create_expr_resolved(
                ExpressionKind::VariableAccess(next_indent_var.clone()),
                int_type.clone(),
                node,
            );

            // Call appropriate string method based on use_short_string_for_children
            let key_pretty_str = if use_short_string_for_children {
                call_to_short_string_method(generator, key_access, node)
            } else {
                call_to_pretty_string_method(
                    generator,
                    key_access,
                    next_indent_access.clone(),
                    node,
                )
            };
            let value_pretty_str = if use_short_string_for_children {
                call_to_short_string_method(generator, value_access, node)
            } else {
                call_to_pretty_string_method(generator, value_access, next_indent_access, node)
            };

            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );

            // Generate indentation string for next_indentation level
            let indent_spaces =
                generate_indentation_string(generator, scope, &next_indent_var, node);

            let colon_str = create_expr_resolved(
                ExpressionKind::StringLiteral(" : ".to_string()),
                string_type.clone(),
                node,
            );
            let newline_str = create_expr_resolved(
                ExpressionKind::StringLiteral("\n".to_string()),
                string_type.clone(),
                node,
            );

            // result + "\n" + indent_spaces + key_pretty + " : " + value_pretty
            let temp1 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(newline_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let temp2 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp1),
                    right: Box::new(indent_spaces),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let temp3 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp2),
                    right: Box::new(key_pretty_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let temp4 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp3),
                    right: Box::new(colon_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let final_str = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp4),
                    right: Box::new(value_pretty_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_str)),
                unit_type.clone(),
                node,
            )
        };

        let for_body = create_expr_resolved(
            ExpressionKind::Block(vec![set_has_items, add_key_value_line]),
            unit_type.clone(),
            node,
        );

        let iterable = Iterable {
            key_type: Some(key_type.clone()),
            value_type: value_type.clone(),
            resolved_expression: Box::new(self_expression),
        };

        let for_pattern = ForPattern::Pair(key_var, value_var);
        create_expr_resolved(
            ExpressionKind::ForLoop(for_pattern, iterable, Box::new(for_body)),
            unit_type.clone(),
            node,
        )
    };

    // Handle empty vs non-empty maps
    let handle_closing = {
        let has_items_access = create_expr_resolved(
            ExpressionKind::VariableAccess(has_items_var),
            generator.types.bool(),
            node,
        );

        // If map is empty: result = "[:]"
        let empty_case = {
            let empty_map_str = create_expr_resolved(
                ExpressionKind::StringLiteral("[:]".to_string()),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(empty_map_str)),
                unit_type.clone(),
                node,
            )
        };

        // If map is not empty: result += "\n" + indentation_spaces + "|]"
        let non_empty_case = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let closing_brace = create_expr_resolved(
                ExpressionKind::StringLiteral("|]".to_string()),
                string_type.clone(),
                node,
            );
            let concat_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(closing_brace),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
                unit_type.clone(),
                node,
            )
        };

        create_expr_resolved(
            ExpressionKind::If(
                BooleanExpression {
                    expression: Box::new(has_items_access),
                },
                Box::new(non_empty_case),
                Some(Box::new(empty_case)),
            ),
            unit_type,
            node,
        )
    };

    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    create_expr_resolved(
        ExpressionKind::Block(vec![
            result_def,
            has_items_def,
            indentation_var_def,
            next_indent_def,
            for_loop,
            handle_closing,
            result_access,
        ]),
        string_type,
        node,
    )
}


#[allow(clippy::too_many_lines)]
fn generate_multi_line_sequence_format(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    indentation_expression: Expression,
    ty: &TypeRef,
    node: &Node,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();

    // Get element type from the sequence type
    let (TypeKind::FixedCapacityAndLengthArray(element_type, _)
    | TypeKind::SliceView(element_type)
    | TypeKind::DynamicLengthVecView(element_type)
    | TypeKind::VecStorage(element_type, _)
    | TypeKind::StackView(element_type)
    | TypeKind::QueueView(element_type)
    | TypeKind::StackStorage(element_type, _)
    | TypeKind::QueueStorage(element_type, _)
    | TypeKind::SparseView(element_type)
    | TypeKind::SparseStorage(element_type, _)) = &*ty.kind
    else {
        panic!("Expected sequence type")
    };

    // Start with "[\n"
    let (result_var, result_def) = {
        let var = scope.create_local_mut_variable("result", &string_type, node);
        let initial = create_expr_resolved(
            ExpressionKind::StringLiteral("[\n".to_string()),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(initial)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // next_indentation = indentation + 1
    let (next_indent_var, next_indent_def) = {
        let var = scope.create_local_variable("next_indentation", &int_type, node);
        let one = create_expr_resolved(ExpressionKind::IntLiteral(1), int_type.clone(), node);
        let add_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(indentation_expression.clone()),
                right: Box::new(one),
                node: node.clone(),
            }),
            int_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(add_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // for element in self { add indentation + element.to_pretty_string(next_indentation) + ",\n" }
    let for_loop = {
        let element_var = scope.create_local_variable_param_like("element", element_type, node);

        // Add element line: result += indentation_spaces + element.to_pretty_string(next_indentation) + ",\n"
        let add_element_line = {
            let element_access = create_expr_resolved(
                ExpressionKind::VariableAccess(element_var.clone()),
                element_type.clone(),
                node,
            );
            let next_indent_access = create_expr_resolved(
                ExpressionKind::VariableAccess(next_indent_var.clone()),
                int_type.clone(),
                node,
            );

            let element_pretty_str = if use_short_string_for_children {
                call_to_short_string_method(generator, element_access, node)
            } else {
                call_to_pretty_string_method(generator, element_access, next_indent_access, node)
            };

            let result_access = create_var_access(&result_var, &string_type, node);

            // Generate indentation string for next_indentation level
            let indent_spaces =
                generate_indentation_string(generator, scope, &next_indent_var, node);

            let comma_newline_str = create_string_literal(",\n", &string_type, node);

            // result + indent_spaces + element_pretty + ",\n"
            let temp1 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(indent_spaces),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let temp2 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp1),
                    right: Box::new(element_pretty_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let final_str = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp2),
                    right: Box::new(comma_newline_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_str)),
                unit_type.clone(),
                node,
            )
        };

        let for_body = create_expr_resolved(
            ExpressionKind::Block(vec![add_element_line]),
            unit_type.clone(),
            node,
        );

        let iterable = Iterable {
            key_type: Some(int_type.clone()),
            value_type: element_type.clone(),
            resolved_expression: Box::new(self_expression),
        };

        let for_pattern = ForPattern::Single(element_var);
        create_expr_resolved(
            ExpressionKind::ForLoop(for_pattern, iterable, Box::new(for_body)),
            unit_type.clone(),
            node,
        )
    };

    // Store original indentation in a variable
    let (indentation_var, indentation_var_def) = {
        let var = scope.create_local_variable("indentation", &int_type, node);
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(indentation_expression)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // Add final indentation and closing bracket
    let add_closing = {
        let result_access = create_expr_resolved(
            ExpressionKind::VariableAccess(result_var.clone()),
            string_type.clone(),
            node,
        );

        let final_indent_spaces =
            generate_indentation_string(generator, scope, &indentation_var, node);
        let closing_str = create_expr_resolved(
            ExpressionKind::StringLiteral("]".to_string()),
            string_type.clone(),
            node,
        );

        // result + final_indent_spaces + "]"
        let temp1 = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_access),
                right: Box::new(final_indent_spaces),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        );
        let final_result = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(temp1),
                right: Box::new(closing_str),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        );

        create_expr_resolved(
            ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_result)),
            unit_type,
            node,
        )
    };

    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    create_expr_resolved(
        ExpressionKind::Block(vec![
            result_def,
            next_indent_def,
            indentation_var_def,
            for_loop,
            add_closing,
            result_access,
        ]),
        string_type,
        node,
    )
}

fn generate_struct_pretty_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: &Expression,
    indentation_expression: &Expression,
    ty: &TypeRef,
    node: &Node,
    use_short_string_for_children: bool,
) -> Expression {
    match &*ty.kind {
        TypeKind::NamedStruct(named_struct) => generate_named_struct_pretty_string(
            generator,
            scope,
            self_expression,
            indentation_expression,
            named_struct,
            node,
            false, // use_short_string_for_self (always false for struct pretty printing)
            use_short_string_for_children,
        ),
        TypeKind::AnonymousStruct(_) => generate_anon_struct_pretty_string(
            generator,
            scope,
            self_expression,
            indentation_expression,
            ty,
            node,
            false, // use_short_string_for_self
            use_short_string_for_children, // use_short_string_for_children
        ),
        TypeKind::Tuple(tuple_types) => generate_tuple_pretty_string(
            generator,
            scope,
            self_expression,
            indentation_expression,
            tuple_types,
            node,
            false, // use_short_string_for_self
            use_short_string_for_children, // use_short_string_for_children
        ),
        _ => {
            // Fallback to compact format
            call_to_string_method(generator, self_expression.clone(), node)
        }
    }
}

fn generate_add_indentation_to_result(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    result_var: &VariableRef,
    indentation_expr: &Expression,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let unit_type = generator.types.unit();

    // Get the current result
    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var.clone()),
        string_type.clone(),
        node,
    );

    // Generate indentation string based on the expression type
    let indentation_spaces = if let ExpressionKind::VariableAccess(var_ref) = &indentation_expr.kind {
        // Direct variable access - use the optimized path
        generate_indentation_string(generator, scope, var_ref, node)
    } else {
        // For other expressions (like IntLiteral(0)), use string repeat directly
        let spaces_literal = create_expr_resolved(
            ExpressionKind::StringLiteral("  ".to_string()),
            string_type.clone(),
            node,
        );

        // Use the Multiply operator for string repeat
        create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Multiply,
                left: Box::new(spaces_literal),
                right: Box::new(indentation_expr.clone()),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        )
    };

    // Concatenate result + indentation_spaces
    let concat = concat_expressions(result_access, indentation_spaces, &string_type, node);

    create_var_reassignment(result_var, concat, &unit_type, node)
}

#[allow(clippy::too_many_lines)]
fn generate_named_struct_pretty_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: &Expression,
    indentation_expression: &Expression,
    named_struct: &NamedStructType,
    node: &Node,
    use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();

    // Start with "{\n" or "StructName {\n" depending on use_short_string_for_self
    let (result_var, result_def) = {
        let var = scope.create_local_mut_variable("result", &string_type, node);
        let struct_prefix = if use_short_string_for_self {
            "{\n".to_string()
        } else {
            format!("{} {{\n", named_struct.assigned_name)
        };
        let initial = create_expr_resolved(
            ExpressionKind::StringLiteral(struct_prefix),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(initial)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // next_indentation = indentation + 1
    let (next_indent_var, next_indent_def) = {
        let var = scope.create_local_variable("next_indentation", &int_type, node);
        let one = create_expr_resolved(ExpressionKind::IntLiteral(1), int_type.clone(), node);
        let add_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(indentation_expression.clone()),
                right: Box::new(one),
                node: node.clone(),
            }),
            int_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(add_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // Generate field additions
    let mut field_assignments = Vec::new();
    let TypeKind::AnonymousStruct(anon_struct) = &*named_struct.anon_struct_type.kind else {
        panic!("Named struct should contain anonymous struct");
    };

    for (field_index, (field_name, field_type)) in
        anon_struct.field_name_sorted_fields.iter().enumerate()
    {
        // Add indentation for field (next_indent_var level)
        let next_indent_access = create_var_access(&next_indent_var, &int_type, node);
        let add_indent = generate_add_indentation_to_result(
            generator,
            scope,
            &result_var,
            &next_indent_access,
            node,
        );

        // Add field name: "field_name: "
        let add_field_name = {
            let result_access = create_var_access(&result_var, &string_type, node);
            let field_name_str =
                create_string_literal(&format!("{field_name}: "), &string_type, node);
            let concat = concat_expressions(result_access, field_name_str, &string_type, node);
            create_var_reassignment(&result_var, concat, &unit_type, node)
        };

        // Get field value and call to_pretty_string on it
        let add_field_value = {
            let postfix_kind =
                PostfixKind::StructField(named_struct.anon_struct_type.clone(), field_index);
            let postfix_lookup_field = Postfix {
                node: node.clone(),
                ty: field_type.field_type.clone(),
                kind: postfix_kind,
            };

            let start_of_chain = StartOfChain {
                kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                node: node.clone(),
            };
            let field_access = create_expr_resolved(
                ExpressionKind::PostfixChain(start_of_chain, vec![postfix_lookup_field]),
                field_type.field_type.clone(),
                node,
            );

            let next_indent_access = create_var_access(&next_indent_var, &int_type, node);

            let field_pretty_string = if use_short_string_for_children {
                call_to_short_string_method(generator, field_access, node)
            } else {
                call_to_pretty_string_method(generator, field_access, next_indent_access, node)
            };

            let result_access = create_var_access(&result_var, &string_type, node);

            // result + field_pretty_string + ",\n"
            let temp1 = concat_expressions(result_access, field_pretty_string, &string_type, node);
            let comma_newline = create_string_literal(",\n", &string_type, node);
            let final_str = concat_expressions(temp1, comma_newline, &string_type, node);

            create_var_reassignment(&result_var, final_str, &unit_type, node)
        };

        let field_block = create_expr_resolved(
            ExpressionKind::Block(vec![add_indent, add_field_name, add_field_value]),
            unit_type.clone(),
            node,
        );
        field_assignments.push(field_block);
    }

    // Add final indentation and closing brace
    let add_closing = {
        let add_indent_spaces = generate_add_indentation_to_result(
            generator,
            scope,
            &result_var,
            indentation_expression,
            node,
        );

        let result_access = create_expr_resolved(
            ExpressionKind::VariableAccess(result_var.clone()),
            string_type.clone(),
            node,
        );
        let closing = create_expr_resolved(
            ExpressionKind::StringLiteral("}".to_string()),
            string_type.clone(),
            node,
        );
        let final_result = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_access),
                right: Box::new(closing),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        );
        let close_assignment = create_expr_resolved(
            ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_result)),
            unit_type.clone(),
            node,
        );

        create_expr_resolved(
            ExpressionKind::Block(vec![add_indent_spaces, close_assignment]),
            unit_type,
            node,
        )
    };

    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    let mut block_statements = vec![result_def, next_indent_def];
    block_statements.extend(field_assignments);
    block_statements.push(add_closing);
    block_statements.push(result_access);

    create_expr_resolved(ExpressionKind::Block(block_statements), string_type, node)
}

#[allow(clippy::too_many_lines)]
fn generate_anon_struct_pretty_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: &Expression,
    indentation_expression: &Expression,
    ty: &TypeRef,
    node: &Node,
    _use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();

    // Start with "{\n"
    let (result_var, result_def) = {
        let var = scope.create_local_mut_variable("result", &string_type, node);
        let initial = create_expr_resolved(
            ExpressionKind::StringLiteral("{\n".to_string()),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(initial)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // next_indentation = indentation + 1
    let (next_indent_var, next_indent_def) = {
        let var = scope.create_local_variable("next_indentation", &int_type, node);
        let one = create_expr_resolved(ExpressionKind::IntLiteral(1), int_type.clone(), node);
        let add_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(indentation_expression.clone()),
                right: Box::new(one),
                node: node.clone(),
            }),
            int_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(add_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // Generate field additions
    let mut field_assignments = Vec::new();
    let TypeKind::AnonymousStruct(anon_struct) = &*ty.kind else {
        panic!("Expected anonymous struct");
    };

    for (field_index, (field_name, field_type)) in
        anon_struct.field_name_sorted_fields.iter().enumerate()
    {
        // Add indentation for field (next_indent_var level)
        let next_indent_access = create_var_access(&next_indent_var, &int_type, node);
        let add_indent = generate_add_indentation_to_result(
            generator,
            scope,
            &result_var,
            &next_indent_access,
            node,
        );

        // Add field name: "field_name: "
        let add_field_name = {
            let result_access = create_var_access(&result_var, &string_type, node);
            let field_name_str =
                create_string_literal(&format!("{field_name}: "), &string_type, node);
            let concat = concat_expressions(result_access, field_name_str, &string_type, node);
            create_var_reassignment(&result_var, concat, &unit_type, node)
        };

        // Get field value and call to_pretty_string on it
        let add_field_value = {
            let postfix_kind = PostfixKind::StructField(ty.clone(), field_index);
            let postfix_lookup_field = Postfix {
                node: node.clone(),
                ty: field_type.field_type.clone(),
                kind: postfix_kind,
            };

            let start_of_chain = StartOfChain {
                kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                node: node.clone(),
            };
            let field_access = create_expr_resolved(
                ExpressionKind::PostfixChain(start_of_chain, vec![postfix_lookup_field]),
                field_type.field_type.clone(),
                node,
            );

            let next_indent_access = create_var_access(&next_indent_var, &int_type, node);

            let field_pretty_string = if use_short_string_for_children {
                call_to_short_string_method(generator, field_access, node)
            } else {
                call_to_pretty_string_method(generator, field_access, next_indent_access, node)
            };

            let result_access = create_var_access(&result_var, &string_type, node);

            // result + field_pretty_string + ",\n"
            let temp1 = concat_expressions(result_access, field_pretty_string, &string_type, node);
            let comma_newline = create_string_literal(",\n", &string_type, node);
            let final_str = concat_expressions(temp1, comma_newline, &string_type, node);

            create_var_reassignment(&result_var, final_str, &unit_type, node)
        };

        let field_block = create_expr_resolved(
            ExpressionKind::Block(vec![add_indent, add_field_name, add_field_value]),
            unit_type.clone(),
            node,
        );
        field_assignments.push(field_block);
    }

    // Add final indentation and closing brace
    let add_closing = {
        let add_indent_spaces = generate_add_indentation_to_result(
            generator,
            scope,
            &result_var,
            indentation_expression,
            node,
        );

        let result_access = create_expr_resolved(
            ExpressionKind::VariableAccess(result_var.clone()),
            string_type.clone(),
            node,
        );
        let closing = create_expr_resolved(
            ExpressionKind::StringLiteral("}".to_string()),
            string_type.clone(),
            node,
        );
        let final_result = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_access),
                right: Box::new(closing),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        );
        let close_assignment = create_expr_resolved(
            ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_result)),
            unit_type.clone(),
            node,
        );

        create_expr_resolved(
            ExpressionKind::Block(vec![add_indent_spaces, close_assignment]),
            unit_type,
            node,
        )
    };

    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    let mut block_statements = vec![result_def, next_indent_def];
    block_statements.extend(field_assignments);
    block_statements.push(add_closing);
    block_statements.push(result_access);

    create_expr_resolved(ExpressionKind::Block(block_statements), string_type, node)
}

#[allow(clippy::too_many_lines)]
fn generate_tuple_pretty_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: &Expression,
    indentation_expression: &Expression,
    tuple_types: &[TypeRef],
    node: &Node,
    _use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();

    // Start with "(\n"
    let (result_var, result_def) = {
        let var = scope.create_local_mut_variable("result", &string_type, node);
        let initial = create_expr_resolved(
            ExpressionKind::StringLiteral("(\n".to_string()),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(initial)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // next_indentation = indentation + 1
    let (next_indent_var, next_indent_def) = {
        let var = scope.create_local_variable("next_indentation", &int_type, node);
        let one = create_expr_resolved(ExpressionKind::IntLiteral(1), int_type.clone(), node);
        let add_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(indentation_expression.clone()),
                right: Box::new(one),
                node: node.clone(),
            }),
            int_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(add_expr)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // Generate element additions
    let mut element_assignments = Vec::new();
    for (element_index, element_type) in tuple_types.iter().enumerate() {
        // Add indentation for element (next_indent_var level)
        let next_indent_access = create_expr_resolved(
            ExpressionKind::VariableAccess(next_indent_var.clone()),
            int_type.clone(),
            node,
        );
        let add_indent = generate_add_indentation_to_result(
            generator,
            scope,
            &result_var,
            &next_indent_access,
            node,
        );

        // Get tuple element and call to_pretty_string on it
        let add_element_value = {
            let start_of_chain = StartOfChain {
                kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                node: node.clone(),
            };
            let postfix = Postfix {
                node: node.clone(),
                ty: element_type.clone(),
                kind: PostfixKind::StructField(self_expression.ty.clone(), element_index),
            };
            let element_access = create_expr_resolved(
                ExpressionKind::PostfixChain(start_of_chain, vec![postfix]),
                element_type.clone(),
                node,
            );

            let next_indent_access = create_var_access(&next_indent_var, &int_type, node);

            let element_pretty_string = if use_short_string_for_children {
                call_to_short_string_method(generator, element_access, node)
            } else {
                call_to_pretty_string_method(generator, element_access, next_indent_access, node)
            };

            let result_access = create_var_access(&result_var, &string_type, node);

            // result + element_pretty_string + ",\n"
            let temp1 = concat_expressions(result_access, element_pretty_string, &string_type, node);
            let comma_newline = create_string_literal(",\n", &string_type, node);
            let final_str = concat_expressions(temp1, comma_newline, &string_type, node);

            create_var_reassignment(&result_var, final_str, &unit_type, node)
        };

        let element_block = create_expr_resolved(
            ExpressionKind::Block(vec![add_indent, add_element_value]),
            unit_type.clone(),
            node,
        );
        element_assignments.push(element_block);
    }

    // Add final indentation and closing paren
    let add_closing = {
        let add_indent_spaces = generate_add_indentation_to_result(
            generator,
            scope,
            &result_var,
            indentation_expression,
            node,
        );

        let result_access = create_expr_resolved(
            ExpressionKind::VariableAccess(result_var.clone()),
            string_type.clone(),
            node,
        );
        let closing = create_expr_resolved(
            ExpressionKind::StringLiteral(")".to_string()),
            string_type.clone(),
            node,
        );
        let final_result = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_access),
                right: Box::new(closing),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        );
        let close_assignment = create_expr_resolved(
            ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_result)),
            unit_type.clone(),
            node,
        );

        create_expr_resolved(
            ExpressionKind::Block(vec![add_indent_spaces, close_assignment]),
            unit_type,
            node,
        )
    };

    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    let mut block_statements = vec![result_def, next_indent_def];
    block_statements.extend(element_assignments);
    block_statements.push(add_closing);
    block_statements.push(result_access);

    create_expr_resolved(ExpressionKind::Block(block_statements), string_type, node)
}

#[allow(clippy::too_many_lines)]
fn generate_to_string_for_grid(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    element_type: &TypeRef,
    node: &Node,
    _use_short_string_for_self: bool,
    use_short_string_for_children: bool,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();

    // Create a variable for the total string result
    let (result_var, result_var_def) = {
        let var = scope.create_local_mut_variable("total", &string_type, node);
        let empty_string = create_expr_resolved(
            ExpressionKind::StringLiteral(String::new()),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(empty_string)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // Get grid height using intrinsic call
    let (height_var, height_var_def) = {
        let var = scope.create_local_variable("height", &int_type, node);

        // Use GridHeight intrinsic instead of height() method
        let height_expr = create_expr_resolved(
            ExpressionKind::IntrinsicCallEx(
                IntrinsicFunction::GridHeight,
                vec![ArgumentExpression::Expression(self_expression.clone())],
            ),
            int_type.clone(),
            node,
        );

        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(height_expr)),
            unit_type.clone(),
            node,
        );

        (var, def)
    };

    // Get grid width using intrinsic call
    let (width_var, width_var_def) = {
        let var = scope.create_local_variable("width", &int_type, node);

        // Use GridWidth intrinsic instead of width() method
        let width_expr = create_expr_resolved(
            ExpressionKind::IntrinsicCallEx(
                IntrinsicFunction::GridWidth,
                vec![ArgumentExpression::Expression(self_expression.clone())],
            ),
            int_type.clone(),
            node,
        );

        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(width_expr)),
            unit_type.clone(),
            node,
        );

        (var, def)
    };

    // Create outer loop for rows (y coordinate)
    let y_loop = {
        // Create y counter variable
        let y_var = scope.create_local_variable("y", &int_type, node);

        // Create inner loop for columns (x coordinate)
        let x_loop = {
            // Create x counter variable
            let x_var = scope.create_local_variable("x", &int_type, node);

            // if x > 0 { total += ", " }
            let if_comma = {
                let x_access = create_expr_resolved(
                    ExpressionKind::VariableAccess(x_var.clone()),
                    int_type.clone(),
                    node,
                );
                let zero =
                    create_expr_resolved(ExpressionKind::IntLiteral(0), int_type.clone(), node);

                let condition = create_expr_resolved(
                    ExpressionKind::BinaryOp(BinaryOperator {
                        kind: BinaryOperatorKind::GreaterThan,
                        left: Box::new(x_access),
                        right: Box::new(zero),
                        node: node.clone(),
                    }),
                    bool_type,
                    node,
                );

                let update_result =
                    append_literal_to_result(&result_var, ", ", &string_type, &unit_type, node);

                create_expr_resolved(
                    ExpressionKind::If(
                        BooleanExpression {
                            expression: Box::new(condition),
                        },
                        Box::new(update_result),
                        None,
                    ),
                    unit_type.clone(),
                    node,
                )
            };

            // Calculate inverse_y = height - y - 1
            let inverse_y = {
                let height_access = create_expr_resolved(
                    ExpressionKind::VariableAccess(height_var.clone()),
                    int_type.clone(),
                    node,
                );
                let y_access = create_expr_resolved(
                    ExpressionKind::VariableAccess(y_var.clone()),
                    int_type.clone(),
                    node,
                );
                let one =
                    create_expr_resolved(ExpressionKind::IntLiteral(1), int_type.clone(), node);

                // height - y
                let height_minus_y = create_expr_resolved(
                    ExpressionKind::BinaryOp(BinaryOperator {
                        kind: BinaryOperatorKind::Subtract,
                        left: Box::new(height_access),
                        right: Box::new(y_access),
                        node: node.clone(),
                    }),
                    int_type.clone(),
                    node,
                );

                // (height - y) - 1
                create_expr_resolved(
                    ExpressionKind::BinaryOp(BinaryOperator {
                        kind: BinaryOperatorKind::Subtract,
                        left: Box::new(height_minus_y),
                        right: Box::new(one),
                        node: node.clone(),
                    }),
                    int_type.clone(),
                    node,
                )
            };

            // Access grid element using get() function call
            let grid_element = {
                // Use GridGet intrinsic instead of get() method
                let x_access = create_expr_resolved(
                    ExpressionKind::VariableAccess(x_var.clone()),
                    int_type.clone(),
                    node,
                );

                // Create the intrinsic call with grid, x and inverse_y arguments
                create_expr_resolved(
                    ExpressionKind::IntrinsicCallEx(
                        IntrinsicFunction::GridGet,
                        vec![
                            ArgumentExpression::Expression(self_expression),
                            ArgumentExpression::Expression(x_access),
                            ArgumentExpression::Expression(inverse_y),
                        ],
                    ),
                    element_type.clone(),
                    node,
                )
            };

            // Get string representation of the grid element
            let element_string = create_string_representation_of_expression(
                generator,
                grid_element,
                node,
                use_short_string_for_children,
            );

            // Append element string to result
            let append_element = {
                let result_access = create_expr_resolved(
                    ExpressionKind::VariableAccess(result_var.clone()),
                    string_type.clone(),
                    node,
                );

                let concat_expr = create_expr_resolved(
                    ExpressionKind::BinaryOp(BinaryOperator {
                        kind: BinaryOperatorKind::Add,
                        left: Box::new(result_access),
                        right: Box::new(element_string),
                        node: node.clone(),
                    }),
                    string_type.clone(),
                    node,
                );

                create_expr_resolved(
                    ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
                    unit_type.clone(),
                    node,
                )
            };

            // Combine operations for the inner loop body
            let inner_loop_body = create_expr_resolved(
                ExpressionKind::Block(vec![if_comma, append_element]),
                unit_type.clone(),
                node,
            );

            // Create inner for loop from 0 to width
            let zero = create_expr_resolved(ExpressionKind::IntLiteral(0), int_type.clone(), node);

            let width_access = create_expr_resolved(
                ExpressionKind::VariableAccess(width_var),
                int_type.clone(),
                node,
            );

            // Create an iterable for `0..width` using Range struct
            let iterable = Iterable {
                key_type: None,
                value_type: int_type.clone(),
                resolved_expression: Box::new(create_expr_resolved(
                    ExpressionKind::IntrinsicCallEx(
                        IntrinsicFunction::RangeInit,
                        vec![
                            ArgumentExpression::Expression(zero),
                            ArgumentExpression::Expression(width_access),
                            ArgumentExpression::Expression(create_expr_resolved(
                                ExpressionKind::BoolLiteral(false),
                                generator.types.bool(),
                                node,
                            )),
                        ],
                    ),
                    generator.types.range(int_type.clone()),
                    node,
                )),
            };

            // Create the for loop
            create_expr_resolved(
                ExpressionKind::ForLoop(
                    ForPattern::Single(x_var),
                    iterable,
                    Box::new(inner_loop_body),
                ),
                unit_type.clone(),
                node,
            )
        };

        // Add newline after each row
        let add_newline = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );

            let newline_str = create_expr_resolved(
                ExpressionKind::StringLiteral("\n".to_string()),
                string_type.clone(),
                node,
            );

            let concat_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(newline_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat_expr)),
                unit_type.clone(),
                node,
            )
        };

        // Combine x_loop and newline addition for the outer loop body
        let outer_loop_body = create_expr_resolved(
            ExpressionKind::Block(vec![x_loop, add_newline]),
            unit_type.clone(),
            node,
        );

        // Create outer for loop from 0 to height
        let zero = create_expr_resolved(ExpressionKind::IntLiteral(0), int_type.clone(), node);

        let height_access = create_expr_resolved(
            ExpressionKind::VariableAccess(height_var),
            int_type.clone(),
            node,
        );

        // Create an iterable for 0..height using Range struct
        let iterable = Iterable {
            key_type: None,
            value_type: int_type.clone(),
            resolved_expression: Box::new(create_expr_resolved(
                ExpressionKind::IntrinsicCallEx(
                    IntrinsicFunction::RangeInit,
                    vec![
                        ArgumentExpression::Expression(zero),
                        ArgumentExpression::Expression(height_access),
                        ArgumentExpression::Expression(create_expr_resolved(
                            ExpressionKind::BoolLiteral(false),
                            generator.types.bool(),
                            node,
                        )),
                    ],
                ),
                generator.types.range(int_type.clone()),
                node,
            )),
        };

        // Create the for loop
        create_expr_resolved(
            ExpressionKind::ForLoop(
                ForPattern::Single(y_var),
                iterable,
                Box::new(outer_loop_body),
            ),
            unit_type,
            node,
        )
    };

    // Return the result string
    let result_access = create_expr_resolved(
        ExpressionKind::VariableAccess(result_var),
        string_type.clone(),
        node,
    );

    // Put everything together in a block
    create_expr_resolved(
        ExpressionKind::Block(vec![
            result_var_def,
            height_var_def,
            width_var_def,
            y_loop,
            result_access,
        ]),
        string_type,
        node,
    )
}
