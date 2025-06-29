///! For `to_string()` and `to_short_string()` lowering
/// Should not use the normal analyzer helpers, since that can interfere with the normal analyze.
use source_map_node::Node;
use swamp_attributes::Attributes;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    ArgumentExpression, AssociatedImpls, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    Expression, ExpressionKind, ForPattern, Function, FunctionRef, InternalFunctionDefinition,
    InternalFunctionIdAllocator, Iterable, LocalIdentifier, Match, MatchArm, NormalPattern,
    Pattern, Postfix, PostfixKind, StartOfChain, StartOfChainKind, UnaryOperator,
    UnaryOperatorKind, Variable, VariableRef, VariableScopes, VariableType, WhenBinding,
};
use swamp_types::prelude::{EnumType, NamedStructType, Signature, TypeCache, TypeForParameter};
use swamp_types::{TypeKind, TypeRef};

fn generate_to_string_for_named_struct(
    generator: &mut ExpressionGenerator,
    named: &NamedStructType,
    self_expression: &Expression,
) -> Expression {
    let node = self_expression.node.clone();
    let struct_name_string_kind =
        ExpressionKind::StringLiteral(format!("{} ", named.assigned_name.clone()));
    let string_type = generator.types.string();
    let struct_name_string_expr = create_expr_resolved(struct_name_string_kind, string_type, &node);
    let anon_struct_string_expr =
        generate_to_string_for_anon_struct(generator, &named.anon_struct_type, self_expression);

    let concat_kind = BinaryOperator {
        kind: BinaryOperatorKind::Add,
        left: Box::new(struct_name_string_expr),
        right: Box::new(anon_struct_string_expr),
        node: node.clone(),
    };

    let string_type = generator.types.string();
    create_expr_resolved(ExpressionKind::BinaryOp(concat_kind), string_type, &node)
}

fn generate_to_short_string_for_named_struct(
    type_cache: &mut ExpressionGenerator,
    named: &NamedStructType,
    self_expression: &Expression,
) -> Expression {
    // For to_short_string, we skip the struct name and just show it as an anonymous struct
    generate_to_short_string_for_anon_struct(type_cache, &named.anon_struct_type, self_expression)
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
    anonymous_struct_type_ref: &TypeRef,
    self_expression: &Expression,
) -> Expression {
    let node = &self_expression.node;
    let string_type = generator.types.string();

    // Create opening brace string
    let opening_kind = ExpressionKind::StringLiteral("{ ".to_string());
    let mut result_expr = create_expr_resolved(opening_kind, string_type.clone(), node);

    let TypeKind::AnonymousStruct(anonymous_struct_type) = &*anonymous_struct_type_ref.kind else {
        panic!("internal error")
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
            let separator_expr = create_expr_resolved(separator_kind, string_type.clone(), node);

            let concat_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_expr),
                right: Box::new(separator_expr),
                node: node.clone(),
            };
            result_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(concat_kind),
                string_type.clone(),
                node,
            );
        }

        let field_name_kind = ExpressionKind::StringLiteral(format!("{field_name}: "));
        let field_name_expr = create_expr_resolved(field_name_kind, string_type.clone(), node);

        let concat_name_kind = BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(result_expr),
            right: Box::new(field_name_expr),
            node: node.clone(),
        };

        result_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(concat_name_kind),
            string_type.clone(),
            node,
        );

        // Get field value from the struct
        let postfix_kind = PostfixKind::StructField(anonymous_struct_type_ref.clone(), field_index);
        let postfix_lookup_field_in_self = Postfix {
            node: node.clone(),
            ty: field_type.field_type.clone(),
            kind: postfix_kind,
        };

        let field_value_expr = if *field_type.field_type.kind == TypeKind::String {
            let start_of_chain = StartOfChain {
                kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                node: node.clone(),
            };
            let field_access_expr = create_expr_resolved(
                ExpressionKind::PostfixChain(start_of_chain, vec![postfix_lookup_field_in_self]),
                string_type.clone(),
                node,
            );

            let quote_kind = ExpressionKind::StringLiteral("\"".to_string());
            let quote_expr = create_expr_resolved(quote_kind, string_type.clone(), node);

            let concat_left_quote_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(quote_expr.clone()),
                right: Box::new(field_access_expr),
                node: node.clone(),
            };

            let with_left_quote_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(concat_left_quote_kind),
                string_type.clone(),
                node,
            );

            let concat_right_quote_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(with_left_quote_expr),
                right: Box::new(quote_expr),
                node: node.clone(),
            };
            create_expr_resolved(
                ExpressionKind::BinaryOp(concat_right_quote_kind),
                string_type.clone(),
                node,
            )
        } else {
            // Get to_string function for the field type - it must exist
            let to_string_fn = generator
                .associated_impls
                .get_internal_member_function(&field_type.field_type, "to_string")
                .unwrap_or_else(|| {
                    panic!(
                        "to_string() function must exist for type: {}",
                        field_type.field_type
                    )
                });

            let function_ref = Function::Internal(to_string_fn.clone());

            // Create call to to_string for the field
            let postfix_call_to_string = Postfix {
                node: node.clone(),
                ty: generator.types.string(),
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

            create_expr_resolved(lookup_kind, string_type.clone(), node)
        };

        // Concatenate field value to result
        let concat_value_kind = BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(result_expr),
            right: Box::new(field_value_expr),
            node: node.clone(),
        };
        result_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(concat_value_kind),
            string_type.clone(),
            node,
        );
    }

    // Create closing brace string
    let closing_kind = ExpressionKind::StringLiteral(" }".to_string());
    let closing_expr = create_expr_resolved(closing_kind, string_type.clone(), node);

    // Concatenate closing brace to result
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
fn generate_to_short_string_for_anon_struct(
    generator: &mut ExpressionGenerator,
    anonymous_struct_type_ref: &TypeRef,
    self_expression: &Expression,
) -> Expression {
    let node = &self_expression.node;
    let string_type = generator.types.string();

    // Create opening brace string
    let opening_kind = ExpressionKind::StringLiteral("{ ".to_string());
    let mut result_expr = create_expr_resolved(opening_kind, string_type.clone(), node);

    let TypeKind::AnonymousStruct(anonymous_struct_type) = &*anonymous_struct_type_ref.kind else {
        panic!("internal error")
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
            let separator_expr = create_expr_resolved(separator_kind, string_type.clone(), node);

            // Concatenate using + operator
            let concat_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(result_expr),
                right: Box::new(separator_expr),
                node: node.clone(),
            };
            result_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(concat_kind),
                string_type.clone(),
                node,
            );
        }

        let field_name_kind = ExpressionKind::StringLiteral(format!("{field_name}: "));
        let field_name_expr = create_expr_resolved(field_name_kind, string_type.clone(), node);

        let concat_name_kind = BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(result_expr),
            right: Box::new(field_name_expr),
            node: node.clone(),
        };

        result_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(concat_name_kind),
            string_type.clone(),
            node,
        );

        // Get field value from the struct
        let postfix_kind = PostfixKind::StructField(anonymous_struct_type_ref.clone(), field_index);
        let postfix_lookup_field_in_self = Postfix {
            node: node.clone(),
            ty: field_type.field_type.clone(),
            kind: postfix_kind,
        };

        let field_value_expr = if *field_type.field_type.kind == TypeKind::String {
            let start_of_chain = StartOfChain {
                kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                node: node.clone(),
            };
            let field_access_expr = create_expr_resolved(
                ExpressionKind::PostfixChain(start_of_chain, vec![postfix_lookup_field_in_self]),
                string_type.clone(),
                node,
            );

            let quote_kind = ExpressionKind::StringLiteral("\"".to_string());
            let quote_expr = create_expr_resolved(quote_kind, string_type.clone(), node);

            let concat_left_quote_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(quote_expr.clone()),
                right: Box::new(field_access_expr),
                node: node.clone(),
            };

            let with_left_quote_expr = create_expr_resolved(
                ExpressionKind::BinaryOp(concat_left_quote_kind),
                string_type.clone(),
                node,
            );

            let concat_right_quote_kind = BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(with_left_quote_expr),
                right: Box::new(quote_expr),
                node: node.clone(),
            };
            create_expr_resolved(
                ExpressionKind::BinaryOp(concat_right_quote_kind),
                string_type.clone(),
                node,
            )
        } else {
            // Get to_short_string function for the field type - it must exist
            let to_short_string_fn = generator
                .associated_impls
                .get_internal_member_function(&field_type.field_type, "to_short_string")
                .unwrap_or_else(|| {
                    panic!(
                        "to_short_string() function must exist for type: {}",
                        field_type.field_type
                    )
                });

            let function_ref = Function::Internal(to_short_string_fn.clone());

            // Create call to to_short_string for the field
            let postfix_call_to_string = Postfix {
                node: node.clone(),
                ty: generator.types.string(),
                kind: PostfixKind::MemberCall(FunctionRef::from(function_ref), vec![]),
            };

            // Create chain to access field and call to_short_string
            let start_of_chain = StartOfChain {
                kind: StartOfChainKind::Expression(Box::from(self_expression.clone())),
                node: node.clone(),
            };

            let lookup_kind = ExpressionKind::PostfixChain(
                start_of_chain,
                vec![postfix_lookup_field_in_self, postfix_call_to_string],
            );

            create_expr_resolved(lookup_kind, string_type.clone(), node)
        };

        // Concatenate field value to result
        let concat_value_kind = BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(result_expr),
            right: Box::new(field_value_expr),
            node: node.clone(),
        };
        result_expr = create_expr_resolved(
            ExpressionKind::BinaryOp(concat_value_kind),
            string_type.clone(),
            node,
        );
    }

    // Create closing brace string
    let closing_kind = ExpressionKind::StringLiteral(" }".to_string());
    let closing_expr = create_expr_resolved(closing_kind, string_type.clone(), node);

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

fn generate_to_string_for_enum(
    type_cache: &mut TypeCache,
    enum_type: &EnumType,
    argument_expression: Expression,
) -> Expression {
    let node = argument_expression.node.clone();
    let mut arms = Vec::new();
    let string_type = type_cache.string();
    for (variant_name, variant_type) in &enum_type.variants {
        let kind =
            ExpressionKind::StringLiteral(format!("{}::{}", enum_type.assigned_name, variant_name));
        let string_expr = create_expr_resolved(kind.clone(), string_type.clone(), &node);

        let arm_kind = MatchArm {
            pattern: Pattern::Normal(NormalPattern::EnumPattern(variant_type.clone(), None), None),
            expression: Box::new(string_expr.clone()),
            expression_type: type_cache.int(),
        };
        arms.push(arm_kind);
    }

    create_expr_resolved(
        ExpressionKind::Match(Match {
            arms,
            expression: Box::new(argument_expression),
        }),
        string_type,
        &node,
    )
}

fn generate_to_short_string_for_enum(
    type_cache: &mut TypeCache,
    enum_type: &EnumType,
    argument_expression: Expression,
) -> Expression {
    let node = argument_expression.node.clone();
    let mut arms = Vec::new();
    let string_type = type_cache.string();
    for (variant_name, variant_type) in &enum_type.variants {
        // For to_short_string, we only show the variant name without the enum type prefix
        let kind = ExpressionKind::StringLiteral(variant_name.clone());
        let string_expr = create_expr_resolved(kind.clone(), string_type.clone(), &node);

        let arm_kind = MatchArm {
            pattern: Pattern::Normal(NormalPattern::EnumPattern(variant_type.clone(), None), None),
            expression: Box::new(string_expr.clone()),
            expression_type: type_cache.int(),
        };
        arms.push(arm_kind);
    }

    create_expr_resolved(
        ExpressionKind::Match(Match {
            arms,
            expression: Box::new(argument_expression),
        }),
        string_type,
        &node,
    )
}

fn create_string_representation_of_expression(
    generator: &mut ExpressionGenerator,
    expression_to_convert: Expression,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let ty = expression_to_convert.ty.clone();

    if *ty.kind == TypeKind::String {
        let quote_expr = create_expr_resolved(
            ExpressionKind::StringLiteral("\"".to_string()),
            string_type.clone(),
            node,
        );
        let left_concat = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(quote_expr.clone()),
                right: Box::new(expression_to_convert),
                node: node.clone(),
            }),
            string_type.clone(),
            node,
        );
        create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::Add,
                left: Box::new(left_concat),
                right: Box::new(quote_expr),
                node: node.clone(),
            }),
            string_type,
            node,
        )
    } else {
        // Get to_short_string function first, fall back to to_string - both must exist
        let to_string_fn = generator
            .associated_impls
            .get_internal_member_function(&ty, "to_short_string")
            .or_else(|| {
                generator
                    .associated_impls
                    .get_internal_member_function(&ty, "to_string")
            })
            .unwrap_or_else(|| {
                panic!("to_short_string() or to_string() function must exist for type: {ty}")
            });

        let function_ref = Function::Internal(to_string_fn.clone());
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
}
impl GeneratedScope {
    pub fn new() -> Self {
        Self {
            scope: VariableScopes::default(),
        }
    }

    pub(crate) fn create_local_variable(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
    ) -> VariableRef {
        self.scope.current_register += 1;
        let virtual_register = self.scope.current_register;

        let var_ref = VariableRef::new(Variable {
            name: node.clone(), // Technically not correct, but at least we get a "valid" node
            assigned_name: assigned_name.to_string(),
            resolved_type: variable_type.clone(),
            mutable_node: None,
            variable_type: VariableType::Local,
            scope_index: 0,
            variable_index: 0,
            unique_id_within_function: virtual_register,
            virtual_register: virtual_register as u8,
            is_unused: false,
        });

        self.scope.all_variables.push(var_ref.clone());

        var_ref
    }

    pub(crate) fn create_parameter(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
    ) -> VariableRef {
        self.scope.current_register += 1;
        let virtual_register = self.scope.current_register;

        let var_ref = VariableRef::new(Variable {
            name: node.clone(), // Technically not correct, but at least we get a "valid" node
            assigned_name: assigned_name.to_string(),
            resolved_type: variable_type.clone(),
            mutable_node: None,
            variable_type: VariableType::Parameter,
            scope_index: 0,
            variable_index: 0,
            unique_id_within_function: virtual_register,
            virtual_register: virtual_register as u8,
            is_unused: false,
        });

        self.scope.all_variables.push(var_ref.clone());

        var_ref
    }

    pub(crate) fn create_local_mut_variable(
        &mut self,
        assigned_name: &str,
        variable_type: &TypeRef,
        node: &Node,
    ) -> VariableRef {
        self.scope.current_register += 1;
        let virtual_register = self.scope.current_register;

        let var_ref = VariableRef::new(Variable {
            name: node.clone(), // Technically not correct, but at least we get a "valid" node
            assigned_name: assigned_name.to_string(),
            resolved_type: variable_type.clone(),
            mutable_node: Some(node.clone()),
            variable_type: VariableType::Local,
            scope_index: 0,
            variable_index: 0,
            unique_id_within_function: virtual_register,
            virtual_register: virtual_register as u8,
            is_unused: false,
        });

        self.scope.all_variables.push(var_ref.clone());

        var_ref
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
) -> Expression {
    let string_type = generator.types.string();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();
    let int_type = generator.types.int();

    // let mut result = "["
    let (result_var, result_var_def) = {
        let result_var_ref = block_scope.create_local_mut_variable("result", &string_type, node);
        let opening_bracket = create_expr_resolved(
            ExpressionKind::StringLiteral("[".to_string()),
            string_type.clone(),
            node,
        );
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(result_var_ref.clone(), Box::new(opening_bracket)),
            unit_type.clone(),
            node,
        );
        (result_var_ref, def)
    };

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
        let element_var = block_scope.create_local_variable("element", element_type, node);

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

        // result = result + element.to_string()
        let append_element_expr = {
            let element_access = create_expr_resolved(
                ExpressionKind::VariableAccess(element_var.clone()),
                element_type.clone(),
                node,
            );
            let string_repr_expr =
                create_string_representation_of_expression(generator, element_access, node);
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

    create_expr_resolved(
        ExpressionKind::Block(vec![
            result_var_def,
            is_first_var_def,
            for_loop,
            closing_bracket_def,
            result_access_expr,
        ]),
        string_type,
        node,
    )
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
        let key_var = scope.create_local_variable("key", key_type, node);
        let value_var = scope.create_local_variable("val", value_type, node);

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
            let key_string_repr =
                create_string_representation_of_expression(generator, key_access, node);

            let value_access = create_expr_resolved(
                ExpressionKind::VariableAccess(value_var.clone()),
                value_type.clone(),
                node,
            );
            let value_string_repr =
                create_string_representation_of_expression(generator, value_access, node);

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

    let result_access_expr = create_expr_resolved(
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
            result_access_expr,
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
) -> Expression {
    let string_type = generator.types.string();

    // For optional types, we'll use a when expression instead of complex pattern matching
    // This creates: when value_var = self_expression { "Some(" + value_var.to_string() + ")" } else { "None" }

    // Create a variable to bind the unwrapped optional value
    let value_var = scope.create_local_variable("value", inner_type, node);

    // Create the `Some` case: "Some(" + value.to_string() + ")"
    let some_prefix = create_expr_resolved(
        ExpressionKind::StringLiteral("Some(".to_string()),
        string_type.clone(),
        node,
    );
    let some_suffix = create_expr_resolved(
        ExpressionKind::StringLiteral(")".to_string()),
        string_type.clone(),
        node,
    );

    // Get the variable access for the unwrapped value
    let value_access = create_expr_resolved(
        ExpressionKind::VariableAccess(value_var.clone()),
        inner_type.clone(),
        node,
    );

    // Get the to_string for the inner value
    let value_string = create_string_representation_of_expression(generator, value_access, node);

    // Concatenate: "Some(" + value.to_string() + ")"
    let prefix_plus_value = create_expr_resolved(
        ExpressionKind::BinaryOp(BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(some_prefix),
            right: Box::new(value_string),
            node: node.clone(),
        }),
        string_type.clone(),
        node,
    );

    let some_result = create_expr_resolved(
        ExpressionKind::BinaryOp(BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(prefix_plus_value),
            right: Box::new(some_suffix),
            node: node.clone(),
        }),
        string_type.clone(),
        node,
    );

    // Create the None case: "None"
    let none_result = create_expr_resolved(
        ExpressionKind::StringLiteral("None".to_string()),
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
            Box::new(some_result),
            Some(Box::new(none_result)),
        ),
        string_type,
        node,
    )
}

fn generate_to_short_string_for_optional(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    inner_type: &TypeRef,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();

    // For to_short_string on optional, we return the inner value's string for Some, "None" for None
    // This creates: when value_var = self_expression { value_var.to_short_string() } else { "None" }

    // Create a variable to bind the unwrapped optional value
    let value_var = scope.create_local_variable("value", inner_type, node);

    // Get the variable access for the unwrapped value
    let value_access = create_expr_resolved(
        ExpressionKind::VariableAccess(value_var.clone()),
        inner_type.clone(),
        node,
    );

    // Get the to_short_string for the inner value (just return the inner value's representation)
    let value_string = create_string_representation_of_expression(generator, value_access, node);

    // Create the None case: "None"
    let none_result = create_expr_resolved(
        ExpressionKind::StringLiteral("None".to_string()),
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
}

fn generate_to_string_for_tuple(
    generator: &mut ExpressionGenerator,
    self_expression: &Expression,
    tuple_types: &[TypeRef],
    node: &Node,
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

        let element_string =
            create_string_representation_of_expression(generator, element_access, node);

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

fn generate_to_short_string_for_tuple(
    generator: &mut ExpressionGenerator,
    self_expression: &Expression,
    tuple_types: &[TypeRef],
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();

    let mut result_expr = create_expr_resolved(
        ExpressionKind::StringLiteral("(".to_string()),
        string_type.clone(),
        node,
    );

    for (index, element_type) in tuple_types.iter().enumerate() {
        // Access the tuple element at this index using PostfixChain
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

        let element_string =
            create_string_representation_of_expression(generator, element_access, node);

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
        TypeKind::Int => panic!("Int to_string() is handled in core_text(), not generated here"),
        TypeKind::Float => {
            panic!("Float to_string() is handled in core_text(), not generated here")
        }
        TypeKind::Bool => panic!("Bool to_string() is handled in core_text(), not generated here"),
        TypeKind::String => {
            // For String type, to_string() should just return self
            first_self_param
        }
        TypeKind::StringStorage(_, _) => {
            panic!("StringStorage to_string() should be handled elsewhere")
        }
        // Unit and Function types cannot be stored in fields/collections in Swamp, so no to_string() needed
        TypeKind::Unit => panic!("Unit type cannot be stored in fields, no to_string() needed"),
        TypeKind::Tuple(tuple_types) => {
            generate_to_string_for_tuple(generator, &first_self_param, tuple_types, resolved_node)
        }
        TypeKind::NamedStruct(named) => {
            generate_to_string_for_named_struct(generator, named, &first_self_param)
        }
        TypeKind::AnonymousStruct(_anon_struct) => {
            generate_to_string_for_anon_struct(generator, ty, &first_self_param)
        }
        TypeKind::Range(_) => {
            panic!("Range to_string() is handled in core_text(), not generated here")
        }
        TypeKind::Enum(enum_type) => {
            generate_to_string_for_enum(generator.types, &enum_type.clone(), first_self_param)
        }
        TypeKind::Function(_) => {
            panic!("Function types cannot be stored in fields, no to_string() needed")
        }
        TypeKind::Optional(inner_type) => generate_to_string_for_optional(
            generator,
            &mut block_scope_to_use,
            first_self_param,
            inner_type,
            resolved_node,
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
        ),
        TypeKind::GridView(_) | TypeKind::GridStorage(_, _, _) => {
            // Grids don't support iteration yet, so return a placeholder string
            create_expr_resolved(
                ExpressionKind::StringLiteral("<Grid: iteration not yet supported>".to_string()),
                generator.types.string(),
                resolved_node,
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
        ),
    };

    let unique_function_id = id_gen.alloc();

    InternalFunctionDefinition {
        body: body_expr,
        name: LocalIdentifier(resolved_node.clone()),
        assigned_name: "to_string".to_string(),
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

#[allow(clippy::too_many_lines)]
pub fn internal_generate_to_short_string_function_for_type(
    generator: &mut ExpressionGenerator,
    id_gen: &mut InternalFunctionIdAllocator,
    module_path: &[String],
    ty: &TypeRef,
    resolved_node: &Node,
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
        TypeKind::Int => panic!("Int to_string() is handled in core_text(), not generated here"),
        TypeKind::Float => {
            panic!("Float to_string() is handled in core_text(), not generated here")
        }
        TypeKind::Bool => panic!("Bool to_string() is handled in core_text(), not generated here"),
        TypeKind::String => {
            // For String type, to_string() should just return self
            first_self_param
        }
        TypeKind::StringStorage(_, _) => {
            panic!("StringStorage to_string() should be handled elsewhere")
        }
        // Unit and Function types cannot be stored in fields/collections in Swamp, so no to_string() needed
        TypeKind::Unit => panic!("Unit type cannot be stored in fields, no to_string() needed"),
        TypeKind::Tuple(tuple_types) => generate_to_short_string_for_tuple(
            generator,
            &first_self_param,
            tuple_types,
            resolved_node,
        ),
        TypeKind::NamedStruct(named) => {
            generate_to_short_string_for_named_struct(generator, named, &first_self_param)
        }
        TypeKind::AnonymousStruct(_anon_struct) => {
            generate_to_short_string_for_anon_struct(generator, ty, &first_self_param)
        }
        TypeKind::Range(_) => {
            panic!("Range to_string() is handled in core_text(), not generated here")
        }
        TypeKind::Enum(enum_type) => {
            generate_to_short_string_for_enum(generator.types, &enum_type.clone(), first_self_param)
        }
        TypeKind::Function(_) => {
            panic!("Function types cannot be stored in fields, no to_string() needed")
        }
        TypeKind::Optional(inner_type) => generate_to_short_string_for_optional(
            generator,
            &mut block_scope_to_use,
            first_self_param,
            inner_type,
            resolved_node,
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
        ),
        TypeKind::GridView(_) | TypeKind::GridStorage(_, _, _) => {
            // Grids don't support iteration yet, so return a placeholder string
            create_expr_resolved(
                ExpressionKind::StringLiteral("<Grid: iteration not yet supported>".to_string()),
                generator.types.string(),
                resolved_node,
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
        ),
    };

    let unique_function_id = id_gen.alloc();

    block_scope_to_use.scope.finalize();

    InternalFunctionDefinition {
        body: body_expr,
        name: LocalIdentifier(resolved_node.clone()),
        assigned_name: "to_short_string".to_string(),
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
    );

    let unique_function_id = id_gen.alloc();

    block_scope_to_use.scope.finalize();

    InternalFunctionDefinition {
        body: body_expr,
        name: LocalIdentifier(resolved_node.clone()),
        assigned_name: "to_pretty_string".to_string(),
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

#[allow(clippy::too_many_lines)]
fn generate_to_pretty_string_for_type(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    ty: &TypeRef,
    self_expression: &Expression,
    indentation_expression: Expression,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();

    // Step 1: compact = self.to_string()
    let compact_var = scope.create_local_variable("compact", &string_type, node);
    let compact_string = call_to_string_method(generator, self_expression.clone(), node);
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
        | TypeKind::SparseStorage(_, _) => generate_sequence_pretty_string(
            generator,
            scope,
            self_expression.clone(),
            indentation_expression,
            ty,
            node,
        ),
        TypeKind::NamedStruct(_) | TypeKind::AnonymousStruct(_) | TypeKind::Tuple(_) => {
            generate_struct_pretty_string(
                generator,
                scope,
                self_expression,
                &indentation_expression,
                ty,
                node,
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

fn call_to_string_method(
    generator: &mut ExpressionGenerator,
    self_expression: Expression,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let ty = self_expression.ty.clone();

    let to_string_fn = generator
        .associated_impls
        .get_internal_member_function(&ty, "to_string")
        .unwrap_or_else(|| panic!("to_string() function must exist for type: {ty}"));

    let function_ref = Function::Internal(to_string_fn.clone());
    let start_of_chain = StartOfChain {
        kind: StartOfChainKind::Expression(Box::from(self_expression)),
        node: node.clone(),
    };
    let postfix_call_to_string = Postfix {
        node: node.clone(),
        ty: string_type.clone(),
        kind: PostfixKind::MemberCall(FunctionRef::from(function_ref), vec![]),
    };

    let lookup_kind = ExpressionKind::PostfixChain(start_of_chain, vec![postfix_call_to_string]);
    create_expr_resolved(lookup_kind, string_type, node)
}

fn call_to_pretty_string_method(
    generator: &mut ExpressionGenerator,
    self_expression: Expression,
    indentation_expression: Expression,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let ty = self_expression.ty.clone();

    let to_pretty_string_fn = generator
        .associated_impls
        .get_internal_member_function(&ty, "to_pretty_string")
        .unwrap_or_else(|| panic!("to_pretty_string() function must exist for type: {ty}"));

    let function_ref = Function::Internal(to_pretty_string_fn.clone());
    let start_of_chain = StartOfChain {
        kind: StartOfChainKind::Expression(Box::from(self_expression)),
        node: node.clone(),
    };
    let postfix_call_to_pretty_string = Postfix {
        node: node.clone(),
        ty: string_type.clone(),
        kind: PostfixKind::MemberCall(
            FunctionRef::from(function_ref),
            vec![ArgumentExpression::Expression(indentation_expression)],
        ),
    };

    let lookup_kind =
        ExpressionKind::PostfixChain(start_of_chain, vec![postfix_call_to_pretty_string]);
    create_expr_resolved(lookup_kind, string_type, node)
}

#[allow(clippy::too_many_lines)]
fn generate_indentation_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    indentation_var: &VariableRef,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();

    // let mut result = ""
    let (result_var, result_var_def) = {
        let var = scope.create_local_mut_variable("indentation_str", &string_type, node);
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

    // let mut i = 0
    let (counter_var, counter_var_def) = {
        let var = scope.create_local_mut_variable("i", &int_type, node);
        let zero = create_expr_resolved(ExpressionKind::IntLiteral(0), int_type.clone(), node);
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(zero)),
            unit_type.clone(),
            node,
        );
        (var, def)
    };

    // while i < indentation { result = result + "  "; i = i + 1 }
    let while_loop = {
        let counter_access = create_expr_resolved(
            ExpressionKind::VariableAccess(counter_var.clone()),
            int_type.clone(),
            node,
        );
        let indentation_access = create_expr_resolved(
            ExpressionKind::VariableAccess(indentation_var.clone()),
            int_type.clone(),
            node,
        );

        let condition = create_expr_resolved(
            ExpressionKind::BinaryOp(BinaryOperator {
                kind: BinaryOperatorKind::LessThan,
                left: Box::new(counter_access),
                right: Box::new(indentation_access),
                node: node.clone(),
            }),
            bool_type,
            node,
        );

        // result = result + "  "
        let append_spaces = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let spaces = create_expr_resolved(
                ExpressionKind::StringLiteral("  ".to_string()),
                string_type.clone(),
                node,
            );
            let concat = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(spaces),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat)),
                unit_type.clone(),
                node,
            )
        };

        // i = i + 1
        let increment_counter = {
            let counter_access = create_expr_resolved(
                ExpressionKind::VariableAccess(counter_var.clone()),
                int_type.clone(),
                node,
            );
            let one = create_expr_resolved(ExpressionKind::IntLiteral(1), int_type.clone(), node);
            let add = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(counter_access),
                    right: Box::new(one),
                    node: node.clone(),
                }),
                int_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(counter_var, Box::new(add)),
                unit_type.clone(),
                node,
            )
        };

        let while_body = create_expr_resolved(
            ExpressionKind::Block(vec![append_spaces, increment_counter]),
            unit_type.clone(),
            node,
        );

        create_expr_resolved(
            ExpressionKind::WhileLoop(
                BooleanExpression {
                    expression: Box::new(condition),
                },
                Box::new(while_body),
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
            counter_var_def,
            while_loop,
            result_access,
        ]),
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
            ExpressionKind::VariableAccess(indentation_var.clone()),
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
        let key_var = scope.create_local_variable("key", key_type, node);
        let value_var = scope.create_local_variable("value", value_type, node);

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

            // Call to_pretty_string on both key and value
            let key_pretty_str = call_to_pretty_string_method(
                generator,
                key_access,
                next_indent_access.clone(),
                node,
            );
            let value_pretty_str =
                call_to_pretty_string_method(generator, value_access, next_indent_access, node);

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

            let final_indent_spaces =
                generate_indentation_string(generator, scope, &indentation_var, node);
            let newline_str = create_expr_resolved(
                ExpressionKind::StringLiteral("\n".to_string()),
                string_type.clone(),
                node,
            );
            let closing_str = create_expr_resolved(
                ExpressionKind::StringLiteral("|]".to_string()),
                string_type.clone(),
                node,
            );

            // result + "\n" + final_indent_spaces + "|]"
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
                    right: Box::new(final_indent_spaces),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let final_result = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp2),
                    right: Box::new(closing_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );

            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(final_result)),
                unit_type.clone(),
                node,
            )
        };

        // if !has_items { empty_case } else { non_empty_case }
        let condition = create_expr_resolved(
            ExpressionKind::UnaryOp(UnaryOperator {
                kind: UnaryOperatorKind::Not,
                left: Box::new(has_items_access),
                node: node.clone(),
            }),
            generator.types.bool(),
            node,
        );

        create_expr_resolved(
            ExpressionKind::If(
                BooleanExpression {
                    expression: Box::new(condition),
                },
                Box::new(empty_case),
                Some(Box::new(non_empty_case)),
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

fn generate_sequence_pretty_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    indentation_expression: Expression,
    ty: &TypeRef,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();
    let bool_type = generator.types.bool();

    // Step 1: Get compact representation
    let (compact_var, compact_def) = {
        let var = scope.create_local_variable("compact", &string_type, node);
        let compact_string = call_to_string_method(generator, self_expression.clone(), node);
        let def = create_expr_resolved(
            ExpressionKind::VariableDefinition(var.clone(), Box::new(compact_string)),
            unit_type,
            node,
        );
        (var, def)
    };

    // Step 2: Check length using StringLen intrinsic
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

    // Step 3: Generate multi-line format for sequences
    let multi_line_result = generate_multi_line_sequence_format(
        generator,
        scope,
        self_expression,
        indentation_expression,
        ty,
        node,
    );

    // Step 4: Return compact if short, multi-line if long
    let compact_access_for_if = create_expr_resolved(
        ExpressionKind::VariableAccess(compact_var),
        string_type.clone(),
        node,
    );

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

#[allow(clippy::too_many_lines)]
fn generate_multi_line_sequence_format(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: Expression,
    indentation_expression: Expression,
    ty: &TypeRef,
    node: &Node,
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
        let element_var = scope.create_local_variable("element", element_type, node);

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

            let element_pretty_str =
                call_to_pretty_string_method(generator, element_access, next_indent_access, node);

            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );

            // Generate indentation string for next_indentation level
            let indent_spaces =
                generate_indentation_string(generator, scope, &next_indent_var, node);

            let comma_newline_str = create_expr_resolved(
                ExpressionKind::StringLiteral(",\n".to_string()),
                string_type.clone(),
                node,
            );

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

    // Add final indentation and closing bracket `result += indentation_spaces + ]`
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
) -> Expression {
    match &*ty.kind {
        TypeKind::NamedStruct(named_struct) => generate_named_struct_pretty_string(
            generator,
            scope,
            self_expression,
            indentation_expression,
            named_struct,
            node,
        ),
        TypeKind::AnonymousStruct(_) => generate_anon_struct_pretty_string(
            generator,
            scope,
            self_expression,
            indentation_expression,
            ty,
            node,
        ),
        TypeKind::Tuple(tuple_types) => generate_tuple_pretty_string(
            generator,
            scope,
            self_expression,
            indentation_expression,
            tuple_types,
            node,
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

    // We need to extract the indentation variable from the expression
    // Since we know indentation_expr is a VariableAccess, we can extract the variable
    let indentation_var = match &indentation_expr.kind {
        ExpressionKind::VariableAccess(var_ref) => var_ref.clone(),
        _ => panic!("Expected variable access for indentation"),
    };

    // Generate the proper indentation string
    let indentation_spaces = generate_indentation_string(generator, scope, &indentation_var, node);

    // Concatenate result + indentation_spaces
    let concat = create_expr_resolved(
        ExpressionKind::BinaryOp(BinaryOperator {
            kind: BinaryOperatorKind::Add,
            left: Box::new(result_access),
            right: Box::new(indentation_spaces),
            node: node.clone(),
        }),
        string_type,
        node,
    );

    create_expr_resolved(
        ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat)),
        unit_type,
        node,
    )
}

#[allow(clippy::too_many_lines)]
fn generate_named_struct_pretty_string(
    generator: &mut ExpressionGenerator,
    scope: &mut GeneratedScope,
    self_expression: &Expression,
    indentation_expression: &Expression,
    named_struct: &NamedStructType,
    node: &Node,
) -> Expression {
    let string_type = generator.types.string();
    let int_type = generator.types.int();
    let unit_type = generator.types.unit();

    // Start with struct name + " {\n"
    let (result_var, result_def) = {
        let var = scope.create_local_mut_variable("result", &string_type, node);
        let struct_prefix = format!("{} {{\n", named_struct.assigned_name);
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

        // Add field name: "field_name: "
        let add_field_name = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let field_name_str = create_expr_resolved(
                ExpressionKind::StringLiteral(format!("{field_name}: ")),
                string_type.clone(),
                node,
            );
            let concat = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(field_name_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat)),
                unit_type.clone(),
                node,
            )
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

            let next_indent_access = create_expr_resolved(
                ExpressionKind::VariableAccess(next_indent_var.clone()),
                int_type.clone(),
                node,
            );

            let field_pretty_string =
                call_to_pretty_string_method(generator, field_access, next_indent_access, node);

            let newline = create_expr_resolved(
                ExpressionKind::StringLiteral("\n".to_string()),
                string_type.clone(),
                node,
            );

            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );

            // result + field_pretty_string + "\n"
            let temp1 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(field_pretty_string),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let final_str = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp1),
                    right: Box::new(newline),
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

        // Add field name: "field_name: "
        let add_field_name = {
            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );
            let field_name_str = create_expr_resolved(
                ExpressionKind::StringLiteral(format!("{field_name}: ")),
                string_type.clone(),
                node,
            );
            let concat = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(field_name_str),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            create_expr_resolved(
                ExpressionKind::VariableReassignment(result_var.clone(), Box::new(concat)),
                unit_type.clone(),
                node,
            )
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

            let next_indent_access = create_expr_resolved(
                ExpressionKind::VariableAccess(next_indent_var.clone()),
                int_type.clone(),
                node,
            );

            let field_pretty_string =
                call_to_pretty_string_method(generator, field_access, next_indent_access, node);

            let newline = create_expr_resolved(
                ExpressionKind::StringLiteral("\n".to_string()),
                string_type.clone(),
                node,
            );

            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );

            // result + field_pretty_string + "\n"
            let temp1 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(field_pretty_string),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let final_str = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp1),
                    right: Box::new(newline),
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

            let next_indent_access = create_expr_resolved(
                ExpressionKind::VariableAccess(next_indent_var.clone()),
                int_type.clone(),
                node,
            );

            let element_pretty_string =
                call_to_pretty_string_method(generator, element_access, next_indent_access, node);

            let newline = create_expr_resolved(
                ExpressionKind::StringLiteral("\n".to_string()),
                string_type.clone(),
                node,
            );

            let result_access = create_expr_resolved(
                ExpressionKind::VariableAccess(result_var.clone()),
                string_type.clone(),
                node,
            );

            // result + element_pretty_string + "\n"
            let temp1 = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(result_access),
                    right: Box::new(element_pretty_string),
                    node: node.clone(),
                }),
                string_type.clone(),
                node,
            );
            let final_str = create_expr_resolved(
                ExpressionKind::BinaryOp(BinaryOperator {
                    kind: BinaryOperatorKind::Add,
                    left: Box::new(temp1),
                    right: Box::new(newline),
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
