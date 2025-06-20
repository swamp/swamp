/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::ErrorKind;
use crate::{Analyzer, TypeContext};
use seq_map::SeqMap;
use seq_set::SeqSet;
use source_map_node::Node;
use swamp_semantic::{
    AnonymousStructLiteral, Expression, ExpressionKind, FunctionRef, LocationAccess,
    LocationAccessKind, MutableReferenceKind, SingleLocationExpression, TargetAssignmentLocation,
};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    fn analyze_struct_init_calling_default(
        &mut self,
        function: &FunctionRef,
        super_type: &TypeRef,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: Vec<(usize, Option<Node>, Expression)>,
        node: &swamp_ast::Node,
    ) -> Expression {
        let mut expressions = Vec::new();

        self.push_block_scope("struct_instantiation");

        let temp_var = self.create_local_variable_generated("__generated", true, super_type);

        // temp_var = StructType::default()
        let return_type = function.signature().return_type.clone();

        let default_call_kind = self.create_default_static_call(node, super_type);

        let static_call = self.create_expr(default_call_kind, return_type, node);

        let unit_type = self.shared.state.types.unit();
        let expr = self.create_expr(
            ExpressionKind::VariableDefinition(temp_var.clone(), Box::new(static_call)),
            unit_type,
            node,
        );
        expressions.push(expr);

        // overwrite fields in temp_var with assignments
        for (field_target_index, resolved_field_name_node, field_source_expression) in
            source_order_expressions
        {
            let node = field_source_expression.node.clone();

            let field_expression_type = field_source_expression.ty.clone();

            let kind = LocationAccessKind::FieldIndex(anon_struct_type.clone(), field_target_index);

            let single_chain = vec![LocationAccess {
                node: Node::default(), // resolved_field_name_node.map(|| Node::default()),
                ty: field_expression_type.clone(),
                kind,
            }];

            let created_location = SingleLocationExpression {
                kind: MutableReferenceKind::MutStructFieldRef(
                    anon_struct_type.clone(),
                    field_target_index,
                ),
                node: node.clone(),
                ty: field_expression_type.clone(),
                starting_variable: temp_var.clone(),
                access_chain: single_chain,
            };

            let created_mut_location = TargetAssignmentLocation(created_location);

            let unit_type = self.shared.state.types.unit();
            let overwrite_expression = self.create_expr_resolved(
                ExpressionKind::Assignment(
                    Box::from(created_mut_location),
                    Box::new(field_source_expression),
                ),
                unit_type,
                &node,
            );

            expressions.push(overwrite_expression);
        }

        let ty = temp_var.resolved_type.clone();
        let access_variable =
            self.create_expr(ExpressionKind::VariableAccess(temp_var), ty.clone(), node);

        expressions.push(access_variable); // make sure the block returns the overwritten temp_var

        self.pop_block_scope("struct instantiation");

        self.create_expr(ExpressionKind::Block(expressions), ty, node)
    }

    fn get_struct_like_type(ty: &TypeRef) -> TypeRef {
        ty.clone()
    }

    fn analyze_struct_init_field_by_field(
        &mut self,
        borrowed_anon_type: &AnonymousStructType,
        mut source_order_expressions: Vec<(usize, Option<Node>, Expression)>,
        missing_fields: SeqSet<String>,
        result_type: &TypeRef,
        node: &swamp_ast::Node,
    ) -> Expression {
        {
            for missing_field_name in missing_fields {
                let field = borrowed_anon_type
                    .field_name_sorted_fields
                    .get(&missing_field_name)
                    .expect("should have been verified by helper function");
                let field_index = borrowed_anon_type
                    .field_name_sorted_fields
                    .get_index(&missing_field_name)
                    .expect("should have been verified earlier");

                let expression = self.create_default_value_for_type(node, &field.field_type);

                source_order_expressions.push((field_index, field.identifier.clone(), expression));
            }
        }

        self.create_expr(
            ExpressionKind::AnonymousStructLiteral(AnonymousStructLiteral {
                struct_like_type: Self::get_struct_like_type(result_type),
                source_order_expressions: source_order_expressions.clone(),
            }),
            result_type.clone(),
            node,
        )
    }

    pub fn deduce_the_anon_struct_type(
        &mut self,
        ast_fields: &Vec<swamp_ast::FieldExpression>,
    ) -> AnonymousStructType {
        let mut map_for_creating_type = SeqMap::new();

        for field in ast_fields {
            let field_name = self.get_text(&field.field_name.0).to_string();
            let resolved_node = self.to_node(&field.field_name.0);

            let field_type_context = TypeContext::new_anything_argument();
            let resolved_expression =
                self.analyze_expression(&field.expression, &field_type_context);

            let expression_type = resolved_expression.ty.clone();

            let field = StructTypeField {
                identifier: Some(resolved_node),
                field_type: expression_type,
            };

            map_for_creating_type
                .insert(field_name.clone(), field)
                .expect("TODO: panic message");
        }

        // For a pure anonymous struct type, the types of the sorted
        // fields by field name is the actual type
        AnonymousStructType::new_and_sort_fields(&map_for_creating_type)
    }

    /// # Errors
    ///
    /// # Panics
    ///
    pub fn analyze_anonymous_struct_literal(
        &mut self,
        node: &swamp_ast::Node,
        ast_fields: &Vec<swamp_ast::FieldExpression>,
        rest_was_specified: bool,
        context: &TypeContext,
    ) -> Expression {
        // First check what we should compare the anonymous struct literal to. Either it is "pure" and then we
        // compare it against itself or otherwise a type that the context require (a named or an anonymous struct type).
        let (super_type, anon_struct_type) = if let Some(expected_type) = context.expected_type {
            match &*expected_type.kind {
                TypeKind::NamedStruct(named_struct_type) => {
                    //maybe_named_struct = Some(named_struct.clone());
                    if let TypeKind::AnonymousStruct(anon_struct) =
                        &*named_struct_type.anon_struct_type.kind
                    {
                        (expected_type, anon_struct)
                    } else {
                        return self
                            .create_err(ErrorKind::CouldNotCoerceTo(expected_type.clone()), node);
                    }
                }
                TypeKind::AnonymousStruct(anonymous_struct_type) => {
                    (expected_type, anonymous_struct_type)
                }
                _ => {
                    return self
                        .create_err(ErrorKind::CouldNotCoerceTo(expected_type.clone()), node);
                }
            }
        } else {
            let deduced_anon_struct_type = self.deduce_the_anon_struct_type(ast_fields);
            let anon_struct_type_ref = self
                .shared
                .state
                .types
                .anonymous_struct(deduced_anon_struct_type.clone());

            // Move the anon_struct_type_ref to avoid lifetime issues
            return self.analyze_struct_init(
                node,
                &anon_struct_type_ref,
                &deduced_anon_struct_type,
                ast_fields,
                rest_was_specified,
            );
        };

        // This path is for the first branch (when we have a specific expected type)
        self.analyze_struct_init(
            node,
            super_type,
            anon_struct_type,
            ast_fields,
            rest_was_specified,
        )
    }

    pub(crate) fn analyze_named_struct_literal(
        &mut self,
        qualified_type_identifier: &swamp_ast::QualifiedTypeIdentifier,
        ast_fields: &Vec<swamp_ast::FieldExpression>,
        rest_was_specified: bool,
    ) -> Expression {
        let named_struct_type = self.get_struct_type(qualified_type_identifier);

        let super_type = self
            .shared
            .state
            .types
            .named_struct(named_struct_type.clone());

        if let TypeKind::AnonymousStruct(anon_struct) = &*named_struct_type.anon_struct_type.kind {
            self.analyze_struct_init(
                &qualified_type_identifier.name.0,
                &super_type,
                anon_struct,
                ast_fields,
                rest_was_specified,
            )
        } else {
            self.create_err(
                ErrorKind::UnknownStructTypeReference,
                &qualified_type_identifier.name.0,
            )
        }
    }

    fn analyze_struct_init(
        &mut self,
        node: &swamp_ast::Node,
        super_type: &TypeRef,
        anon_struct_type: &AnonymousStructType,
        ast_fields: &Vec<swamp_ast::FieldExpression>,
        rest_was_specified: bool,
    ) -> Expression {
        let (source_order_expressions, missing_fields) = self
            .place_anon_struct_fields_that_exist_and_return_missing(anon_struct_type, ast_fields);

        if missing_fields.is_empty() {
            // No missing fields, we are done!
            self.create_expr(
                ExpressionKind::AnonymousStructLiteral(AnonymousStructLiteral {
                    struct_like_type: Self::get_struct_like_type(super_type),
                    source_order_expressions,
                }),
                super_type.clone(),
                node,
            )
        } else {
            // Missing fields detected!
            if rest_was_specified {
                self.make_solution_for_missing_fields(
                    node,
                    super_type,
                    anon_struct_type,
                    source_order_expressions,
                    missing_fields,
                )
            } else {
                self.create_err(
                    ErrorKind::MissingFieldInStructInstantiation(
                        missing_fields.to_vec(),
                        anon_struct_type.clone(),
                    ),
                    node,
                )
            }
        }
    }

    /*
    pub fn analyze_struct_fields_against_anon_struct_type(
        &mut self,
        node: &swamp_ast::Node,
        ast_fields: &Vec<swamp_ast::FieldExpression>,
        rest_was_specified: bool,
        context: &TypeContext,
    ) -> Expression {
        let mut maybe_named_struct: Option<NamedStructType> = None;



        let mapped = self.analyze_anon_struct_instantiation(
            node,
            &struct_to_instantiate,
            ast_fields,
            rest_was_specified,
        )?;

        let (kind, ty) = if let Some(named_struct) = maybe_named_struct {
            (
                ExpressionKind::StructInstantiation(StructInstantiation {
                    source_order_expressions: mapped,
                    struct_type_ref: named_struct.clone(),
                }),
                TypeKind::NamedStruct(named_struct),
            )
        } else {
            (
                ExpressionKind::AnonymousStructLiteral(AnonymousStructLiteral {
                    source_order_expressions: mapped,
                    anonymous_struct_type: struct_to_instantiate.clone(),
                }),
                TypeKind::AnonymousStruct(struct_to_instantiate),
            )
        };

        Ok(self.create_expr(kind, ty, node))
    }

     */

    fn make_solution_for_missing_fields(
        &mut self,
        node: &swamp_ast::Node,
        super_type: &TypeRef,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: Vec<(usize, Option<Node>, Expression)>,
        missing_fields: SeqSet<String>,
    ) -> Expression {
        // First check if the super type has a default function (trait)
        let maybe_default = {
            self.shared
                .state
                .associated_impls
                .get_member_function(super_type, "default")
                .cloned()
        };

        // if it has a `default` function, call that to get a starting value

        if let Some(function) = maybe_default {
            self.analyze_struct_init_calling_default(
                &function,
                super_type,
                anon_struct_type,
                source_order_expressions,
                node,
            )
        } else {
            /*
            let mapped: Vec<(usize, Expression)> = &source_order_expressions
                .into_iter()
                .map(|(a, _b, c)| (a, c))
                .collect::<Vec<_>>();

             */
            self.analyze_struct_init_field_by_field(
                anon_struct_type,
                source_order_expressions,
                missing_fields,
                super_type,
                node,
            )
        }

        /*
        else if missing_fields.is_empty() {

            let ty = TypeKind::NamedStruct(struct_to_instantiate.clone());
            let node = qualified_type_identifier.name.0.clone();
            let mapped: Vec<(usize, Expression)> = source_order_expressions
                .into_iter()
                .map(|(a, _b, c)| (a, c))
                .collect::<Vec<_>>();
            Ok(self.create_expr(
                ExpressionKind::StructInstantiation(StructInstantiation {
                    source_order_expressions: mapped,
                    struct_type_ref: struct_to_instantiate,
                }),
                ty,
                &node,
            ))
        }

         */
    }

    fn place_anon_struct_fields_that_exist_and_return_missing(
        &mut self,
        target_anon_struct_type: &AnonymousStructType,
        ast_fields: &Vec<swamp_ast::FieldExpression>,
    ) -> (Vec<(usize, Option<Node>, Expression)>, SeqSet<String>) {
        let mut missing_fields: SeqSet<String> = target_anon_struct_type
            .field_name_sorted_fields
            .keys()
            .cloned()
            .collect();

        let mut source_order_expressions = Vec::new();

        for field in ast_fields {
            let field_name = self.get_text(&field.field_name.0).to_string();
            let resolved_node = self.to_node(&field.field_name.0);

            // If we can't remove it from missing_fields, it's either a duplicate or unknown field
            if !missing_fields.remove(&field_name) {
                return if target_anon_struct_type
                    .field_name_sorted_fields
                    .contains_key(&field_name)
                {
                    self.add_err(
                        ErrorKind::DuplicateFieldInStructInstantiation(field_name),
                        &field.field_name.0,
                    );
                    (vec![], SeqSet::new())
                } else {
                    self.add_err(ErrorKind::UnknownStructField, &field.field_name.0);
                    (vec![], SeqSet::new())
                };
            }

            let looked_up_field = target_anon_struct_type
                .field_name_sorted_fields
                .get(&field_name)
                .expect("field existence checked above");

            let field_index_in_definition = target_anon_struct_type
                .field_name_sorted_fields
                .get_index(&field_name)
                .expect("field_name is checked earlier");

            let field_type_context = TypeContext::new_argument(&looked_up_field.field_type);
            let resolved_expression =
                self.analyze_expression(&field.expression, &field_type_context);

            source_order_expressions.push((
                field_index_in_definition,
                Some(resolved_node),
                resolved_expression,
            ));
        }

        (source_order_expressions, missing_fields)
    }

    pub(crate) fn analyze_anon_struct_instantiation(
        &mut self,
        node: &swamp_ast::Node,
        struct_to_instantiate: &AnonymousStructType,
        ast_fields: &Vec<swamp_ast::FieldExpression>,
        allow_rest: bool,
    ) -> Vec<(usize, Option<Node>, Expression)> {
        let (source_order_expressions, missing_fields) = self
            .place_anon_struct_fields_that_exist_and_return_missing(
                struct_to_instantiate,
                ast_fields,
            );

        let mut mapped: Vec<(usize, Option<Node>, Expression)> = source_order_expressions;

        if allow_rest {
            // Call `default()` for the missing fields
            for missing_field_name in missing_fields {
                let field = struct_to_instantiate
                    .field_name_sorted_fields
                    .get(&missing_field_name)
                    .expect("field must exist in struct definition");

                let field_index = struct_to_instantiate
                    .field_name_sorted_fields
                    .get_index(&missing_field_name)
                    .expect("field must exist in struct definition");

                // Here you would create the default value for the field
                let default_expression =
                    self.create_default_value_for_type(node, &field.field_type);

                mapped.push((field_index, None, default_expression));
            }
        } else if !missing_fields.is_empty() {
            self.add_err(
                ErrorKind::MissingFieldInStructInstantiation(
                    missing_fields.to_vec(),
                    struct_to_instantiate.clone(),
                ),
                node,
            );
            return vec![];
        }

        mapped
    }
}
