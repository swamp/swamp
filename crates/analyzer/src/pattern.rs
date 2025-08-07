/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use swamp_semantic::err::ErrorKind;
use swamp_semantic::{NormalPattern, Pattern, PatternElement};
use swamp_symbol::TopLevelSymbolId;
use swamp_types::prelude::EnumVariantType as TypesEnumVariantType;
use swamp_types::prelude::{EnumVariantCommon, TypeKind, TypeRef};

impl Analyzer<'_> {
    fn find_variant_in_pattern(
        &mut self,
        expression_type: &TypeRef,
        ast_name: &swamp_ast::Node,
    ) -> TypesEnumVariantType {
        let enum_type_ref = if let TypeKind::Enum(enum_type_ref) = &*expression_type.kind {
            enum_type_ref
        } else {
            self.add_err(ErrorKind::ExpectedEnumInPattern, ast_name);

            return TypesEnumVariantType {
                common: EnumVariantCommon {
                    symbol_id: TopLevelSymbolId::new_illegal(),
                    name: Default::default(),
                    assigned_name: String::new(),
                    container_index: 0,
                },
                payload_type: self.types().unit(),
            };
        };

        let variant_name = self.get_text(ast_name).to_string();

        if let Some(variant) = enum_type_ref.get_variant(&variant_name) {
            variant.clone()
        } else {
            self.add_err(ErrorKind::UnknownEnumVariantTypeInPattern, ast_name);
            TypesEnumVariantType {
                common: EnumVariantCommon {
                    symbol_id: TopLevelSymbolId::new_illegal(),
                    name: Default::default(),
                    assigned_name: String::new(),
                    container_index: 0,
                },
                payload_type: self.types().unit(),
            }
        }
    }

    pub(crate) fn analyze_pattern(
        &mut self,
        ast_pattern: &swamp_ast::Pattern,
        expected_condition_type: &TypeRef,
    ) -> (Pattern, bool, bool) {
        match ast_pattern {
            swamp_ast::Pattern::Wildcard(node) => {
                (Pattern::Wildcard(self.to_node(node)), false, false)
            }
            swamp_ast::Pattern::ConcretePattern(node, concrete_pattern, maybe_guard) => {
                let (normal_pattern, was_pushed, wanted_mutable) =
                    self.analyze_normal_pattern(node, concrete_pattern, expected_condition_type);

                let resolved_guard =
                    maybe_guard
                        .as_ref()
                        .and_then(|guard_clause| match guard_clause {
                            swamp_ast::GuardClause::Wildcard(_) => None,
                            swamp_ast::GuardClause::Expression(clause_expr) => {
                                Some(self.analyze_bool_argument_expression(clause_expr))
                            }
                        });
                (
                    Pattern::Normal(normal_pattern, resolved_guard),
                    was_pushed,
                    wanted_mutable,
                )
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn analyze_normal_pattern(
        &mut self,
        node: &swamp_ast::Node,
        ast_concrete_pattern: &swamp_ast::ConcretePattern,
        expected_condition_type: &TypeRef,
    ) -> (NormalPattern, bool, bool) {
        let mut anyone_wants_mutable = false;
        match ast_concrete_pattern {
            swamp_ast::ConcretePattern::EnumPattern(variant_name, destructuring_pattern) => {
                let mut scope_was_pushed = false;
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expected_condition_type, variant_name);

                let name_node = self.to_node(node);
                self.shared.state.refs.add(
                    enum_variant_type_ref.common.symbol_id.into(),
                    name_node.clone(),
                );
                self.shared
                    .definition_table
                    .refs
                    .add(enum_variant_type_ref.common.symbol_id.into(), name_node);

                match destructuring_pattern {
                    swamp_ast::DestructuringPattern::Struct { fields } => {
                        if !scope_was_pushed {
                            self.push_block_scope("enum struct");
                            scope_was_pushed = true;
                        }

                        let mut resolved_elements = Vec::new();

                        match &*enum_variant_type_ref.payload_type.kind {
                            TypeKind::AnonymousStruct(anon_struct_type) => {
                                // For structs, can match any subset of fields in any order
                                for var in fields {
                                    let var_name_str = self.get_text(&var.name).to_string();
                                    // Check if the field exists
                                    let Some(field_index) = anon_struct_type
                                        .field_name_sorted_fields
                                        .get_index(&var_name_str)
                                    else {
                                        return (
                                            NormalPattern::Literal(
                                                self.create_err(ErrorKind::UnknownField, &var.name),
                                            ),
                                            false,
                                            false,
                                        );
                                    };

                                    let Some(field_type) = anon_struct_type
                                        .field_name_sorted_fields
                                        .get(&var_name_str)
                                    else {
                                        return (
                                            NormalPattern::Literal(
                                                self.create_err(ErrorKind::UnknownField, &var.name),
                                            ),
                                            false,
                                            false,
                                        );
                                    };

                                    if var.is_mutable.is_some() {
                                        anyone_wants_mutable = true;
                                    }

                                    let variable_ref = self.create_local_variable_parameter_like(
                                        &var.name,
                                        Option::from(&var.is_mutable),
                                        &field_type.field_type,
                                        false,
                                    );

                                    resolved_elements.push(PatternElement::VariableWithFieldIndex(
                                        variable_ref,
                                        field_index,
                                    ));
                                }
                            }
                            _ => {
                                return (
                                    NormalPattern::Literal(
                                        self.create_err(ErrorKind::CanNotDestructure, variant_name),
                                    ),
                                    false,
                                    false,
                                );
                            }
                        }

                        (
                            NormalPattern::EnumPattern(
                                enum_variant_type_ref,
                                Some(resolved_elements),
                            ),
                            scope_was_pushed,
                            anyone_wants_mutable,
                        )
                    }
                    swamp_ast::DestructuringPattern::Tuple { elements } => {
                        if !scope_was_pushed {
                            self.push_block_scope("enum tuple");
                            scope_was_pushed = true;
                        }
                        assert!(elements.len() > 1, "internal error with tuple");

                        let mut resolved_elements = Vec::new();

                        if let TypeKind::Tuple(fields_in_order) =
                            &*enum_variant_type_ref.payload_type.kind
                        {
                            // For tuples, elements must be in order but can be partial
                            if elements.len() > fields_in_order.len() {
                                return (
                                    NormalPattern::Literal(self.create_err(
                                        ErrorKind::TooManyTupleFields {
                                            max: fields_in_order.len(),
                                            got: elements.len(),
                                        },
                                        variant_name,
                                    )),
                                    false,
                                    false,
                                );
                            }

                            // Only zip with as many fields as we have elements
                            for (tuple_element_index, (element, field_type)) in
                                elements.iter().zip(fields_in_order).enumerate()
                            {
                                match element {
                                    swamp_ast::PatternVariableOrWildcard::Variable(var) => {
                                        if var.is_mutable.is_some() {
                                            anyone_wants_mutable = true;
                                        }

                                        let variable_ref = self
                                            .create_local_variable_parameter_like(
                                                &var.name,
                                                var.is_mutable.as_ref(),
                                                field_type,
                                                false,
                                            );

                                        resolved_elements.push(
                                            PatternElement::VariableWithFieldIndex(
                                                variable_ref,
                                                tuple_element_index,
                                            ),
                                        );
                                    }
                                    swamp_ast::PatternVariableOrWildcard::Wildcard(node) => {
                                        resolved_elements
                                            .push(PatternElement::Wildcard(self.to_node(node)));
                                    }
                                }
                            }
                            (
                                NormalPattern::EnumPattern(
                                    enum_variant_type_ref,
                                    Some(resolved_elements),
                                ),
                                scope_was_pushed,
                                anyone_wants_mutable,
                            )
                        } else {
                            self.add_err(ErrorKind::ExpectedTupleType, node);
                            (NormalPattern::PatternList(vec![]), false, false)
                        }
                    }
                    swamp_ast::DestructuringPattern::None { variable } => {
                        // Single payload variable - capture the entire payload
                        if !scope_was_pushed {
                            self.push_block_scope("enum payload");
                            scope_was_pushed = true;
                        }

                        if variable.is_mutable.is_some() {
                            anyone_wants_mutable = true;
                        }

                        let variable_ref = self.create_local_variable(
                            &variable.name,
                            variable.is_mutable.as_ref(),
                            &enum_variant_type_ref.payload_type,
                            false,
                        );

                        let payload_pattern_element = PatternElement::Variable(variable_ref);

                        (
                            NormalPattern::EnumPattern(
                                enum_variant_type_ref,
                                Some(vec![payload_pattern_element]),
                            ),
                            scope_was_pushed,
                            anyone_wants_mutable,
                        )
                    }
                    swamp_ast::DestructuringPattern::Unit => {
                        // Unit enum variant with no payload - like `Red`, `Green`, `Blue`
                        (
                            NormalPattern::EnumPattern(enum_variant_type_ref, None),
                            scope_was_pushed,
                            anyone_wants_mutable,
                        )
                    }
                }
            }

            swamp_ast::ConcretePattern::Literal(ast_literal) => (
                self.analyze_pattern_literal(node, ast_literal, expected_condition_type),
                false,
                anyone_wants_mutable,
            ),
        }
    }
}
