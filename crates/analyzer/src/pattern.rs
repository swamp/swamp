/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::{Error, ErrorKind};
use swamp_semantic::{NormalPattern, Pattern, PatternElement};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    fn find_variant_in_pattern(
        &self,
        expression_type: &Type,
        ast_name: &swamp_ast::Node,
    ) -> Result<EnumVariantType, Error> {
        let enum_type_ref = match expression_type.underlying() {
            Type::Enum(enum_type_ref) => enum_type_ref,
            _ => Err(self.create_err(ErrorKind::ExpectedEnumInPattern, ast_name))?,
        };

        let variant_name = self.get_text(ast_name).to_string();

        enum_type_ref.get_variant(&variant_name).map_or_else(
            || Err(self.create_err(ErrorKind::UnknownEnumVariantTypeInPattern, ast_name)),
            |found_variant| Ok(found_variant.clone()),
        )
    }

    pub(crate) fn analyze_pattern(
        &mut self,
        ast_pattern: &swamp_ast::Pattern,
        expected_condition_type: &Type,
    ) -> Result<(Pattern, bool, bool), Error> {
        match ast_pattern {
            swamp_ast::Pattern::Wildcard(node) => {
                Ok((Pattern::Wildcard(self.to_node(node)), false, false))
            }
            swamp_ast::Pattern::NormalPattern(node, normal_pattern, maybe_guard) => {
                let (normal_pattern, was_pushed, wanted_mutable) =
                    self.analyze_normal_pattern(node, normal_pattern, expected_condition_type)?;

                let resolved_guard = if let Some(guard_clause) = maybe_guard {
                    match guard_clause {
                        swamp_ast::GuardClause::Wildcard(_) => None,
                        swamp_ast::GuardClause::Expression(clause_expr) => {
                            Some(self.analyze_bool_argument_expression(clause_expr)?)
                        }
                    }
                } else {
                    None
                };
                Ok((
                    Pattern::Normal(normal_pattern, resolved_guard),
                    was_pushed,
                    wanted_mutable,
                ))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn analyze_normal_pattern(
        &mut self,
        node: &swamp_ast::Node,
        ast_normal_pattern: &swamp_ast::NormalPattern,
        expected_condition_type: &Type,
    ) -> Result<(NormalPattern, bool, bool), Error> {
        let mut anyone_wants_mutable = false;
        match ast_normal_pattern {
            swamp_ast::NormalPattern::PatternList(elements) => {
                let mut resolved_elements = Vec::new();
                let mut scope_is_pushed = false;
                for element in elements {
                    match element {
                        swamp_ast::PatternElement::Variable(var) => {
                            if !scope_is_pushed {
                                self.push_block_scope("pattern_list one variable");
                                scope_is_pushed = true;
                            }
                            if var.is_mutable.is_some() {
                                anyone_wants_mutable = true;
                            }
                            let variable_ref = self.create_local_variable(
                                &var.name,
                                Option::from(&var.is_mutable),
                                expected_condition_type,
                            )?;
                            resolved_elements.push(PatternElement::Variable(variable_ref));
                        }
                        swamp_ast::PatternElement::Expression(expr) => {
                            return Err(self.create_err(
                                ErrorKind::ExpressionsNotAllowedInLetPattern,
                                &expr.node,
                            ));
                        }
                        swamp_ast::PatternElement::Wildcard(node) => {
                            resolved_elements.push(PatternElement::Wildcard(self.to_node(node)));
                        }
                    }
                }
                Ok((
                    NormalPattern::PatternList(resolved_elements),
                    scope_is_pushed,
                    anyone_wants_mutable,
                ))
            }

            swamp_ast::NormalPattern::EnumPattern(variant_name, maybe_elements) => {
                let mut scope_was_pushed = false;
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expected_condition_type, variant_name)?;

                if let Some(elements) = maybe_elements {
                    let mut resolved_elements = Vec::new();
                    match enum_variant_type_ref {
                        EnumVariantType::Tuple(ref tuple_type) => {
                            // For tuples, elements must be in order but can be partial
                            if elements.len() > tuple_type.fields_in_order.len() {
                                return Err(self.create_err(
                                    ErrorKind::TooManyTupleFields {
                                        max: tuple_type.fields_in_order.len(),
                                        got: elements.len(),
                                    },
                                    variant_name,
                                ));
                            }

                            if !scope_was_pushed {
                                self.push_block_scope("enum tuple");
                                scope_was_pushed = true;
                            }

                            // Only zip with as many fields as we have elements
                            for (tuple_element_index, (element, field_type)) in
                                elements.iter().zip(&tuple_type.fields_in_order).enumerate()
                            {
                                match element {
                                    swamp_ast::PatternElement::Variable(var) => {
                                        if var.is_mutable.is_some() {
                                            anyone_wants_mutable = true;
                                        }

                                        let variable_ref = self.create_local_variable(
                                            &var.name,
                                            var.is_mutable.as_ref(),
                                            field_type,
                                        )?;
                                        /*
                                        resolved_elements
                                            .push(PatternElement::Variable(variable_ref));

                                         */

                                        resolved_elements.push(
                                            PatternElement::VariableWithFieldIndex(
                                                variable_ref,
                                                tuple_element_index,
                                            ),
                                        );
                                    }
                                    swamp_ast::PatternElement::Wildcard(node) => {
                                        resolved_elements
                                            .push(PatternElement::Wildcard(self.to_node(node)));
                                    }
                                    swamp_ast::PatternElement::Expression(expr) => {
                                        return Err(self.create_err(
                                            ErrorKind::ExpressionsNotAllowedInLetPattern,
                                            &expr.node,
                                        ));
                                    }
                                }
                            }
                        }
                        EnumVariantType::Struct(ref struct_type) => {
                            if !scope_was_pushed {
                                self.push_block_scope("enum struct");
                                scope_was_pushed = true;
                            }
                            // For structs, can match any subset of fields in any order
                            for element in elements {
                                match element {
                                    swamp_ast::PatternElement::Variable(var) => {
                                        let var_name_str = self.get_text(&var.name).to_string();
                                        // Check if the field exists
                                        let field_index = struct_type
                                            .anon_struct
                                            .field_name_sorted_fields
                                            .get_index(&var_name_str)
                                            .ok_or_else(|| {
                                                self.create_err(ErrorKind::UnknownField, &var.name)
                                            })?;

                                        let field_type = struct_type
                                            .anon_struct
                                            .field_name_sorted_fields
                                            .get(&var_name_str)
                                            .ok_or_else(|| {
                                                self.create_err(ErrorKind::UnknownField, &var.name)
                                            })?;

                                        if var.is_mutable.is_some() {
                                            anyone_wants_mutable = true;
                                        }

                                        let variable_ref = self.create_local_variable(
                                            &var.name,
                                            Option::from(&var.is_mutable),
                                            &field_type.field_type,
                                        )?;

                                        resolved_elements.push(
                                            PatternElement::VariableWithFieldIndex(
                                                variable_ref,
                                                field_index,
                                            ),
                                        );
                                    }
                                    swamp_ast::PatternElement::Wildcard(node) => {
                                        resolved_elements
                                            .push(PatternElement::Wildcard(self.to_node(node)));
                                    }
                                    swamp_ast::PatternElement::Expression(expr) => {
                                        return Err(self.create_err(
                                            ErrorKind::ExpressionsNotAllowedInLetPattern,
                                            &expr.node,
                                        ));
                                    }
                                }
                            }
                        }
                        EnumVariantType::Nothing(_) => {
                            if !elements.is_empty() {
                                return Err(self
                                    .create_err(ErrorKind::EnumVariantHasNoFields, variant_name));
                            }
                        }
                    }

                    Ok((
                        NormalPattern::EnumPattern(enum_variant_type_ref, Some(resolved_elements)),
                        scope_was_pushed,
                        anyone_wants_mutable,
                    ))
                } else {
                    Ok((
                        NormalPattern::EnumPattern(enum_variant_type_ref, None),
                        false,
                        anyone_wants_mutable,
                    ))
                }
            }

            swamp_ast::NormalPattern::Literal(ast_literal) => Ok((
                self.analyze_pattern_literal(node, ast_literal, expected_condition_type)?,
                false,
                anyone_wants_mutable,
            )),
        }
    }
}
