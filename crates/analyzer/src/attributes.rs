/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use swamp_attributes::{Attribute, AttributeArg, AttributeIdentifier, AttributeValue, Attributes};

impl Analyzer<'_> {
    #[must_use]
    pub fn analyze_attributes(&self, attributes: &[swamp_ast::Attribute]) -> Attributes {
        let items = attributes
            .iter()
            .map(|swamp_attr| self.analyze_attribute(swamp_attr))
            .collect();

        Attributes(items)
    }

    #[must_use]
    pub fn analyze_attribute(&self, parsed: &swamp_ast::Attribute) -> Attribute {
        Attribute {
            path: self.analyze_attribute_identifier(&parsed.path),
            args: self.analyze_attribute_arguments(&parsed.args),
            node: self.to_node(&parsed.node),
        }
    }

    fn analyze_attribute_identifier(
        &self,
        attribute_identifier: &swamp_ast::QualifiedIdentifier,
    ) -> AttributeIdentifier {
        let text = self.get_text(&attribute_identifier.name).to_string();
        AttributeIdentifier { name: text }
    }

    fn analyze_attribute_arguments(&self, args: &[swamp_ast::AttributeArg]) -> Vec<AttributeArg> {
        args.iter()
            .map(|ast_arg| match ast_arg {
                swamp_ast::AttributeArg::Path(attribute_identifier) => {
                    let analyzed_attribute_identifier =
                        self.analyze_attribute_identifier(attribute_identifier);
                    AttributeArg::Path(analyzed_attribute_identifier)
                }
                swamp_ast::AttributeArg::Literal(attribute_value) => match attribute_value {
                    swamp_ast::AttributeValue::Literal(ast_node, ast_literal_kind) => {
                        let text = self.get_text(ast_node).to_string();
                        let converted_value = match ast_literal_kind {
                            swamp_ast::AttributeLiteralKind::Int => {
                                let i = Self::str_to_int(&text).unwrap();
                                AttributeValue::Int(i)
                            }
                            swamp_ast::AttributeLiteralKind::String(s) => {
                                AttributeValue::String(s.clone())
                            }
                            swamp_ast::AttributeLiteralKind::Bool => {
                                let b = Self::str_to_bool(&text).unwrap();
                                AttributeValue::Bool(b)
                            }
                        };
                        AttributeArg::Literal(converted_value)
                    }
                    swamp_ast::AttributeValue::Path(attribute_identifier) => {
                        let analyzed_attribute_identifier =
                            self.analyze_attribute_identifier(attribute_identifier);
                        AttributeArg::Path(analyzed_attribute_identifier)
                    }
                    swamp_ast::AttributeValue::Function(attribute_identifier, args) => {
                        let analyzed_attribute_identifier =
                            self.analyze_attribute_identifier(attribute_identifier);
                        let analyzed_args = self.analyze_attribute_arguments(args);

                        AttributeArg::Function(analyzed_attribute_identifier, analyzed_args)
                    }
                },
                swamp_ast::AttributeArg::Function(attribute_identifier, arguments) => {
                    let attribute_identifier =
                        self.analyze_attribute_identifier(attribute_identifier);
                    let args = self.analyze_attribute_arguments(arguments);
                    AttributeArg::Function(attribute_identifier, args)
                }
            })
            .collect()
    }
}
