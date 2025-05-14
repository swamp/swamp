/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::Error;
use swamp_ast::QualifiedTypeIdentifier;
use swamp_types::{Signature, Type, TypeForParameter};

impl Analyzer<'_> {
    /// # Errors
    ///
    pub fn analyze_map_type(
        &mut self,
        ast_key_type: &swamp_ast::Type,
        ast_value_type: &swamp_ast::Type,
    ) -> Result<(Type, Type), Error> {
        // TODO: Check for an existing map type with exact same type

        let key_type = self.analyze_type(ast_key_type)?;
        let value_type = self.analyze_type(ast_value_type)?;

        Ok((key_type, value_type))
    }

    /// # Errors
    ///
    pub fn analyze_slice_type(&mut self, ast_type: &swamp_ast::Type) -> Result<Type, Error> {
        self.analyze_type(ast_type)
    }

    /// # Errors
    ///
    /// # Panics
    /// if `Vec` wasn't added in core.
    pub fn analyze_type(&mut self, ast_type: &swamp_ast::Type) -> Result<Type, Error> {
        let resolved = match ast_type {
            swamp_ast::Type::AnonymousStruct(ast_struct) => {
                let struct_ref = self.analyze_anonymous_struct_type(ast_struct)?;
                Type::AnonymousStruct(struct_ref)
            }
            swamp_ast::Type::Slice(ast_type, maybe_fixed_size) => {
                // TODO: maybe_fixed_size
                let element_type = self.analyze_slice_type(ast_type)?;
                let int_str = self.get_text(&maybe_fixed_size.clone().unwrap());
                let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;

                Type::Slice(Box::new(element_type), int_value)
                /*
                let vec_blueprint = self
                    .shared
                    .core_symbol_table
                    .get_blueprint("Vec")
                    .unwrap()
                    .clone();
                self.shared
                    .state
                    .instantiator
                    .instantiate_blueprint_and_members(&vec_blueprint, &[analyzed_element_type])?

                 */
            }
            swamp_ast::Type::SlicePair(key_type, value_type, maybe_fixed_size) => {
                // TODO: maybe fixed size
                let analyzed_key_type = self.analyze_slice_type(key_type)?;
                let analyzed_value_type = self.analyze_slice_type(value_type)?;
                let map_blueprint = self
                    .shared
                    .core_symbol_table
                    .get_blueprint("Map")
                    .unwrap()
                    .clone();

                self.shared
                    .state
                    .instantiator
                    .instantiate_blueprint_and_members(
                        &map_blueprint,
                        &[analyzed_key_type, analyzed_value_type],
                    )?
            }
            swamp_ast::Type::Tuple(types) => Type::Tuple(self.analyze_types(types)?),
            swamp_ast::Type::Named(ast_type_reference) => {
                if let Some(found_special_type) =
                    self.analyze_maybe_special_type(ast_type_reference)
                {
                    found_special_type
                } else {
                    self.analyze_named_type(ast_type_reference)?
                }
            }
            swamp_ast::Type::Unit => Type::Unit,
            swamp_ast::Type::Optional(inner_type_ast, _node) => {
                let inner_resolved_type = self.analyze_type(inner_type_ast)?;
                Type::Optional(Box::from(inner_resolved_type))
            }
            swamp_ast::Type::Function(parameters, return_type) => {
                let parameter_types = self.analyze_param_types(parameters)?;

                let resolved_return_type = self.analyze_type(return_type)?;
                Type::Function(Signature {
                    parameters: parameter_types,
                    return_type: Box::new(resolved_return_type),
                })
            }
        };

        Ok(resolved)
    }

    pub(crate) fn analyze_types(&mut self, types: &[swamp_ast::Type]) -> Result<Vec<Type>, Error> {
        let mut resolved_types = Vec::new();
        for some_type in types {
            resolved_types.push(self.analyze_type(some_type)?);
        }
        Ok(resolved_types)
    }

    fn analyze_param_types(
        &mut self,
        type_for_parameters: &Vec<swamp_ast::TypeForParameter>,
    ) -> Result<Vec<TypeForParameter>, Error> {
        let mut vec = Vec::new();
        for x in type_for_parameters {
            vec.push(TypeForParameter {
                name: String::new(),
                resolved_type: self.analyze_type(&x.ast_type)?,
                is_mutable: x.is_mutable,
                node: None,
            });
        }

        Ok(vec)
    }

    fn analyze_maybe_special_type(&self, type_name: &QualifiedTypeIdentifier) -> Option<Type> {
        let text = self.get_text(&type_name.name.0);
        match text {
            _ => None,
        }
    }
}
