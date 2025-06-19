/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::Error;
use swamp_types::TypeRef;
use swamp_types::prelude::{Signature, TypeForParameter};

impl Analyzer<'_> {
    /// # Errors
    ///
    pub fn analyze_map_type(
        &mut self,
        ast_key_type: &swamp_ast::Type,
        ast_value_type: &swamp_ast::Type,
    ) -> Result<(TypeRef, TypeRef), Error> {
        let key_type = self.analyze_type(ast_key_type)?;
        let value_type = self.analyze_type(ast_value_type)?;

        // Using TypeCache to ensure proper type comparison and creation
        Ok((key_type, value_type))
    }

    /// # Errors
    ///
    pub fn analyze_key_and_value_type(
        &mut self,
        ast_key_type: &swamp_ast::Type,
        ast_value_type: &swamp_ast::Type,
    ) -> (TypeRef, TypeRef) {
        let key_type = self.analyze_type(ast_key_type);
        let value_type = self.analyze_type(ast_value_type);

        // Using TypeCache to ensure proper type comparison and creation
        (key_type, value_type)
    }

    /// # Errors
    ///
    /// # Panics
    pub fn analyze_type(&mut self, ast_type: &swamp_ast::Type) -> TypeRef {
        let resolved = match ast_type {
            swamp_ast::Type::AnonymousStruct(ast_struct) => {
                let struct_ref = self.analyze_anonymous_struct_type(ast_struct)?;
                // Use TypeCache to create the anonymous struct type
                self.shared.state.types.anonymous_struct(struct_ref)
            }
            swamp_ast::Type::FixedCapacityArray(ast_type, fixed_size) => {
                let element_type = self.analyze_type(ast_type);
                let int_str = self.get_text(fixed_size);
                let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;

                // Use TypeCache for fixed array creation
                self.shared
                    .state
                    .types
                    .fixed_array(&element_type, int_value)
            }
            swamp_ast::Type::Slice(ast_type) => {
                let element_type = self.analyze_type(ast_type);
                // Use TypeCache for slice view creation
                self.shared.state.types.slice_view(&element_type)
            }

            swamp_ast::Type::FixedCapacityMap(ast_key_type, ast_value_type, fixed_size) => {
                let (key_type, value_type) =
                    self.analyze_key_and_value_type(ast_key_type, ast_value_type);

                let int_str = self.get_text(fixed_size);
                let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;

                // Use TypeCache for map storage creation
                self.shared
                    .state
                    .types
                    .map_storage(&key_type, &value_type, int_value)
            }
            swamp_ast::Type::DynamicLengthMap(ast_key_type, ast_value_type) => {
                let (key_type, value_type) =
                    self.analyze_key_and_value_type(ast_key_type, ast_value_type)?;

                // Use TypeCache for dynamic map view creation
                self.shared
                    .state
                    .types
                    .dynamic_map_view(&key_type, &value_type)
            }

            swamp_ast::Type::Tuple(types) => {
                let analyzed_types = self.analyze_types(types)?;
                // Use TypeCache for tuple creation
                self.shared.state.types.tuple(analyzed_types)
            }
            swamp_ast::Type::Named(ast_type_reference) => {
                // Named types need to be analyzed through the TypeCache as well
                self.analyze_named_type(ast_type_reference)?
            }
            swamp_ast::Type::Unit => self.shared.state.types.unit(),
            swamp_ast::Type::Optional(inner_type_ast, _node) => {
                let inner_resolved_type = self.analyze_type(inner_type_ast)?;
                // Use TypeCache for optional type creation
                self.shared.state.types.optional(&inner_resolved_type)
            }
            swamp_ast::Type::Function(parameters, return_type) => {
                let parameter_types = self.analyze_param_types(parameters)?;

                let resolved_return_type = self.analyze_type(return_type)?;
                let signature = Signature {
                    parameters: parameter_types,
                    return_type: resolved_return_type,
                };
                // Use TypeCache for function type creation
                self.shared.state.types.function(signature)
            }
        };

        Ok(resolved)
    }

    pub(crate) fn analyze_types(
        &mut self,
        types: &[swamp_ast::Type],
    ) -> Result<Vec<TypeRef>, Error> {
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
                // Use TypeCache to ensure the resolved type is properly created
                resolved_type: self.analyze_type(&x.ast_type)?,
                is_mutable: x.is_mutable,
                node: None,
            });
        }

        Ok(vec)
    }
}
