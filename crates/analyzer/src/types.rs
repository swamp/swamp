/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::Error;
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
    pub fn analyze_slice_pair_key_and_value_type(
        &mut self,
        ast_key_type: &swamp_ast::Type,
        ast_value_type: &swamp_ast::Type,
    ) -> Result<(Type, Type), Error> {
        let key_type = self.analyze_type(ast_key_type)?;
        let value_type = self.analyze_type(ast_value_type)?;

        Ok((key_type, value_type))
    }

    /// # Errors
    ///
    pub fn analyze_type(&mut self, ast_type: &swamp_ast::Type) -> Result<Type, Error> {
        let resolved = match ast_type {
            swamp_ast::Type::AnonymousStruct(ast_struct) => {
                let struct_ref = self.analyze_anonymous_struct_type(ast_struct)?;
                Type::AnonymousStruct(struct_ref)
            }
            swamp_ast::Type::Slice(ast_type, maybe_fixed_size) => {
                let element_type = self.analyze_slice_type(ast_type)?;
                if let Some(fixed_size) = maybe_fixed_size {
                    let int_str = self.get_text(&fixed_size);
                    let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;

                    Type::VecStorage(Box::new(element_type), int_value)
                } else {
                    Type::Vec(Box::new(element_type))
                }
            }
            swamp_ast::Type::SlicePair(ast_key_type, ast_value_type, maybe_fixed_size) => {
                let (key_type, value_type) =
                    self.analyze_slice_pair_key_and_value_type(ast_key_type, ast_value_type)?;
                if let Some(fixed_size) = maybe_fixed_size {
                    let int_str = self.get_text(&fixed_size);
                    let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;

                    Type::MapStorage(Box::new(key_type), Box::new(value_type), int_value)
                } else {
                    Type::Map(Box::new(key_type), Box::new(value_type))
                }
            }
            swamp_ast::Type::Tuple(types) => Type::Tuple(self.analyze_types(types)?),
            swamp_ast::Type::Named(ast_type_reference) => {
                if let Some(found_special_type) =
                    self.analyze_maybe_special_type(ast_type_reference)?
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

    fn analyze_maybe_special_type(
        &mut self,
        type_name: &swamp_ast::QualifiedTypeIdentifier,
    ) -> Result<Option<Type>, Error> {
        let text = self.get_text(&type_name.name.0);
        match text {
            /*
            "Vec" => {
                let len = type_name.generic_params.len();
                let ty = self.analyze_type(type_name.generic_params[0].get_type())?;
                if len == 1 {
                    Ok(Some(Type::Vec(Box::new(ty))))
                } else if len == 2 {
                    // storage type
                    let size_node = type_name.generic_params[1].get_unsigned_int_node();
                    let int_str = self.get_text(&size_node);
                    let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;
                    Ok(Some(Type::VecStorage(Box::new(ty), int_value)))
                } else {
                    panic!("strange Vec type")
                }
            }

             */
            // TODO: use for Sparse, Grid, Queue, etc
            _ => Ok(None),
        }
    }
}
