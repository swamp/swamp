/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use swamp_types::prelude::{Signature, TypeForParameter};
use swamp_types::TypeRef;

#[derive(Default)]
pub(crate) struct TypeAnalyzeContext {
    pub allow_ephemeral: bool,
}

impl TypeAnalyzeContext {
    pub(crate) const fn new_ephemeral() -> Self {
        Self {
            allow_ephemeral: true,
        }
    }

    pub const fn allows_ephemeral(&self) -> bool {
        self.allow_ephemeral
    }
}


impl Analyzer<'_> {
    /// # Errors
    ///
    pub fn analyze_map_type(
        &mut self,
        ast_key_type: &swamp_ast::Type,
        ast_value_type: &swamp_ast::Type,
    ) -> (TypeRef, TypeRef) {
        let key_type = self.analyze_type(ast_key_type, &TypeAnalyzeContext::default());
        let value_type = self.analyze_type(ast_value_type, &TypeAnalyzeContext::default());

        // Using TypeCache to ensure proper type comparison and creation
        (key_type, value_type)
    }

    /// # Errors
    ///
    pub fn analyze_key_and_value_type(
        &mut self,
        ast_key_type: &swamp_ast::Type,
        ast_value_type: &swamp_ast::Type,
    ) -> (TypeRef, TypeRef) {
        let key_type = self.analyze_type(ast_key_type, &TypeAnalyzeContext::default());
        let value_type = self.analyze_type(ast_value_type, &TypeAnalyzeContext::default());

        // Using TypeCache to ensure proper type comparison and creation
        (key_type, value_type)
    }

    /// # Errors
    ///
    /// # Panics
    pub fn analyze_type(&mut self, ast_type: &swamp_ast::Type, ctx: &TypeAnalyzeContext) -> TypeRef {
        match ast_type {
            swamp_ast::Type::AnonymousStruct(ast_struct) => {
                let struct_ref = self.analyze_anonymous_struct_type(ast_struct, ctx);
                // Use TypeCache to create the anonymous struct type
                let anon_struct_type = self.shared.state.types.anonymous_struct(struct_ref);

                // Generate default functions for the new anonymous struct type
                let default_node = swamp_ast::Node::default();
                self.add_default_functions(&anon_struct_type, &default_node);

                anon_struct_type
            }
            swamp_ast::Type::FixedCapacityArray(ast_type, fixed_size) => {
                let element_type = self.analyze_type(ast_type, &TypeAnalyzeContext::default());
                let int_str = self.get_text(fixed_size);
                let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;

                // Use TypeCache for fixed array creation
                let array_type = self
                    .shared
                    .state
                    .types
                    .fixed_array(&element_type, int_value);

                // Generate default functions for the new array type
                self.add_default_functions(&array_type, fixed_size);

                array_type
            }
            swamp_ast::Type::Slice(ast_type) => {
                let element_type = self.analyze_type(ast_type, &TypeAnalyzeContext::default());
                // Use TypeCache for slice view creation
                let slice_type = self.shared.state.types.slice_view(&element_type);

                // Generate default functions for the new slice type
                let default_node = swamp_ast::Node::default();
                self.add_default_functions(&slice_type, &default_node);

                slice_type
            }

            swamp_ast::Type::FixedCapacityMap(ast_key_type, ast_value_type, fixed_size) => {
                let (key_type, value_type) =
                    self.analyze_key_and_value_type(ast_key_type, ast_value_type);

                let int_str = self.get_text(fixed_size);
                let int_value = Self::str_to_unsigned_int(int_str).unwrap() as usize;

                // Use TypeCache for map storage creation
                let map_type =
                    self.shared
                        .state
                        .types
                        .map_storage(&key_type, &value_type, int_value);

                // Generate default functions for the new map type
                self.add_default_functions(&map_type, fixed_size);

                map_type
            }
            swamp_ast::Type::DynamicLengthMap(ast_key_type, ast_value_type) => {
                let (key_type, value_type) =
                    self.analyze_key_and_value_type(ast_key_type, ast_value_type);

                // Use TypeCache for dynamic map view creation
                let map_view_type = self
                    .shared
                    .state
                    .types
                    .dynamic_map_view(&key_type, &value_type);

                // Generate default functions for the new map view type
                let default_node = swamp_ast::Node::default();
                self.add_default_functions(&map_view_type, &default_node);

                map_view_type
            }

            swamp_ast::Type::Tuple(types) => {
                let analyzed_types = self.analyze_types(types, &TypeAnalyzeContext::default());
                // Use TypeCache for tuple creation
                let tuple_type = self.shared.state.types.tuple(analyzed_types);

                // Generate default functions for the new tuple type
                let default_node = swamp_ast::Node::default();
                self.add_default_functions(&tuple_type, &default_node);

                tuple_type
            }
            swamp_ast::Type::Named(ast_type_reference) => {
                // Named types need to be analyzed through the TypeCache as well
                self.analyze_named_type(ast_type_reference)
            }
            swamp_ast::Type::Unit => self.shared.state.types.unit(),
            swamp_ast::Type::Optional(inner_type_ast, _node) => {
                let inner_resolved_type = self.analyze_type(inner_type_ast, &TypeAnalyzeContext::default());
                // Use TypeCache for optional type creation
                let optional_type = self.shared.state.types.optional(&inner_resolved_type);

                // Generate default functions for the new optional type
                let default_node = swamp_ast::Node::default();
                self.add_default_functions(&optional_type, &default_node);

                optional_type
            }
            swamp_ast::Type::Function(parameters, return_type) => {
                let parameter_types = self.analyze_param_types(parameters);

                let resolved_return_type = self.analyze_type(return_type, ctx);
                let signature = Signature {
                    parameters: parameter_types,
                    return_type: resolved_return_type,
                };
                // Use TypeCache for function type creation
                self.shared.state.types.function(signature)
            }
        }
    }

    pub(crate) fn analyze_types(&mut self, types: &[swamp_ast::Type], ctx: &TypeAnalyzeContext) -> Vec<TypeRef> {
        let mut resolved_types = Vec::new();
        for some_type in types {
            resolved_types.push(self.analyze_type(some_type, ctx));
        }
        resolved_types
    }

    fn analyze_param_types(
        &mut self,
        type_for_parameters: &Vec<swamp_ast::TypeForParameter>,
    ) -> Vec<TypeForParameter> {
        let mut vec = Vec::new();
        for x in type_for_parameters {
            vec.push(TypeForParameter {
                name: String::new(),
                // Use TypeCache to ensure the resolved type is properly created
                resolved_type: self.analyze_type(&x.ast_type, &TypeAnalyzeContext::default()),
                is_mutable: x.is_mutable,
                node: None,
            });
        }

        vec
    }
}
