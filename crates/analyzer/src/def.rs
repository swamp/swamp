/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::{Error, ErrorKind};
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_semantic::{
    ExternalFunctionDefinition, ExternalFunctionId, Function, InternalFunctionDefinition,
    LocalIdentifier, UseItem,
};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    fn general_import(
        &mut self,
        path: &[String],
        import_items: &swamp_ast::ImportItems,
        node: &swamp_ast::Node,
    ) -> Result<(), Error> {
        let found_module = self
            .shared
            .get_module(path)
            .ok_or_else(|| self.create_err(ErrorKind::UnknownModule, node))?
            .clone();

        match import_items {
            swamp_ast::ImportItems::Nothing => {
                let last_name = path.last().unwrap();
                if self
                    .shared
                    .lookup_table
                    .get_module_link(last_name)
                    .is_none()
                {
                    self.shared
                        .lookup_table
                        .add_module_link(last_name, found_module.clone())
                        .map_err(|err| self.create_err(ErrorKind::SemanticError(err), node))?;
                }
            }
            swamp_ast::ImportItems::Items(items) => {
                for ast_items in items {
                    match ast_items {
                        swamp_ast::ImportItem::Identifier(node) => {
                            let ident_resolved_node = self.to_node(&node.0);
                            let ident = UseItem::Identifier(ident_resolved_node.clone());
                            let ident_text =
                                self.get_text_resolved(&ident_resolved_node).to_string();
                            if let Some(found_symbol) =
                                found_module.symbol_table.get_symbol(&ident_text)
                            {
                                self.shared
                                    .lookup_table
                                    .add_symbol(&ident_text, found_symbol.clone())
                                    .map_err(|err| {
                                        self.create_err(ErrorKind::SemanticError(err), &node.0)
                                    })?;
                            } else {
                                return Err(self.create_err_resolved(
                                    ErrorKind::UnknownTypeReference,
                                    &ident_resolved_node,
                                ));
                            }
                            ident
                        }
                        swamp_ast::ImportItem::Type(node) => {
                            let ident_resolved_node = self.to_node(&node.0);
                            let ident_text =
                                self.get_text_resolved(&ident_resolved_node).to_string();
                            if let Some(found_symbol) =
                                found_module.symbol_table.get_symbol(&ident_text)
                            {
                                self.shared
                                    .lookup_table
                                    .add_symbol(&ident_text, found_symbol.clone())
                                    .map_err(|err| {
                                        self.create_err(ErrorKind::SemanticError(err), &node.0)
                                    })?;
                            } else {
                                return Err(self.create_err_resolved(
                                    ErrorKind::UnknownTypeReference,
                                    &ident_resolved_node,
                                ));
                            }
                            UseItem::TypeIdentifier(self.to_node(&node.0))
                        }
                    };
                }
            }
            swamp_ast::ImportItems::All => {
                self.shared
                    .lookup_table
                    .extend_from(&found_module.symbol_table)?;
            }
        }

        Ok(())
    }

    fn analyze_mod_definition(&mut self, mod_definition: &swamp_ast::Mod) -> Result<(), Error> {
        let mut path = Vec::new();
        for ast_node in &mod_definition.module_path.0 {
            path.push(self.get_text(ast_node).to_string());
        }

        let mut nodes_copy = path.clone();
        nodes_copy.insert(0, "crate".to_string());

        self.general_import(
            &nodes_copy,
            &mod_definition.items,
            &mod_definition.module_path.0[0],
        )
    }

    fn analyze_use_definition(&mut self, use_definition: &swamp_ast::Use) -> Result<(), Error> {
        let mut nodes = Vec::new();
        for ast_node in &use_definition.module_path.0 {
            nodes.push(self.to_node(ast_node));
        }

        let path: Vec<String> = nodes
            .iter()
            .map(|node| {
                let text = self.get_text_resolved(node);
                text.to_string()
            })
            .collect();

        self.general_import(
            &path,
            &use_definition.items,
            &use_definition.module_path.0[0],
        )
    }

    fn analyze_enum_type_definition(
        &mut self,
        enum_type_name: &swamp_ast::LocalTypeIdentifierWithOptionalTypeVariables,
        ast_variants: &[swamp_ast::EnumVariantType],
    ) -> Result<(), Error> {
        let mut resolved_variants = SeqMap::new();

        let mut new_enum_type = EnumType {
            name: self.to_node(&enum_type_name.name),
            assigned_name: self.get_text(&enum_type_name.name).to_string(),
            module_path: vec![],
            variants: SeqMap::default(),
            instantiated_type_parameters: Vec::default(),
        };

        for (container_index_usize, ast_variant_type) in ast_variants.iter().enumerate() {
            let variant_name_node = match ast_variant_type {
                swamp_ast::EnumVariantType::Simple(name) => name,
                swamp_ast::EnumVariantType::Tuple(name, _) => name,
                swamp_ast::EnumVariantType::Struct(name, _) => name,
            };

            let common = EnumVariantCommon {
                name: self.to_node(variant_name_node),
                assigned_name: self.get_text(variant_name_node).to_string(),
                container_index: container_index_usize as u8,
            };

            let variant_type = match ast_variant_type {
                swamp_ast::EnumVariantType::Simple(_variant_name_node) => {
                    let simple_ref = EnumVariantSimpleType { common };
                    EnumVariantType::Nothing(simple_ref)
                }
                swamp_ast::EnumVariantType::Tuple(_variant_name_node, types) => {
                    let mut vec = Vec::new();
                    for tuple_type in types {
                        let resolved_type = self.analyze_type(tuple_type)?;
                        vec.push(resolved_type);
                    }

                    let resolved_tuple_type = EnumVariantTupleType {
                        common,
                        fields_in_order: vec,
                    };

                    EnumVariantType::Tuple(resolved_tuple_type)
                }
                swamp_ast::EnumVariantType::Struct(_variant_name_node, ast_struct_fields) => {
                    let mut fields = SeqMap::new();

                    for field_with_type in &ast_struct_fields.fields {
                        // TODO: Check the index
                        let resolved_type = self.analyze_type(&field_with_type.field_type)?;
                        let field_name_str =
                            self.get_text(&field_with_type.field_name.0).to_string();

                        let resolved_field = StructTypeField {
                            identifier: Some(self.to_node(&field_with_type.field_name.0)),
                            field_type: resolved_type,
                        };

                        fields.insert(field_name_str, resolved_field).map_err(|_| {
                            self.create_err(
                                ErrorKind::DuplicateFieldName,
                                &field_with_type.field_name.0,
                            )
                        })?;
                    }

                    let enum_variant_struct_type = EnumVariantStructType {
                        common,
                        anon_struct: AnonymousStructType::new(fields),
                    };

                    EnumVariantType::Struct(enum_variant_struct_type)
                }
            };

            let variant_name_str = self.get_text(variant_name_node).to_string();

            resolved_variants
                .insert(variant_name_str, variant_type)
                .map_err(|_| self.create_err(ErrorKind::DuplicateFieldName, variant_name_node))?;
        }

        new_enum_type.variants = resolved_variants;

        self.shared
            .definition_table
            .add_enum_type(new_enum_type.clone())
            .map_err(|err| self.create_err(ErrorKind::SemanticError(err), &enum_type_name.name))?;

        self.shared
            .lookup_table
            .add_enum_type_link(new_enum_type.clone())
            .map_err(|err| self.create_err(ErrorKind::SemanticError(err), &enum_type_name.name))?;

        self.add_default_functions(&Type::Enum(new_enum_type), &enum_type_name.name);
        Ok(())
    }

    /// # Errors
    ///
    pub fn analyze_alias_type_definition(
        &mut self,
        ast_alias: &swamp_ast::AliasType,
    ) -> Result<AliasType, Error> {
        let resolved_type = self.analyze_type(&ast_alias.referenced_type)?;

        let alias_name_str = self.get_text(&ast_alias.identifier.0).to_string();
        let resolved_alias = AliasType {
            name: self.to_node(&ast_alias.identifier.0),
            assigned_name: alias_name_str,
            referenced_type: resolved_type,
        };

        let resolved_alias_ref = self
            .shared
            .definition_table
            .add_alias(resolved_alias)
            .map_err(|err| {
                self.create_err(ErrorKind::SemanticError(err), &ast_alias.identifier.0)
            })?;
        self.shared
            .lookup_table
            .add_alias_link(resolved_alias_ref.clone())
            .map_err(|err| {
                self.create_err(ErrorKind::SemanticError(err), &ast_alias.identifier.0)
            })?;

        Ok(resolved_alias_ref)
    }

    /// # Errors
    ///
    pub fn analyze_anonymous_struct_type(
        &mut self,
        ast_struct: &swamp_ast::AnonymousStructType,
    ) -> Result<AnonymousStructType, Error> {
        let resolved_fields = self.analyze_anonymous_struct_type_fields(&ast_struct.fields)?;

        let resolved_anon_struct = AnonymousStructType::new_and_sort_fields(&resolved_fields);

        Ok(resolved_anon_struct)
    }

    /// # Errors
    ///
    pub fn analyze_anonymous_struct_type_fields(
        &mut self,
        ast_struct_fields: &[swamp_ast::StructTypeField],
    ) -> Result<SeqMap<String, StructTypeField>, Error> {
        let mut resolved_fields = SeqMap::new();

        for field_name_and_type in ast_struct_fields {
            let resolved_type = self.analyze_type(&field_name_and_type.field_type)?;
            let name_string = self.get_text(&field_name_and_type.field_name.0).to_string();

            let field_type = StructTypeField {
                identifier: Some(self.to_node(&field_name_and_type.field_name.0)),
                field_type: resolved_type,
            };

            resolved_fields
                .insert(name_string, field_type)
                .map_err(|_| {
                    self.create_err(
                        ErrorKind::DuplicateFieldName,
                        &field_name_and_type.field_name.0,
                    )
                })?;
        }

        Ok(resolved_fields)
    }

    /// # Errors
    ///
    pub fn analyze_named_struct_type_definition(
        &mut self,
        ast_struct_def: &swamp_ast::NamedStructDef,
    ) -> Result<(), Error> {
        let struct_name_str = self.get_text(&ast_struct_def.identifier.name).to_string();

        let fields =
            self.analyze_anonymous_struct_type_fields(&ast_struct_def.struct_type.fields)?;

        let analyzed_anonymous_struct = AnonymousStructType::new(fields); // the order encountered in source should be kept

        let named_struct_type = NamedStructType {
            name: self.to_node(&ast_struct_def.identifier.name),
            anon_struct_type: analyzed_anonymous_struct,
            assigned_name: struct_name_str,
            module_path: self.shared.definition_table.module_path(),
            instantiated_type_parameters: Vec::default(),
        };

        let struct_ref = self
            .shared
            .definition_table
            .add_struct(named_struct_type.clone())
            .map_err(|err| {
                self.create_err(
                    ErrorKind::SemanticError(err),
                    &ast_struct_def.identifier.name,
                )
            })?;

        self.shared
            .lookup_table
            .add_struct_link(struct_ref)
            .map_err(|err| {
                self.create_err(
                    ErrorKind::SemanticError(err),
                    &ast_struct_def.identifier.name,
                )
            })?;

        self.add_default_functions(
            &Type::NamedStruct(named_struct_type),
            &ast_struct_def.identifier.name,
        );
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn analyze_function_definition(
        &mut self,
        function: &swamp_ast::Function,
        attributes: &Attributes,
    ) -> Result<Function, Error> {
        let func = match &function {
            swamp_ast::Function::Internal(function_data) => {
                let parameters = self.analyze_parameters(&function_data.declaration.params)?;
                let return_type = if let Some(found) = &function_data.declaration.return_type {
                    self.analyze_type(found)?
                } else {
                    Type::Unit
                };

                // Set up scope for function body
                for param in &parameters {
                    let param_type = &param.resolved_type;
                    let actual_type = if param.node.as_ref().unwrap().is_mutable() {
                        Type::MutableReference(Box::new(param_type.clone()))
                    } else {
                        param_type.clone()
                    };
                    self.create_parameter_resolved(
                        &param.node.as_ref().unwrap().name,
                        param.node.as_ref().unwrap().is_mutable.as_ref(),
                        &actual_type,
                    )?;
                }
                let function_name = self
                    .get_text(&function_data.declaration.name)
                    .trim()
                    .to_string();
                let statements =
                    self.analyze_function_body_expression(&function_data.body, &return_type)?;

                let internal = InternalFunctionDefinition {
                    signature: Signature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    body: statements,
                    name: LocalIdentifier(self.to_node(&function_data.declaration.name)),
                    assigned_name: self.get_text(&function_data.declaration.name).to_string(),
                    associated_with_type: None,
                    defined_in_module_path: self.module_path.clone(),
                    function_variables: self.function_variables.clone(),
                    parameters: self.function_parameters.clone(),
                    program_unique_id: self.shared.state.allocate_internal_function_id(),
                    attributes: attributes.clone(),
                };

                let function_ref = self
                    .shared
                    .definition_table
                    .add_internal_function(&function_name, internal)
                    .map_err(|err| {
                        self.create_err(
                            ErrorKind::SemanticError(err),
                            &function_data.declaration.name,
                        )
                    })?;

                self.shared
                    .lookup_table
                    .add_internal_function_link(&function_name, function_ref.clone())
                    .map_err(|err| {
                        self.create_err(
                            ErrorKind::SemanticError(err),
                            &function_data.declaration.name,
                        )
                    })?;

                Function::Internal(function_ref)
            }
            swamp_ast::Function::External(int_node, ast_signature) => {
                let parameters = self.analyze_parameters(&ast_signature.params)?;
                let external_return_type = if let Some(found) = &ast_signature.return_type {
                    self.analyze_type(found)?
                } else {
                    Type::Unit
                };

                let return_type = external_return_type;

                let int_string = self.get_text(int_node);
                let external_function_id_int = Self::str_to_int(int_string).unwrap() as u32;

                let external_function_id = external_function_id_int as ExternalFunctionId; //self.shared.state.allocate_external_function_id();

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&ast_signature.name).to_string(),
                    signature: Signature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    name: Some(self.to_node(&ast_signature.name)),
                    id: external_function_id,
                };

                let function_ref = self
                    .shared
                    .definition_table
                    .add_external_function_declaration(external)
                    .map_err(|err| {
                        self.create_err(ErrorKind::SemanticError(err), &ast_signature.name)
                    })?;

                self.shared
                    .lookup_table
                    .add_external_function_declaration_link(function_ref.clone())
                    .map_err(|err| {
                        self.create_err(ErrorKind::SemanticError(err), &ast_signature.name)
                    })?;

                Function::External(function_ref)
            }
        };

        Ok(func)
    }

    pub fn debug_definition(&self, _definition: &swamp_ast::Definition) {
        /*
        let (line, col) = self
            .shared
            .source_map
            .get_span_location_utf8(self.shared.file_id, definition.node.span.offset as usize);
        let source_line = self
            .shared
            .source_map
            .get_source_line(self.shared.file_id, line);

         */
    }

    /// # Errors
    ///
    pub fn analyze_definition(&mut self, ast_def: &swamp_ast::Definition) -> Result<(), Error> {
        //self.debug_definition(ast_def);

        let analyzed_attributes = self.analyze_attributes(&ast_def.attributes);

        match &ast_def.kind {
            swamp_ast::DefinitionKind::NamedStructDef(ast_struct) => {
                self.analyze_named_struct_type_definition(ast_struct)?;
            }
            swamp_ast::DefinitionKind::AliasDef(alias_def) => {
                self.analyze_alias_type_definition(alias_def)?;
            }
            swamp_ast::DefinitionKind::EnumDef(identifier, variants) => {
                self.analyze_enum_type_definition(identifier, variants)?;
            }
            swamp_ast::DefinitionKind::FunctionDef(function) => {
                let _resolved_return_type = self.analyze_return_type(function)?;
                self.start_function();
                self.analyze_function_definition(function, &analyzed_attributes)?;
                self.stop_function();
            }
            swamp_ast::DefinitionKind::ImplDef(type_identifier, functions) => {
                self.analyze_impl_definition(type_identifier, functions)?;
            }
            swamp_ast::DefinitionKind::Mod(mod_info) => self.analyze_mod_definition(mod_info)?,
            swamp_ast::DefinitionKind::Use(use_info) => self.analyze_use_definition(use_info)?,
            swamp_ast::DefinitionKind::Constant(const_info) => {
                self.analyze_constant_definition(const_info)?;
            }
        };

        Ok(())
    }

    fn analyze_impl_definition(
        &mut self,
        attached_to_type: &swamp_ast::LocalTypeIdentifierWithOptionalTypeVariables,
        functions: &[swamp_ast::Function],
    ) -> Result<(), Error> {
        let type_name_text = self.get_text(&attached_to_type.name).to_string();

        let converted_type_variables_to_ast_types = attached_to_type
            .type_variables
            .iter()
            .map(|x| {
                swamp_ast::GenericParameter::Type(swamp_ast::Type::Named(
                    swamp_ast::QualifiedTypeIdentifier {
                        name: swamp_ast::LocalTypeIdentifier(x.0.clone()),
                        module_path: None,
                        generic_params: vec![],
                    },
                ))
            })
            .collect();

        let qualified = swamp_ast::QualifiedTypeIdentifier {
            name: swamp_ast::LocalTypeIdentifier(attached_to_type.name.clone()),
            module_path: None,
            generic_params: converted_type_variables_to_ast_types,
        };

        let maybe_type_to_attach_to = Some(self.analyze_named_type(&qualified)?);
        if let Some(type_to_attach_to) = maybe_type_to_attach_to {
            let function_refs: Vec<&swamp_ast::Function> = functions.iter().collect();

            self.analyze_impl_functions(&type_to_attach_to, &function_refs)?;

            Ok(())
        } else {
            Err(self.create_err(
                ErrorKind::CanNotAttachFunctionsToType,
                &attached_to_type.name,
            ))
        }
    }

    /// # Errors
    ///
    pub fn analyze_impl_functions(
        &mut self,
        attach_to_type: &Type, // Needed for self
        functions: &[&swamp_ast::Function],
    ) -> Result<(), Error> {
        if !self
            .shared
            .state
            .associated_impls
            .is_prepared(attach_to_type)
        {
            self.shared.state.associated_impls.prepare(attach_to_type);
        }

        for function in functions {
            self.start_function();

            let function_name = match function {
                swamp_ast::Function::Internal(function_with_body) => {
                    &function_with_body.declaration
                }
                swamp_ast::Function::External(_, external_declaration) => external_declaration,
            };

            let function_name_str = self.get_text(&function_name.name).to_string();
            //            info!(function_name_str, "impl function");

            let resolved_function = self.analyze_impl_func(function, attach_to_type)?;

            let resolved_function_ref = Rc::new(resolved_function);

            self.stop_function();

            let is_built_in = matches!(function_name_str.as_str(), "to_string" | "default");
            if is_built_in {
                self.shared
                    .state
                    .associated_impls
                    .remove_internal_function_if_exists(attach_to_type, &function_name_str);
            }

            self.shared
                .state
                .associated_impls
                .add_member_function(attach_to_type, &function_name_str, resolved_function_ref)
                .map_err(|err| {
                    self.create_err(ErrorKind::SemanticError(err), &function_name.name)
                })?;
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_impl_func(
        &mut self,
        function: &swamp_ast::Function,
        self_type: &Type,
    ) -> Result<Function, Error> {
        let resolved_fn = match function {
            swamp_ast::Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &function_data.declaration.self_parameter {
                    let actual_self_type = if found_self.is_mutable.is_some() {
                        Type::MutableReference(Box::from(self_type.clone()))
                    } else {
                        self_type.clone()
                    };
                    parameters.push(TypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type: actual_self_type,
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(Option::from(&found_self.is_mutable)),
                        }),
                    });
                }

                for param in &function_data.declaration.params {
                    let mut resolved_type = self.analyze_type(&param.param_type)?;
                    if param.variable.is_mutable.is_some() {
                        resolved_type = Type::MutableReference(Box::from(resolved_type));
                    }

                    let resolved_param = TypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type,
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&param.variable.name),
                            is_mutable: self
                                .to_node_option(Option::from(&param.variable.is_mutable)),
                        }),
                    };
                    if !resolved_param.resolved_type.is_allowed_as_parameter_type() {
                        return Err(self.create_err(
                            ErrorKind::ParameterTypeCanNotBeStorage(
                                resolved_param.resolved_type.clone(),
                            ),
                            &function_data.body.node,
                        ));
                    }

                    parameters.push(resolved_param);
                }

                let return_type =
                    if let Some(ast_return_type) = &function_data.declaration.return_type {
                        let resolved_return_type = self.analyze_type(ast_return_type)?;
                        if resolved_return_type.is_allowed_as_return_type() {
                            resolved_return_type
                        } else {
                            return Err(self.create_err(
                                ErrorKind::NotAllowedAsReturnType(resolved_return_type),
                                function.node(),
                            ));
                        }
                    } else {
                        Type::Unit
                    };

                for param in &parameters {
                    self.create_parameter_resolved(
                        &param.node.as_ref().unwrap().name,
                        param.node.as_ref().unwrap().is_mutable.as_ref(),
                        &param.resolved_type.clone(),
                    )?;
                }

                let statements =
                    self.analyze_function_body_expression(&function_data.body, &return_type)?;

                let attributes = self.analyze_attributes(&function_data.attributes);

                let internal = InternalFunctionDefinition {
                    signature: Signature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    body: statements,
                    name: LocalIdentifier(self.to_node(&function_data.declaration.name)),
                    assigned_name: self.get_text(&function_data.declaration.name).to_string(),
                    defined_in_module_path: self.module_path.clone(),
                    associated_with_type: Some(self_type.clone()),
                    //variable_scopes: self.scope.clone(),
                    function_variables: self.function_variables.clone(),
                    parameters: self.function_parameters.clone(),
                    program_unique_id: self.shared.state.allocate_internal_function_id(),
                    attributes,
                };

                let internal_ref = Rc::new(internal);

                Function::Internal(internal_ref)
            }

            swamp_ast::Function::External(int_node, signature) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &signature.self_parameter {
                    let param = TypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type: self_type.clone(),
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(Option::from(&found_self.is_mutable)),
                        }),
                    };

                    parameters.push(param);
                }

                // Handle parameters, including self if present
                for param in &signature.params {
                    let resolved_type = self.analyze_type(&param.param_type)?;

                    parameters.push(TypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type,
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&param.variable.name),
                            is_mutable: self
                                .to_node_option(Option::from(&param.variable.is_mutable)),
                        }),
                    });
                }

                let return_type = self.analyze_maybe_type(Option::from(&signature.return_type))?;

                let int_string = self.get_text(int_node);
                let external_function_id_int = Self::str_to_int(int_string).unwrap() as u32;

                let external_function_id = external_function_id_int as ExternalFunctionId; //self.shared.state.allocate_external_function_id();

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&signature.name).to_string(),
                    name: Some(self.to_node(&signature.name)),
                    signature: Signature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    id: external_function_id,
                };

                let external_ref = Rc::new(external);

                Function::External(external_ref)
            }
        };
        Ok(resolved_fn)
    }

    fn add_default_functions(&mut self, type_to_attach_to: &Type, node: &swamp_ast::Node) {
        let underlying = type_to_attach_to.underlying();
        if self
            .shared
            .state
            .associated_impls
            .get_internal_member_function(&underlying, "to_string")
            .is_none()
        {
            if matches!(
                &underlying,
                Type::Enum(_) | Type::NamedStruct(_) | Type::AnonymousStruct(_)
            ) {
                let new_internal_function =
                    self.generate_to_string_function_for_type(&type_to_attach_to, node);
                self.shared
                    .state
                    .associated_impls
                    .prepare(&type_to_attach_to);
                self.shared
                    .state
                    .associated_impls
                    .add_internal_function(&type_to_attach_to, new_internal_function)
                    .unwrap()
            }
        }
    }
}
