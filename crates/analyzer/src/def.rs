/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::ErrorKind;
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_attributes::Attributes;
use swamp_modules::symtbl::AliasType;
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
    ) {
        // Clone the module reference to avoid borrowing conflicts
        let found_module = if let Some(module) = self.shared.get_module(path) {
            module.clone()
        } else {
            self.add_err(ErrorKind::UnknownModule, node);
            return;
        };

        match import_items {
            swamp_ast::ImportItems::Nothing => {
                let last_name = path.last().unwrap();
                if self
                    .shared
                    .lookup_table
                    .get_module_link(last_name)
                    .is_none()
                {
                    match self
                        .shared
                        .lookup_table
                        .add_module_link(last_name, found_module.clone())
                    {
                        Ok(_x) => {}
                        Err(err) => {
                            self.add_err(ErrorKind::SemanticError(err), node);
                        }
                    }
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
                                if let Err(sem_err) = self
                                    .shared
                                    .lookup_table
                                    .add_symbol(&ident_text, found_symbol.clone())
                                {
                                    self.add_err(ErrorKind::SemanticError(sem_err), &node.0);
                                    return;
                                }
                            } else {
                                return self.add_err_resolved(
                                    ErrorKind::UnknownTypeReference,
                                    &ident_resolved_node,
                                );
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
                                if let Err(sem_err) = self
                                    .shared
                                    .lookup_table
                                    .add_symbol(&ident_text, found_symbol.clone())
                                {
                                    self.add_err(ErrorKind::SemanticError(sem_err), &node.0);
                                    return;
                                }
                            } else {
                                return self.add_err_resolved(
                                    ErrorKind::UnknownTypeReference,
                                    &ident_resolved_node,
                                );
                            }
                            UseItem::TypeIdentifier(self.to_node(&node.0))
                        }
                    };
                }
            }
            swamp_ast::ImportItems::All => {
                if let Err(sem_err) = self
                    .shared
                    .lookup_table
                    .extend_from(&found_module.symbol_table)
                {
                    self.add_err(ErrorKind::SemanticError(sem_err), node)
                }
            }
        }
    }

    fn analyze_mod_definition(&mut self, mod_definition: &swamp_ast::Mod) {
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
        );
    }

    fn analyze_use_definition(&mut self, use_definition: &swamp_ast::Use) {
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
        );
    }

    fn analyze_enum_type_definition(
        &mut self,
        enum_type_name: &swamp_ast::LocalTypeIdentifierWithOptionalTypeVariables,
        ast_variants: &[swamp_ast::EnumVariantType],
    ) {
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

            let payload_type = match ast_variant_type {
                swamp_ast::EnumVariantType::Simple(_variant_name_node) => self.types().unit(),
                swamp_ast::EnumVariantType::Tuple(_variant_name_node, types) => {
                    let mut vec = Vec::new();
                    for tuple_type in types {
                        let resolved_type = self.analyze_type(tuple_type);
                        vec.push(resolved_type);
                    }

                    self.types().tuple(vec)
                }
                swamp_ast::EnumVariantType::Struct(_variant_name_node, ast_struct_fields) => {
                    let mut fields = SeqMap::new();

                    for field_with_type in &ast_struct_fields.fields {
                        // TODO: Check the index
                        let resolved_type = self.analyze_type(&field_with_type.field_type);
                        let field_name_str =
                            self.get_text(&field_with_type.field_name.0).to_string();

                        let resolved_field = StructTypeField {
                            identifier: Some(self.to_node(&field_with_type.field_name.0)),
                            field_type: resolved_type,
                        };

                        if let Err(seq_map_err) = fields.insert(field_name_str, resolved_field) {
                            return self.add_err(
                                ErrorKind::DuplicateFieldName,
                                &field_with_type.field_name.0,
                            );
                        }
                    }

                    let anonymous_struct_type = AnonymousStructType {
                        field_name_sorted_fields: fields,
                    };

                    self.types().anonymous_struct(anonymous_struct_type)
                }
            };

            let enum_variant_type = swamp_types::prelude::EnumVariantType {
                common,
                payload_type,
            };

            let variant_name_str = self.get_text(variant_name_node).to_string();

            if let Err(_seq_map_err) = resolved_variants.insert(variant_name_str, enum_variant_type)
            {
                return self.add_err(ErrorKind::DuplicateFieldName, variant_name_node);
            }
        }

        new_enum_type.variants = resolved_variants;

        let enum_type_ref = self.shared.state.types.enum_type(new_enum_type);

        if let Err(sem_err) = self
            .shared
            .definition_table
            .add_named_type(enum_type_ref.clone())
        {
            return self.add_err(ErrorKind::SemanticError(sem_err), &enum_type_name.name);
        }

        if let Err(sem_err) = self
            .shared
            .lookup_table
            .add_named_type(enum_type_ref.clone())
        {
            return self.add_err(ErrorKind::SemanticError(sem_err), &enum_type_name.name);
        }

        self.add_default_functions(&enum_type_ref, &enum_type_name.name);
    }

    /// # Errors
    ///
    pub fn analyze_alias_type_definition(&mut self, ast_alias: &swamp_ast::AliasType) -> AliasType {
        let resolved_type = self.analyze_type(&ast_alias.referenced_type);

        let alias_name_str = self.get_text(&ast_alias.identifier.0).to_string();
        let resolved_alias = AliasType {
            name: None,
            ty: resolved_type,
            assigned_name: alias_name_str,
        };

        let resolved_alias_ref = match self.shared.definition_table.add_alias(resolved_alias) {
            Ok(re) => re,

            Err(err) => {
                self.add_err(ErrorKind::SemanticError(err), &ast_alias.identifier.0);
                AliasType {
                    name: None,
                    assigned_name: "err".to_string(),
                    ty: self.types().unit(),
                }
            }
        };

        if let Err(sem_err) = self
            .shared
            .lookup_table
            .add_alias_link(resolved_alias_ref.clone())
        {
            self.add_err(ErrorKind::SemanticError(sem_err), &ast_alias.identifier.0);
            AliasType {
                name: None,
                assigned_name: "err".to_string(),
                ty: self.types().unit(),
            }
        } else {
            resolved_alias_ref
        }
    }

    /// # Errors
    ///
    pub fn analyze_anonymous_struct_type(
        &mut self,
        ast_struct: &swamp_ast::AnonymousStructType,
    ) -> AnonymousStructType {
        let resolved_fields = self.analyze_anonymous_struct_type_fields(&ast_struct.fields);

        AnonymousStructType::new_and_sort_fields(&resolved_fields)
    }

    /// # Errors
    ///
    pub fn analyze_anonymous_struct_type_fields(
        &mut self,
        ast_struct_fields: &[swamp_ast::StructTypeField],
    ) -> SeqMap<String, StructTypeField> {
        let mut resolved_fields = SeqMap::new();

        for field_name_and_type in ast_struct_fields {
            let resolved_type = self.analyze_type(&field_name_and_type.field_type);
            let name_string = self.get_text(&field_name_and_type.field_name.0).to_string();

            let field_type = StructTypeField {
                identifier: Some(self.to_node(&field_name_and_type.field_name.0)),
                field_type: resolved_type,
            };

            if let Err(_seq_map_err) = resolved_fields.insert(name_string, field_type) {
                self.add_err(
                    ErrorKind::DuplicateFieldName,
                    &field_name_and_type.field_name.0,
                );
            }
        }

        resolved_fields
    }

    /// # Errors
    ///
    pub fn analyze_named_struct_type_definition(
        &mut self,
        ast_struct_def: &swamp_ast::NamedStructDef,
    ) {
        let struct_name_str = self.get_text(&ast_struct_def.identifier.name).to_string();

        let fields = self.analyze_anonymous_struct_type_fields(&ast_struct_def.struct_type.fields);

        let analyzed_anonymous_struct = AnonymousStructType::new(fields); // the order encountered in source should be kept

        // Create a TypeRef for the anonymous struct to enable deduplication
        let anon_struct_type_ref = self
            .shared
            .state
            .types
            .anonymous_struct(analyzed_anonymous_struct);

        let named_struct_type = NamedStructType {
            name: self.to_node(&ast_struct_def.identifier.name),
            anon_struct_type: anon_struct_type_ref,
            assigned_name: struct_name_str,
            module_path: self.shared.definition_table.module_path(),
            instantiated_type_parameters: Vec::default(),
        };

        let named_struct_type_ref = self.shared.state.types.named_struct(named_struct_type);

        match self
            .shared
            .definition_table
            .add_named_type(named_struct_type_ref.clone())
        {
            Ok(()) => {}
            Err(sem_err) => {
                return self.add_err(
                    ErrorKind::SemanticError(sem_err),
                    &ast_struct_def.identifier.name,
                );
            }
        }

        match self
            .shared
            .lookup_table
            .add_named_type(named_struct_type_ref.clone())
        {
            Ok(()) => {}
            Err(sem_err) => {
                return self.add_err(
                    ErrorKind::SemanticError(sem_err),
                    &ast_struct_def.identifier.name,
                );
            }
        }

        self.add_default_functions(&named_struct_type_ref, &ast_struct_def.identifier.name);
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn analyze_function_definition(
        &mut self,
        function: &swamp_ast::Function,
        attributes: &Attributes,
    ) -> Option<Function> {
        let func = match &function {
            swamp_ast::Function::Internal(function_data) => {
                let parameters = self.analyze_parameters(&function_data.declaration.params);
                let return_type = if let Some(found) = &function_data.declaration.return_type {
                    self.analyze_type(found)
                } else {
                    self.shared.state.types.unit()
                };

                // Set up scope for function body
                for param in &parameters {
                    let param_type = &param.resolved_type;
                    let actual_type = param_type.clone();
                    self.create_parameter_resolved(
                        &param.node.as_ref().unwrap().name,
                        param.node.as_ref().unwrap().is_mutable.as_ref(),
                        &actual_type,
                    );
                }
                let function_name = self
                    .get_text(&function_data.declaration.name)
                    .trim()
                    .to_string();
                let statements =
                    self.analyze_function_body_expression(&function_data.body, &return_type);

                let internal = InternalFunctionDefinition {
                    signature: Signature {
                        parameters,
                        return_type,
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

                let function_ref = match self
                    .shared
                    .definition_table
                    .add_internal_function(&function_name, internal)
                {
                    Ok(func_def) => func_def,
                    Err(sem_err) => {
                        self.add_err(
                            ErrorKind::SemanticError(sem_err),
                            &function_data.declaration.name,
                        );
                        return None;
                    }
                };

                match self
                    .shared
                    .lookup_table
                    .add_internal_function_link(&function_name, function_ref.clone())
                {
                    Ok(()) => {}
                    Err(sem_err) => {
                        self.add_err(
                            ErrorKind::SemanticError(sem_err),
                            &function_data.declaration.name,
                        );
                        return None;
                    }
                }

                Function::Internal(function_ref)
            }
            swamp_ast::Function::External(int_node, ast_signature) => {
                let parameters = self.analyze_parameters(&ast_signature.params);
                let external_return_type = if let Some(found) = &ast_signature.return_type {
                    self.analyze_type(found)
                } else {
                    self.shared.state.types.unit()
                };

                let return_type = external_return_type;

                let int_string = self.get_text(int_node);
                let external_function_id_int = Self::str_to_int(int_string).unwrap() as u32;

                let external_function_id = external_function_id_int as ExternalFunctionId; //self.shared.state.allocate_external_function_id();

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&ast_signature.name).to_string(),
                    signature: Signature {
                        parameters,
                        return_type,
                    },
                    name: Some(self.to_node(&ast_signature.name)),
                    id: external_function_id,
                };

                let function_ref = match self
                    .shared
                    .definition_table
                    .add_external_function_declaration(external)
                {
                    Ok(func_ref) => func_ref,
                    Err(sem_err) => {
                        self.add_err(ErrorKind::SemanticError(sem_err), &ast_signature.name);
                        return None;
                    }
                };

                if let Err(sem_err) = self
                    .shared
                    .lookup_table
                    .add_external_function_declaration_link(function_ref.clone())
                {
                    self.add_err(ErrorKind::SemanticError(sem_err), &ast_signature.name);
                    return None;
                }

                Function::External(function_ref)
            }
        };

        Some(func)
    }

    pub const fn debug_definition(&self, _definition: &swamp_ast::Definition) {
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
    pub fn analyze_definition(&mut self, ast_def: &swamp_ast::Definition) {
        //self.debug_definition(ast_def);

        let analyzed_attributes = self.analyze_attributes(&ast_def.attributes);

        match &ast_def.kind {
            swamp_ast::DefinitionKind::NamedStructDef(ast_struct) => {
                self.analyze_named_struct_type_definition(ast_struct);
            }
            swamp_ast::DefinitionKind::AliasDef(alias_def) => {
                self.analyze_alias_type_definition(alias_def);
            }
            swamp_ast::DefinitionKind::EnumDef(identifier, variants) => {
                self.analyze_enum_type_definition(identifier, variants);
            }
            swamp_ast::DefinitionKind::FunctionDef(function) => {
                let _resolved_return_type = self.analyze_return_type(function);
                self.start_function();
                self.analyze_function_definition(function, &analyzed_attributes);
                self.stop_function();
            }
            swamp_ast::DefinitionKind::ImplDef(type_identifier, functions) => {
                self.analyze_impl_definition(type_identifier, functions);
            }
            swamp_ast::DefinitionKind::Mod(mod_info) => self.analyze_mod_definition(mod_info),
            swamp_ast::DefinitionKind::Use(use_info) => self.analyze_use_definition(use_info),
            swamp_ast::DefinitionKind::Constant(const_info) => {
                self.analyze_constant_definition(const_info);
            }
        }
    }

    fn analyze_impl_definition(
        &mut self,
        attached_to_type: &swamp_ast::LocalTypeIdentifierWithOptionalTypeVariables,
        functions: &[swamp_ast::Function],
    ) {
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

        let maybe_type_to_attach_to = Some(self.analyze_named_type(&qualified));
        if let Some(type_to_attach_to) = maybe_type_to_attach_to {
            let function_refs: Vec<&swamp_ast::Function> = functions.iter().collect();
            self.analyze_impl_functions(&type_to_attach_to, &function_refs);
        } else {
            self.add_err(
                ErrorKind::CanNotAttachFunctionsToType,
                &attached_to_type.name,
            );
        }
    }

    /// # Errors
    ///
    pub fn analyze_impl_functions(
        &mut self,
        attach_to_type: &TypeRef, // Needed for self
        functions: &[&swamp_ast::Function],
    ) {
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

            let resolved_function = self.analyze_impl_func(function, attach_to_type);

            let resolved_function_ref = Rc::new(resolved_function);

            self.stop_function();

            let is_built_in = matches!(function_name_str.as_str(), "to_string" | "default");
            if is_built_in {
                self.shared
                    .state
                    .associated_impls
                    .remove_internal_function_if_exists(attach_to_type, &function_name_str);
            }

            if let Err(sem_err) = self.shared.state.associated_impls.add_member_function(
                attach_to_type,
                &function_name_str,
                resolved_function_ref,
            ) {
                return self.add_err(ErrorKind::SemanticError(sem_err), &function_name.name);
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_impl_func(
        &mut self,
        function: &swamp_ast::Function,
        self_type: &TypeRef,
    ) -> Function {
        match function {
            swamp_ast::Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &function_data.declaration.self_parameter {
                    let actual_self_type = self_type.clone();
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
                    let resolved_type = self.analyze_type(&param.param_type);

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
                    // TODO: Add back concrete type checking when public method is available

                    parameters.push(resolved_param);
                }

                let return_type =
                    if let Some(ast_return_type) = &function_data.declaration.return_type {
                        self.analyze_type(ast_return_type)
                    } else {
                        self.shared.state.types.unit()
                    };

                for param in &parameters {
                    self.create_parameter_resolved(
                        &param.node.as_ref().unwrap().name,
                        param.node.as_ref().unwrap().is_mutable.as_ref(),
                        &param.resolved_type.clone(),
                    );
                }

                let statements =
                    self.analyze_function_body_expression(&function_data.body, &return_type);

                let attributes = self.analyze_attributes(&function_data.attributes);

                let internal = InternalFunctionDefinition {
                    signature: Signature {
                        parameters,
                        return_type,
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
                    let resolved_type = self.analyze_type(&param.param_type);

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

                let return_type = self.analyze_maybe_type(Option::from(&signature.return_type));

                let int_string = self.get_text(int_node);
                let external_function_id_int = Self::str_to_int(int_string).unwrap() as u32;

                let external_function_id = external_function_id_int as ExternalFunctionId; //self.shared.state.allocate_external_function_id();

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&signature.name).to_string(),
                    name: Some(self.to_node(&signature.name)),
                    signature: Signature {
                        parameters,
                        return_type,
                    },
                    id: external_function_id,
                };

                let external_ref = Rc::new(external);

                Function::External(external_ref)
            }
        }
    }

    fn add_default_functions(&mut self, type_to_attach_to: &TypeRef, node: &swamp_ast::Node) {
        let underlying = type_to_attach_to;
        if self
            .shared
            .state
            .associated_impls
            .get_internal_member_function(underlying, "to_string")
            .is_none()
            && matches!(
                &*underlying.kind,
                TypeKind::Enum(_) | TypeKind::NamedStruct(_) | TypeKind::AnonymousStruct(_)
            )
        {
            let new_internal_function =
                self.generate_to_string_function_for_type(type_to_attach_to, node);
            self.shared
                .state
                .associated_impls
                .prepare(type_to_attach_to);
            self.shared
                .state
                .associated_impls
                .add_internal_function(type_to_attach_to, new_internal_function)
                .unwrap();
        }
    }
}
