/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::prelude::InstantiationCache;
use crate::{
    AssociatedImpls, ExternalFunctionDefinition, Function, FunctionScopeState,
    InternalFunctionDefinition, LocalIdentifier, SemanticError, Variable, VariableRef,
};
use seq_map::SeqMap;
use source_map_node::Node;
use std::rc::Rc;
use swamp_types::{
    AnonymousStructType, GenericAwareSignature, NamedStructType, ParameterizedTypeBlueprint,
    ParameterizedTypeBlueprintInfo, ParameterizedTypeKind, Signature, StructTypeField, Type,
    TypeForParameter, TypeVariable, all_types_are_concrete, all_types_are_concrete_or_unit,
};
use tracing::info;

#[derive(Debug, Clone)]
pub struct TypeVariableScope {
    type_variables_private: SeqMap<String, Type>,
}

impl TypeVariableScope {
    #[must_use]
    pub const fn new(scope: SeqMap<String, Type>) -> Self {
        Self {
            type_variables_private: scope,
        }
    }

    pub fn with_variables(
        &self,
        type_variables: &[TypeVariable],
    ) -> Result<TypeVariableScope, SemanticError> {
        let mut new_scope = self.clone();

        for type_variable in type_variables {
            new_scope.add_type_variable(&type_variable.0)?;
        }

        Ok(new_scope)
    }

    /// # Errors
    ///
    pub fn add_type_variable(&mut self, type_variable: &str) -> Result<(), SemanticError> {
        self.type_variables_private
            .insert(
                type_variable.to_string(),
                Type::Variable(type_variable.to_string()),
            )
            .map_err(|_| SemanticError::DuplicateSymbolName(type_variable.to_string()))
    }

    pub(crate) fn types(&self) -> Vec<Type> {
        self.type_variables_private.values().cloned().collect()
    }

    #[must_use]
    pub fn internal_get_type(&self, name: &str) -> Option<Type> {
        self.type_variables_private.get(&name.to_string()).cloned()
    }
}

#[derive(Clone, Debug)]
pub struct Instantiator {
    pub associated_impls: AssociatedImpls,
    pub instantiation_cache: InstantiationCache,
}

impl Default for Instantiator {
    fn default() -> Self {
        Self::new()
    }
}

impl Instantiator {
    #[must_use]
    pub fn new() -> Self {
        Self {
            associated_impls: AssociatedImpls::new(),
            instantiation_cache: InstantiationCache::new(),
        }
    }
    pub fn instantiate_parameters_and_variables(
        &mut self,
        internal: &InternalFunctionDefinition,
        current_self: &Type,
        type_variables: &TypeVariableScope,
    ) -> Result<Vec<VariableRef>, SemanticError> {
        internal
            .parameter_and_variables
            .iter()
            .map(|var| {
                //                info!(?var, "instantiating variable");
                let instantiated_type = self.instantiate_type_in_signature(
                    current_self,
                    &var.resolved_type,
                    type_variables,
                )?;

                //              info!(?var.assigned_name, ?var.resolved_type, ?instantiated_type, "instantiated variable");

                Ok(VariableRef::new(Variable {
                    name: var.name.clone(),
                    assigned_name: var.assigned_name.clone(),
                    resolved_type: instantiated_type,
                    mutable_node: var.mutable_node.clone(),
                    scope_index: var.scope_index,
                    variable_index: var.variable_index,
                    unique_id_within_function: var.unique_id_within_function,
                    is_unused: var.is_unused,
                }))
            })
            .collect()
    }

    /// # Errors
    ///
    /// # Panics
    ///
    pub fn instantiate_blueprint_and_members(
        &mut self,
        blueprint: &ParameterizedTypeBlueprint,
        analyzed_type_parameters: &[Type],
    ) -> Result<Type, SemanticError> {
        assert!(all_types_are_concrete_or_unit(analyzed_type_parameters));

        if let Some(existing) = self.instantiation_cache.get(
            &blueprint.defined_in_module_path,
            &blueprint.name(),
            analyzed_type_parameters,
        ) {
            return Ok(existing.clone());
        }

        let mut scope = Self::create_type_parameter_scope_from_variables(
            &blueprint.type_variables,
            analyzed_type_parameters,
        );

        let instantiated_type = self.instantiate_base_type_from_blueprint(blueprint, &scope)?;

        let new_impls = {
            let mut new_impls = SeqMap::new();
            let maybe_member_functions = self
                .associated_impls
                .functions
                .get(&Type::Blueprint(blueprint.clone()))
                .cloned();
            if let Some(found_member_functions) = maybe_member_functions {
                for (func_name, func_ref) in &found_member_functions.functions {
                    let maybe_generic_signature = match &**func_ref {
                        Function::Internal(internal) => Some(&internal.signature),
                        _ => None,
                    };

                    /*

                    let new_signature = if let Some(generic_signature) = &maybe_generic_signature {
                        self.instantiate_generic_signature(
                            &instantiated_type,
                            generic_signature,
                            &scope,
                        )?
                    } else {
                        self.instantiate_signature(
                            &instantiated_type,
                            func_ref.signature(),
                            &scope,
                        )?
                    };

                     */

                    scope = if let Some(generic_signature) = &maybe_generic_signature {
                        let mut has_all_generics = true;

                        for required_types in &generic_signature.generic_type_variables {
                            if !scope.type_variables_private.contains_key(&required_types.0) {
                                has_all_generics = false;
                                break;
                            }
                        }

                        if has_all_generics {
                            scope
                        } else {
                            scope.with_variables(&generic_signature.generic_type_variables)?
                        }
                    } else {
                        scope
                    };

                    let new_signature = func_ref.signature();

                    let new_signature =
                        self.instantiate_signature(&instantiated_type, new_signature, &scope)?;

                    let generics_to_keep = if let Some(generic) = maybe_generic_signature {
                        generic.generic_type_variables.clone()
                    } else {
                        vec![]
                    };

                    let new_func = match &**func_ref {
                        Function::Internal(internal) => {
                            let instantiated_variables = self
                                .instantiate_parameters_and_variables(
                                    internal,
                                    &instantiated_type,
                                    &scope,
                                )?;

                            let func_ref = Rc::new(InternalFunctionDefinition {
                                body: internal.body.clone(),
                                name: LocalIdentifier(Node::default()),
                                defined_in_module_path: blueprint.defined_in_module_path.clone(),
                                assigned_name: format!("instantiated {func_name}"),
                                signature: GenericAwareSignature {
                                    signature: new_signature.clone(),
                                    generic_type_variables: generics_to_keep,
                                },
                                parameter_and_variables: instantiated_variables,
                                program_unique_id: internal.program_unique_id,
                            });
                            Function::Internal(func_ref)
                        }
                        Function::External(blueprint_external) => {
                            let func_ref = Rc::new(ExternalFunctionDefinition {
                                name: None,
                                assigned_name: String::new(),
                                signature: new_signature,
                                id: blueprint_external.id,
                            });
                            Function::External(func_ref)
                        }
                        Function::Intrinsic(_) => {
                            panic!("not supported");
                        }
                    };
                    new_impls.insert(func_name.clone(), new_func).unwrap();
                }
            }
            new_impls
        };

        self.associated_impls.prepare(&instantiated_type);
        for (name, func) in &new_impls {
            self.associated_impls.add_member_function(
                &instantiated_type,
                name,
                func.clone().into(),
            )?;
        }

        self.instantiation_cache
            .add(
                &blueprint.defined_in_module_path,
                &blueprint.name(),
                instantiated_type.clone(),
                analyzed_type_parameters,
            )
            .unwrap();

        Ok(instantiated_type)
    }

    fn create_type_parameter_scope_from_variables(
        variables: &[String],
        concrete_types: &[Type],
    ) -> TypeVariableScope {
        assert_eq!(
            variables.len(),
            concrete_types.len(),
            "wrong parameter count"
        );

        assert!(all_types_are_concrete_or_unit(concrete_types));

        let mut scope = SeqMap::new();
        for (param, concrete) in variables.iter().zip(concrete_types) {
            scope.insert(param.clone(), concrete.clone()).unwrap();
        }

        TypeVariableScope::new(scope)
    }

    fn instantiate_base_type_from_blueprint(
        &mut self,
        blueprint: &ParameterizedTypeBlueprint,
        scope: &TypeVariableScope,
    ) -> Result<Type, SemanticError> {
        match &blueprint.kind {
            ParameterizedTypeKind::Struct(struct_ref) => {
                self.instantiate_struct(&Type::Blueprint(blueprint.clone()), struct_ref, scope)
            }
            ParameterizedTypeKind::Enum(_) => todo!(),
        }
    }

    fn instantiate_type_in_signature(
        &mut self,
        current_self: &Type,
        ty: &Type,
        type_variables: &TypeVariableScope,
    ) -> Result<Type, SemanticError> {
        match ty {
            Type::Blueprint(_) => {
                // TODO: Shouldn't this check for matching blueprint?
                Ok(current_self.clone())
            }
            _ => self.instantiate_type_if_needed(Some(current_self), ty, type_variables),
        }
    }

    /// # Errors
    ///
    pub fn instantiate_signature(
        &mut self,
        self_type: &Type,
        signature: &Signature,
        scope: &TypeVariableScope,
    ) -> Result<Signature, SemanticError> {
        let mut instantiated_type_for_parameters = Vec::new();

        for type_for_parameter in &signature.parameters {
            let resolved = self.instantiate_type_in_signature(
                self_type,
                &type_for_parameter.resolved_type,
                scope,
            )?;

            instantiated_type_for_parameters.push(TypeForParameter {
                name: type_for_parameter.name.clone(),
                resolved_type: resolved,
                is_mutable: type_for_parameter.is_mutable,
                node: type_for_parameter.node.clone(),
            });
        }

        let instantiated_return_type =
            self.instantiate_type_in_signature(self_type, &signature.return_type, scope)?;

        let new_signature = Signature {
            parameters: instantiated_type_for_parameters,
            return_type: Box::new(instantiated_return_type),
        };

        Ok(new_signature)
    }

    fn instantiate_types_if_needed(
        &mut self,
        current_self: Option<&Type>,

        types: &[Type],
        type_variables: &TypeVariableScope,
    ) -> Result<Vec<Type>, SemanticError> {
        let mut converted = Vec::new();

        for ty in types {
            let instantiated_type =
                self.instantiate_type_if_needed(current_self, ty, type_variables)?;

            converted.push(instantiated_type);
        }

        Ok(converted)
    }

    fn extract_blueprint_info(ty: &Type) -> Option<ParameterizedTypeBlueprintInfo> {
        match ty {
            Type::NamedStruct(named) => named.blueprint_info.clone(),
            Type::Generic(bp, _) => Some(bp.info()),
            // TODO: This function seems a bit broad, are all three cases needed?
            // | Type::Blueprint(bp) // TODO: Verify that this is never needed
            _ => None,
        }
    }

    fn instantiate_type_if_needed(
        &mut self,
        current_self: Option<&Type>,
        ty: &Type,
        type_variables: &TypeVariableScope,
    ) -> Result<Type, SemanticError> {
        if let Some(cs) = current_self {
            if let Some(cs_bp) = Self::extract_blueprint_info(cs) {
                let other = Self::extract_blueprint_info(ty);
                if let Some(found_other) = other {
                    if found_other == cs_bp {
                        return Ok(cs.clone());
                    }
                }
            }
        }

        let result_type = match ty {
            Type::Generic(parameterized_type, arguments) => {
                let new_arguments =
                    self.instantiate_types_if_needed(current_self, arguments, type_variables)?;
                if all_types_are_concrete(&new_arguments) {
                    self.instantiate_blueprint_and_members(parameterized_type, &new_arguments)?
                } else {
                    panic!("Cannot instantiate generics with unresolved parameters")
                }
            }

            Type::Variable(type_variable) => {
                let found_type =
                    type_variables
                        .internal_get_type(type_variable)
                        .ok_or_else(|| {
                            info!(?type_variable, "could not get");
                            SemanticError::UnknownTypeVariable
                        })?;
                // TODO: Add check for concrete or placeholder                assert!(found_type.is_concrete());
                found_type.clone()
            }

            Type::Blueprint(_blueprint) => {
                panic!("not allowed with blueprints here for types")
            }

            Type::Tuple(types) => {
                let new_types =
                    self.instantiate_types_if_needed(current_self, types, type_variables)?;
                Type::Tuple(new_types)
            }

            Type::Optional(inner_type) => {
                let new_type =
                    self.instantiate_type_if_needed(current_self, inner_type, type_variables)?;
                Type::Optional(Box::new(new_type))
            }

            Type::Slice(inner_type) => {
                let new_type =
                    self.instantiate_type_if_needed(current_self, inner_type, type_variables)?;
                Type::Slice(Box::new(new_type))
            }

            Type::Function(inner_signature) => {
                let new_inner_signature = self.instantiate_signature(
                    current_self.unwrap(),
                    inner_signature,
                    type_variables,
                )?;

                Type::Function(new_inner_signature)
            }

            Type::SlicePair(key_type, value_type) => {
                let new_key_type =
                    self.instantiate_type_if_needed(current_self, key_type, type_variables)?;
                let new_value_type =
                    self.instantiate_type_if_needed(current_self, value_type, type_variables)?;
                Type::SlicePair(Box::new(new_key_type), Box::new(new_value_type))
            }

            _ => ty.clone(),
        };

        Ok(result_type)
    }

    fn parameterized_name(name: &str, parameters: &[Type]) -> String {
        let type_strings: Vec<String> = parameters
            .iter()
            .map(std::string::ToString::to_string)
            .collect();

        format!("{}<{}>", name, type_strings.join(","))
    }

    fn instantiate_struct(
        &mut self,
        current_self: &Type,
        struct_type: &NamedStructType,
        type_variables: &TypeVariableScope,
    ) -> Result<Type, SemanticError> {
        let mut new_fields = SeqMap::new();
        for (name, field) in &struct_type.anon_struct_type.field_name_sorted_fields {
            let new_type = self.instantiate_type_if_needed(
                Some(current_self),
                &field.field_type,
                type_variables,
            )?;
            let new_field = StructTypeField {
                identifier: field.identifier.clone(),
                field_type: new_type,
            };
            new_fields.insert(name.clone(), new_field).unwrap();
        }

        let new_assigned_name =
            Self::parameterized_name(&struct_type.assigned_name, &type_variables.types());

        let Type::Blueprint(blueprint) = current_self else {
            panic!("must be blueprint");
        };

        let new_struct = NamedStructType {
            name: struct_type.name.clone(),
            assigned_name: new_assigned_name,
            anon_struct_type: AnonymousStructType {
                field_name_sorted_fields: new_fields,
            },
            module_path: struct_type.module_path.clone(),
            instantiated_type_parameters: type_variables.types(),
            blueprint_info: Some(blueprint.info()),
        };

        Ok(Type::NamedStruct(new_struct))
    }
}
