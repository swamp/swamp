/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::modules::ModuleRef;
use seq_map::SeqMap;
use source_map_node::Node;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_semantic::prelude::*;
use swamp_types::ParameterizedTypeBlueprint;
use swamp_types::prelude::*;
use tiny_ver::TinyVersion;

#[derive(Debug, Clone)]
pub enum FuncDef {
    Internal(InternalFunctionDefinitionRef),
    Intrinsic(IntrinsicFunctionDefinitionRef),
    External(ExternalFunctionDefinitionRef),
}

impl FuncDef {
    #[must_use]
    pub fn signature(&self) -> &Signature {
        match self {
            Self::Internal(internal) => &internal.signature.signature,
            Self::Intrinsic(intrinsic_fn) => &intrinsic_fn.signature,
            Self::External(host_fn) => &host_fn.signature,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TypeParameterName {
    pub resolved_node: Node,
    pub assigned_name: String,
}

#[derive(Debug)]
pub struct TypeParameter {
    pub ty: Type,
    pub debug_name: String,
}

#[derive(Clone, Debug)]
pub enum TypeGeneratorKind {
    Slice,
    SlicePair,
}

#[derive(Clone, Debug)]
pub struct TypeGenerator {
    pub arity: usize,
    pub kind: TypeGeneratorKind,
}

#[derive(Clone, Debug)]
pub enum Symbol {
    Type(Type),
    Module(ModuleRef),
    PackageVersion(TinyVersion),
    Constant(ConstantRef),
    FunctionDefinition(FuncDef),
    Alias(AliasType),
    Blueprint(ParameterizedTypeBlueprint),
    TypeGenerator(TypeGenerator),
}

impl Symbol {
    #[must_use]
    pub const fn is_basic_type(&self) -> bool {
        matches!(self, Self::Type(..) | Self::Alias(..))
    }

    #[must_use] pub const fn is_alias_type(&self) -> bool {
        matches!(self, Self::Alias(..))
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: SeqMap<String, Symbol>,
    module_path: Vec<String>,
}

impl SymbolTable {
    #[must_use] pub fn internal_functions(&self) -> Vec<InternalFunctionDefinitionRef> {
        let mut v = Vec::new();

        for (_name, sym) in &self.symbols {
            if let Symbol::FunctionDefinition(func_def) = sym {
                if let FuncDef::Internal(internal) = func_def {
                    v.push(internal.clone());
                }
            }
        }

        v
    }
}

impl SymbolTable {
    #[must_use] pub fn module_path(&self) -> Vec<String> {
        self.module_path.clone()
    }
}

pub type SymbolTableRef = Rc<SymbolTable>;

impl SymbolTable {
    #[must_use]
    pub fn new(module_path: &[String]) -> Self {
        Self {
            symbols: SeqMap::default(),
            module_path: module_path.to_vec(),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    #[must_use]
    pub const fn symbols(&self) -> &SeqMap<String, Symbol> {
        &self.symbols
    }

    #[must_use] pub fn structs(&self) -> SeqMap<String, NamedStructType> {
        let mut structs = SeqMap::new();

        for (name, symbol) in &self.symbols {
            if let Symbol::Type(ty) = symbol {
                if let Type::NamedStruct(struct_ref) = ty {
                    structs
                        .insert(name.to_string(), struct_ref.clone())
                        .unwrap();
                }
            }
        }

        structs
    }

    /// # Errors
    ///
    pub fn extend_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.symbols() {
            self.add_symbol(name, symbol.clone())?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn extend_basic_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.symbols() {
            if symbol.is_basic_type() {
                self.add_symbol(name, symbol.clone())?;
            }
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn extend_alias_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.symbols() {
            if symbol.is_alias_type() {
                self.add_symbol(name, symbol.clone())?;
            }
        }
        Ok(())
    }

    pub fn extend_intrinsic_functions_from(
        &mut self,
        symbol_table: &Self,
    ) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.symbols() {
            if let Symbol::FunctionDefinition(func_def) = symbol {
                if let FuncDef::Intrinsic(_intrinsic_def) = func_def {
                    self.add_symbol(name, symbol.clone())?;
                }
            }
        }
        Ok(())
    }

    #[must_use]
    pub fn get_package_version(&self, name: &str) -> Option<String> {
        match self.get_symbol(name)? {
            Symbol::PackageVersion(name) => Some(name.to_string()),
            _ => None,
        }
    }

    /// # Errors
    ///
    pub fn add_constant(&mut self, constant: Constant) -> Result<ConstantRef, SemanticError> {
        let constant_ref = Rc::new(constant);

        self.add_constant_link(constant_ref.clone())?;

        Ok(constant_ref)
    }

    /// # Errors
    ///
    pub fn add_constant_link(&mut self, constant_ref: ConstantRef) -> Result<(), SemanticError> {
        let name = constant_ref.assigned_name.clone();

        self.symbols
            .insert(name.to_string(), Symbol::Constant(constant_ref))
            .map_err(|_| SemanticError::DuplicateConstName(name.to_string()))?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn add_alias(&mut self, alias_type: AliasType) -> Result<AliasType, SemanticError> {
        self.add_alias_link(alias_type.clone())?;
        Ok(alias_type)
    }

    /// # Errors
    ///
    pub fn add_alias_link(&mut self, alias_type_ref: AliasType) -> Result<(), SemanticError> {
        let name = alias_type_ref.assigned_name.clone();
        self.symbols
            .insert(name.clone(), Symbol::Alias(alias_type_ref))
            .map_err(|_| SemanticError::DuplicateStructName(name))?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn add_blueprint(
        &mut self,
        blueprint: ParameterizedTypeBlueprint,
    ) -> Result<ParameterizedTypeBlueprint, SemanticError> {
        self.add_blueprint_link(blueprint.clone())?;
        Ok(blueprint)
    }

    /// # Errors
    ///
    pub fn add_blueprint_link(
        &mut self,
        blueprint_ref: ParameterizedTypeBlueprint,
    ) -> Result<(), SemanticError> {
        let name = blueprint_ref.name();
        self.symbols
            .insert(name.clone(), Symbol::Blueprint(blueprint_ref))
            .map_err(|_| SemanticError::DuplicateStructName(name))?;
        Ok(())
    }

    #[must_use]
    pub fn get_blueprint(&self, name: &str) -> Option<&ParameterizedTypeBlueprint> {
        if let Some(found_symbol) = self.get_symbol(name) {
            if let Symbol::Blueprint(type_ref) = found_symbol {
                return Some(type_ref);
            }
        }

        None
    }

    /// # Errors
    ///
    pub fn add_type_generator(
        &mut self,
        name: &str,
        type_generator: TypeGenerator,
    ) -> Result<TypeGenerator, SemanticError> {
        self.add_type_generator_link(name, type_generator.clone())?;
        Ok(type_generator)
    }

    /// # Errors
    ///
    pub fn add_type_generator_link(
        &mut self,
        name: &str,
        type_generator: TypeGenerator,
    ) -> Result<(), SemanticError> {
        self.symbols
            .insert(name.to_string(), Symbol::TypeGenerator(type_generator))
            .map_err(|_| SemanticError::DuplicateStructName(name.to_string()))?;
        Ok(())
    }

    #[must_use]
    pub fn get_type_generator(&self, name: &str) -> Option<&TypeGenerator> {
        if let Some(found_symbol) = self.get_symbol(name) {
            if let Symbol::TypeGenerator(type_gen) = found_symbol {
                return Some(type_gen);
            }
        }

        None
    }

    /// # Errors
    ///
    pub fn add_struct(
        &mut self,
        struct_type: NamedStructType,
    ) -> Result<NamedStructType, SemanticError> {
        self.add_struct_link(struct_type.clone())?;
        Ok(struct_type)
    }

    /// # Errors
    ///
    pub fn add_generated_struct(
        &mut self,
        name: &str,
        fields: &[(&str, Type)],
    ) -> Result<NamedStructType, SemanticError> {
        let mut defined_fields = SeqMap::new();
        for (name, field_type) in fields {
            defined_fields
                .insert(
                    (*name).to_string(),
                    StructTypeField {
                        identifier: None,
                        field_type: field_type.clone(),
                    },
                )
                .unwrap();
        }

        let struct_type = NamedStructType {
            name: Node::default(),
            assigned_name: name.to_string(),
            anon_struct_type: AnonymousStructType::new(defined_fields),
            module_path: self.module_path.clone(),
            instantiated_type_parameters: Vec::default(),
            blueprint_info: None,
        };

        self.add_struct_link(struct_type.clone())?;

        Ok(struct_type)
    }

    /// # Errors
    ///
    pub fn add_struct_link(
        &mut self,
        struct_type_ref: NamedStructType,
    ) -> Result<(), SemanticError> {
        let name = struct_type_ref.assigned_name.clone();
        self.symbols
            .insert(
                name.clone(),
                Symbol::Type(Type::NamedStruct(struct_type_ref)),
            )
            .map_err(|_| SemanticError::DuplicateStructName(name))?;
        Ok(())
    }

    pub fn add_enum_type(&mut self, enum_type: EnumType) -> Result<(), SemanticError> {
        self.add_enum_type_link(enum_type)?;
        Ok(())
    }

    pub fn add_enum_type_link(&mut self, enum_type_ref: EnumType) -> Result<(), SemanticError> {
        let ty = Type::Enum(enum_type_ref.clone());
        self.symbols
            .insert(enum_type_ref.assigned_name.clone(), Symbol::Type(ty))
            .map_err(|_| SemanticError::DuplicateEnumType(enum_type_ref.assigned_name.clone()))?;

        Ok(())
    }

    pub fn add_internal_function(
        &mut self,
        name: &str,
        function: InternalFunctionDefinition,
    ) -> Result<InternalFunctionDefinitionRef, SemanticError> {
        let function_ref = Rc::new(function);
        self.symbols
            .insert(
                name.to_string(),
                Symbol::FunctionDefinition(FuncDef::Internal(function_ref.clone())),
            )
            .expect("todo: add seqmap error handling");
        Ok(function_ref)
    }

    pub fn add_internal_function_link(
        &mut self,
        name: &str,
        function_ref: InternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.symbols
            .insert(
                name.to_string(),
                Symbol::FunctionDefinition(FuncDef::Internal(function_ref)),
            )
            .expect("todo: add seqmap error handling");
        Ok(())
    }

    #[must_use] pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(&name.to_string())
    }

    pub fn add_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        self.symbols
            .insert(name.to_string(), symbol)
            .map_err(|_| SemanticError::DuplicateSymbolName(name.to_string()))
    }

    #[must_use] pub fn get_type(&self, name: &str) -> Option<&Type> {
        if let Some(found_symbol) = self.get_symbol(name) {
            if let Symbol::Type(type_ref) = found_symbol {
                return Some(type_ref);
            }
        }

        None
    }

    #[must_use] pub fn get_struct(&self, name: &str) -> Option<&NamedStructType> {
        match self.get_type(name)? {
            Type::NamedStruct(struct_ref) => Some(struct_ref),
            _ => None,
        }
    }

    #[must_use] pub fn get_enum(&self, name: &str) -> Option<&EnumType> {
        match self.get_type(name)? {
            Type::Enum(enum_type) => Some(enum_type),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_enum_variant_type(
        &self,
        enum_type_name: &str,
        variant_name: &str,
    ) -> Option<EnumVariantType> {
        self.get_enum(enum_type_name).as_ref().map_or_else(
            || None,
            |found_enum| found_enum.variants.get(&variant_name.to_string()).cloned(),
        )
    }

    #[must_use] pub fn get_constant(&self, name: &str) -> Option<&ConstantRef> {
        match self.get_symbol(name)? {
            Symbol::Constant(constant) => Some(constant),
            _ => None,
        }
    }

    // Functions

    #[must_use]
    pub fn get_function(&self, name: &str) -> Option<&FuncDef> {
        match self.get_symbol(name)? {
            Symbol::FunctionDefinition(func_def) => Some(func_def),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_internal_function(&self, name: &str) -> Option<&InternalFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::Internal(internal_fn) => Some(internal_fn),
            FuncDef::External(_) => None,
            FuncDef::Intrinsic(_) => None,
        }
    }

    #[must_use]
    pub fn get_intrinsic_function(&self, name: &str) -> Option<&IntrinsicFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::Intrinsic(intrinsic_fn) => Some(intrinsic_fn),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ExternalFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::External(external_def) => Some(external_def),
            _ => None,
        }
    }

    fn insert_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        self.symbols
            .insert(name.to_string(), symbol)
            .map_err(|_| SemanticError::DuplicateSymbolName(name.to_string()))
    }

    pub fn add_external_function_declaration(
        &mut self,
        decl: ExternalFunctionDefinition,
    ) -> Result<ExternalFunctionDefinitionRef, SemanticError> {
        let decl_ref = Rc::new(decl);

        self.add_external_function_declaration_link(decl_ref.clone())?;

        Ok(decl_ref)
    }

    pub fn add_external_function_declaration_link(
        &mut self,
        decl_ref: ExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.insert_symbol(
            &decl_ref.assigned_name,
            Symbol::FunctionDefinition(FuncDef::External(decl_ref.clone())),
        )
        .map_err(|_| {
            SemanticError::DuplicateExternalFunction(decl_ref.assigned_name.to_string())
        })?;
        Ok(())
    }

    pub fn add_module_link(&mut self, name: &str, ns: ModuleRef) -> Result<(), SemanticError> {
        self.insert_symbol(name, Symbol::Module(ns))
            .map_err(|_| SemanticError::DuplicateNamespaceLink(name.to_string()))?;
        Ok(())
    }

    #[must_use]
    pub fn get_module_link(&self, name: &str) -> Option<&ModuleRef> {
        match self.get_symbol(name)? {
            Symbol::Module(module_ref) => Some(module_ref),
            _ => None,
        }
    }

    pub fn add_package_version(
        &mut self,
        name: &str,
        version: TinyVersion,
    ) -> Result<(), SemanticError> {
        self.insert_symbol(name, Symbol::PackageVersion(version))
            .map_err(|_| SemanticError::DuplicateNamespaceLink(name.to_string()))?;
        Ok(())
    }

    pub fn add_intrinsic_function(
        &mut self,
        function: IntrinsicFunctionDefinition,
    ) -> Result<IntrinsicFunctionDefinitionRef, SemanticError> {
        let function_ref = Rc::new(function);
        self.symbols
            .insert(
                function_ref.name.clone(),
                Symbol::FunctionDefinition(FuncDef::Intrinsic(function_ref.clone())),
            )
            .expect("todo: add seqmap error handling");
        Ok(function_ref)
    }
}
