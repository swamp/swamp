/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::modules::ModuleRef;
use seq_map::SeqMap;
use source_map_node::Node;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_refs::ModuleSymbolReferences;
use swamp_semantic::prelude::*;
use swamp_symbol::TopLevelSymbolId;
use swamp_types::prelude::*;
use swamp_types::TypeRef;

#[derive(Debug, Clone)]
pub enum FuncDef {
    Internal(InternalFunctionDefinitionRef),
    Intrinsic(IntrinsicFunctionDefinitionRef),
    External(ExternalFunctionDefinitionRef),
}

impl FuncDef {
    #[must_use] pub fn symbol_id(&self) -> TopLevelSymbolId {
        match self {
            Self::Internal(internal) => {
                internal.symbol_id
            }
            Self::Intrinsic(_) => { TopLevelSymbolId::new_illegal() }
            Self::External(_) => { TopLevelSymbolId::new_illegal() }
        }
    }
}

impl FuncDef {
    #[must_use]
    pub fn signature(&self) -> &Signature {
        match self {
            Self::Internal(internal) => &internal.signature,
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
    pub ty: TypeRef,
    pub debug_name: String,
}

#[derive(Clone, Debug)]
pub struct AliasType {
    pub symbol_id: TopLevelSymbolId,
    pub name: Node,
    pub assigned_name: String,
    pub ty: TypeRef,
}

#[derive(Clone, Debug)]
pub enum ModuleDefinitionKind {
    Type(TopLevelSymbolId, TypeRef),
    Module(ModuleRef),
    Constant(ConstantRef),
    FunctionDefinition(FuncDef),
    Alias(AliasType),
}


impl ModuleDefinitionKind {
    #[must_use]
    pub const fn is_basic_type(&self) -> bool {
        matches!(
            self,
            Self::Type(..) | Self::Alias(..) | Self::FunctionDefinition(..)
        )
    }

    #[must_use]
    pub const fn is_alias_type(&self) -> bool {
        matches!(self, Self::Alias(..))
    }

    pub(crate) const fn is_function(&self) -> bool {
        matches!(self, Self::FunctionDefinition(..))
    }
}

#[derive(Debug, Clone)]
pub struct ModuleDefinition {
    pub kind: ModuleDefinitionKind,
    pub range: Node,
    pub name: Node,
}

impl ModuleDefinition {
    #[must_use]
    pub const fn is_basic_type(&self) -> bool {
        self.kind.is_basic_type()
    }

    #[must_use]
    pub const fn is_alias_type(&self) -> bool {
        self.kind.is_alias_type()
    }

    pub(crate) const fn is_function(&self) -> bool {
        self.kind.is_function()
    }
}

#[derive(Debug, Clone)]
pub struct DefinitionTable {
    definitions: SeqMap<String, ModuleDefinition>,
    pub refs: ModuleSymbolReferences,
    module_path: Vec<String>,
}

impl DefinitionTable {
    #[must_use]
    pub fn internal_functions(&self) -> Vec<InternalFunctionDefinitionRef> {
        let mut v = Vec::new();

        for (_name, sym) in &self.definitions {
            if let ModuleDefinitionKind::FunctionDefinition(func_def) = &sym.kind
                && let FuncDef::Internal(internal) = func_def
            {
                v.push(internal.clone());
            }
        }

        v
    }
}

impl DefinitionTable {
    #[must_use]
    pub fn module_path(&self) -> Vec<String> {
        self.module_path.clone()
    }
}

pub type SymbolTableRef = Rc<DefinitionTable>;

impl DefinitionTable {
    #[must_use]
    pub fn new(module_path: &[String]) -> Self {
        Self {
            refs: ModuleSymbolReferences::new(),
            definitions: SeqMap::default(),
            module_path: module_path.to_vec(),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.definitions.is_empty()
    }

    #[must_use]
    pub const fn definitions(&self) -> &SeqMap<String, ModuleDefinition> {
        &self.definitions
    }

    #[must_use]
    pub fn structs(&self) -> SeqMap<String, NamedStructType> {
        let mut structs = SeqMap::new();

        for (name, symbol) in &self.definitions {
            if let ModuleDefinitionKind::Type(_, ty) = &symbol.kind
                && let TypeKind::NamedStruct(struct_ref) = &*ty.kind
            {
                structs
                    .insert(name.to_string(), struct_ref.clone())
                    .unwrap();
            }
        }

        structs
    }

    #[must_use]
    pub fn enums(&self) -> SeqMap<String, EnumType> {
        let mut enums = SeqMap::new();

        for (name, symbol) in &self.definitions {
            if let ModuleDefinitionKind::Type(_, ty) = &symbol.kind
                && let TypeKind::Enum(enum_type) = &*ty.kind
            {
                enums.insert(name.to_string(), enum_type.clone()).unwrap();
            }
        }

        enums
    }

    /// # Errors
    ///
    pub fn extend_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.definitions() {
            self.add_symbol(name, symbol.clone())?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn extend_basic_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.definitions() {
            if symbol.is_basic_type() {
                self.add_symbol(name, symbol.clone())?;
            }
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn extend_alias_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.definitions() {
            if symbol.is_alias_type() || symbol.is_function() {
                self.add_symbol(name, symbol.clone())?;
            }
        }
        Ok(())
    }

    pub fn extend_intrinsic_functions_from(
        &mut self,
        symbol_table: &Self,
    ) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.definitions() {
            if let ModuleDefinitionKind::FunctionDefinition(func_def) = &symbol.kind
                && let FuncDef::Intrinsic(_intrinsic_def) = func_def
            {
                self.add_symbol(name, symbol.clone())?;
            }
        }
        Ok(())
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
        self.insert(&constant_ref.assigned_name, &constant_ref.name, &constant_ref.name, ModuleDefinitionKind::Constant(constant_ref.clone()))
    }

    pub fn insert(&mut self, name: &str, name_node: &Node, range: &Node, kind: ModuleDefinitionKind) -> Result<(), SemanticError> {
        let mod_def = ModuleDefinition {
            kind,
            range: range.clone(),
            name: name_node.clone(),
        };

        self.definitions
            .insert(name.to_string(), mod_def)
            .map_err(|_| SemanticError::DuplicateDefinition(name.to_string()))
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
        self.insert(&alias_type_ref.assigned_name, &alias_type_ref.name, &alias_type_ref.name, ModuleDefinitionKind::Alias(alias_type_ref.clone()))
    }

    pub fn add_internal_function(
        &mut self,
        name: &str,
        function: InternalFunctionDefinition,
    ) -> Result<InternalFunctionDefinitionRef, SemanticError> {
        let function_ref = Rc::new(function);

        self.insert(&function_ref.assigned_name, &function_ref.name.0, &function_ref.name.0, ModuleDefinitionKind::FunctionDefinition(FuncDef::Internal(function_ref.clone())))?;

        Ok(function_ref)
    }

    pub fn add_internal_function_link(
        &mut self,
        name: &str,
        function_ref: InternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.insert(&function_ref.assigned_name, &function_ref.name.0, &function_ref.name.0, ModuleDefinitionKind::FunctionDefinition(FuncDef::Internal(function_ref.clone())))
    }

    #[must_use]
    pub fn get_symbol(&self, name: &str) -> Option<&ModuleDefinition> {
        self.definitions.get(&name.to_string())
    }

    pub fn add_symbol(&mut self, name: &str, symbol: ModuleDefinition) -> Result<(), SemanticError> {
        self.definitions
            .insert(name.to_string(), symbol)
            .map_err(|_| SemanticError::DuplicateSymbolName(name.to_string()))
    }

    #[must_use]
    pub fn get_type(&self, name: &str) -> Option<&TypeRef> {
        if let ModuleDefinitionKind::Type(_, type_ref) = &self.get_symbol(name)?.kind {
            Some(type_ref)
        } else {
            None
        }
    }

    #[must_use]
    pub fn get_struct(&self, name: &str) -> Option<&TypeRef> {
        self.get_type(name)
    }

    #[must_use]
    pub fn get_enum(&self, name: &str) -> Option<&TypeRef> {
        self.get_type(name)
    }

    #[must_use]
    pub fn get_enum_variant_type(
        &self,
        enum_type_name: &str,
        variant_name: &str,
    ) -> Option<EnumVariantType> {
        self.get_enum(enum_type_name).as_ref().map_or_else(
            || None,
            |found_type| {
                if let TypeKind::Enum(found_enum) = &*found_type.kind {
                    found_enum.variants.get(&variant_name.to_string()).cloned()
                } else {
                    panic!("internal error: expected enum type")
                }
            },
        )
    }

    #[must_use]
    pub fn get_constant(&self, name: &str) -> Option<&ConstantRef> {
        match &self.get_symbol(name)?.kind {
            ModuleDefinitionKind::Constant(constant) => Some(constant),
            _ => None,
        }
    }

    // Functions

    #[must_use]
    pub fn get_function(&self, name: &str) -> Option<&FuncDef> {
        match &self.get_symbol(name)?.kind {
            ModuleDefinitionKind::FunctionDefinition(func_def) => Some(func_def),
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

    fn insert_definition(&mut self, name: &str, symbol: ModuleDefinition) -> Result<(), SemanticError> {
        self.definitions
            .insert(name.to_string(), symbol)
            .map_err(|_| SemanticError::DuplicateSymbolName(name.to_string()))
    }

    pub fn add_named_type(&mut self, ty: TypeRef) -> Result<(), SemanticError> {
        let name = match &*ty.kind {
            TypeKind::NamedStruct(named) => named.assigned_name.clone(),
            TypeKind::Enum(enum_type) => enum_type.assigned_name.clone(),
            _ => panic!("not a named type"),
        };

        let symbol_id = match &*ty.kind {
            TypeKind::NamedStruct(named) => named.symbol_id,
            TypeKind::Enum(enum_type) => enum_type.symbol_id,
            _ => panic!("not a named type"),
        };

        let name_node = match &*ty.kind {
            TypeKind::NamedStruct(named) => &named.name,
            TypeKind::Enum(enum_type) => &enum_type.name,
            _ => panic!("not a named type"),
        };

        self.insert(&name, name_node, name_node, ModuleDefinitionKind::Type(symbol_id, ty.clone()))
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
        self.insert(
            &decl_ref.assigned_name,
            &decl_ref.name,
            &decl_ref.name,
            ModuleDefinitionKind::FunctionDefinition(FuncDef::External(decl_ref.clone())),
        )
            .map_err(|_| {
                SemanticError::DuplicateExternalFunction(decl_ref.assigned_name.to_string())
            })?;
        Ok(())
    }

    pub fn add_module_link(&mut self, name: &str, name_node: &Node, ns: ModuleRef) -> Result<(), SemanticError> {
        self.insert(name, name_node, name_node, ModuleDefinitionKind::Module(ns))
    }

    #[must_use]
    pub fn get_module_link(&self, name: &str) -> Option<&ModuleRef> {
        match &self.get_symbol(name)?.kind {
            ModuleDefinitionKind::Module(module_ref) => Some(module_ref),
            _ => None,
        }
    }


    pub fn add_intrinsic_function(
        &mut self,
        function: IntrinsicFunctionDefinition,
    ) -> Result<IntrinsicFunctionDefinitionRef, SemanticError> {
        let function_ref = Rc::new(function);
        let fake_node = Node::new_unknown();
        self.insert(
            &function_ref.name,
            &fake_node,
            &fake_node,
            ModuleDefinitionKind::FunctionDefinition(FuncDef::Intrinsic(function_ref.clone())),
        )
            .expect("todo: add seqmap error handling");

        Ok(function_ref)
    }
}
