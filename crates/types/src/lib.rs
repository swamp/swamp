/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use fmt::{Debug, Display};
use seq_fmt::comma;
use seq_map::SeqMap;
use source_map_node::Node;
use std::cmp::PartialEq;
use std::fmt;
use std::hash::Hash;

#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Type {
    // Primitives
    Int,
    Float,
    String,
    Bool,

    Unit,  // Empty or nothing
    Never, // Not even empty since control flow has escaped with break or return.

    Slice(Box<Type>),
    SlicePair(Box<Type>, Box<Type>),

    // Containers
    Tuple(Vec<Type>),
    NamedStruct(NamedStructType),
    AnonymousStruct(AnonymousStructType),

    Enum(EnumType),

    Function(Signature),

    Optional(Box<Type>),

    Generic(ParameterizedTypeBlueprint, Vec<Type>),
    Blueprint(ParameterizedTypeBlueprint),
    Variable(String),

    MutableReference(Box<Type>),
}

impl Type {
    pub fn inner_optional_mut_or_immutable(&self) -> Option<&Type> {
        if let Self::Optional(normal) = self {
            Some(normal)
        } else if let Self::MutableReference(mutable_reference) = self {
            mutable_reference.inner_optional_mut_or_immutable()
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParameterizedTypeKind {
    Struct(NamedStructType),
    Enum(EnumType),
}

impl ParameterizedTypeKind {
    pub fn name(&self) -> String {
        match self {
            Self::Struct(struct_type_ref) => struct_type_ref.assigned_name.clone(),
            Self::Enum(enum_type_ref) => enum_type_ref.assigned_name.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterizedTypeBlueprintInfo {
    pub name: String,
    pub defined_in_module_path: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ParameterizedTypeBlueprint {
    pub kind: ParameterizedTypeKind,
    pub type_variables: Vec<String>,

    pub defined_in_module_path: Vec<String>,
}

impl ParameterizedTypeBlueprint {
    pub fn info(&self) -> ParameterizedTypeBlueprintInfo {
        ParameterizedTypeBlueprintInfo {
            name: self.name(),
            defined_in_module_path: self.defined_in_module_path.clone(),
        }
    }
    pub fn name(&self) -> String {
        self.kind.name()
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ParameterNode {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

impl Debug for ParameterNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parameter")
    }
}

impl ParameterNode {
    #[inline]
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.is_mutable.is_some()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ExternalType {
    pub type_name: String, // To identify the specific Rust type
    pub number: u32,       // For type comparison
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeForParameter {
    pub name: String,
    pub resolved_type: Type,
    pub is_mutable: bool,
    pub node: Option<ParameterNode>,
}

impl Display for TypeForParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}{}: {:?}",
            if self.is_mutable { "mut " } else { "" },
            self.name,
            self.resolved_type
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeVariable(pub String);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct GenericAwareSignature {
    pub signature: Signature,
    pub generic_type_variables: Vec<TypeVariable>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Signature {
    pub parameters: Vec<TypeForParameter>,
    pub return_type: Box<Type>,
}

impl Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "({})->{}", comma(&self.parameters), self.return_type)
    }
}

impl Signature {
    #[must_use]
    pub fn same_type(&self, other: &Self) -> bool {
        if self.parameters.len() != other.parameters.len()
            || !self.return_type.compatible_with(&other.return_type)
        {
            return false;
        }

        for (param, other_param) in self.parameters.iter().zip(other.parameters.clone()) {
            if !&param
                .resolved_type
                .compatible_with(&other_param.resolved_type)
            {
                return false;
            }

            if param.is_mutable != other_param.is_mutable {
                return false;
            }
        }

        true
    }
}

impl Type {
    #[must_use]
    pub const fn is_concrete(&self) -> bool {
        !matches!(
            self,
            Self::Unit
                | Self::Never
                | Self::Variable(_)
                | Self::Generic(_, _)
                | Self::Blueprint(_)
                | Self::SlicePair(_, _)
                | Self::Slice(_)
        )
    }

    #[must_use]
    pub const fn is_function_type(&self) -> bool {
        matches!(self, Self::Function(_))
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "()"),
            Self::Never => write!(f, "!"),
            Self::Tuple(tuple_type_ref) => write!(f, "( {tuple_type_ref:?} )"),
            Self::NamedStruct(struct_type_ref) => {
                write!(f, "{}", struct_type_ref.assigned_name)
            }
            Self::AnonymousStruct(anonymous_struct_type) => {
                write!(f, "{anonymous_struct_type:?}")
            }
            Self::Enum(enum_type_ref) => write!(f, "{:?}", enum_type_ref.assigned_name),
            Self::Function(function_type_signature) => {
                write!(f, "{function_type_signature:?}")
            }
            Self::Optional(base_type) => write!(f, "{base_type:?}?"),
            Self::MutableReference(base_type) => write!(f, "mut ref {base_type:?}?"),
            Self::Variable(variable_name) => write!(f, "<|{variable_name}|>"),
            Self::Generic(blueprint, non_concrete_arguments) => {
                write!(f, "{blueprint:?}<{non_concrete_arguments:?}>")
            }
            Self::Slice(value_type) => {
                write!(f, "Slice<{value_type:?}>")
            }
            Self::SlicePair(key_type, value_type) => {
                write!(f, "SlicePair<{key_type:?}, {value_type:?}>")
            }
            Self::Blueprint(blueprint) => {
                write!(f, "{blueprint:?}")
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "()"),
            Self::Never => write!(f, "!"),
            Self::Tuple(tuple) => write!(f, "({})", comma(tuple)),
            Self::NamedStruct(struct_ref) => write!(f, "{}", struct_ref.assigned_name),
            Self::AnonymousStruct(struct_ref) => write!(f, "{struct_ref:?}"),
            Self::Enum(enum_type) => write!(f, "{}", enum_type.assigned_name),
            Self::Function(signature) => write!(f, "function {signature}"),
            Self::Optional(base_type) => write!(f, "{base_type}?"),
            Self::MutableReference(base_type) => write!(f, "mut ref {base_type:?}?"),
            Self::Variable(variable_name) => write!(f, "<|{variable_name}|>"),

            Self::Generic(blueprint, non_concrete_arguments) => {
                write!(f, "{blueprint:?}<{non_concrete_arguments:?}>")
            }
            Self::Slice(value_type) => {
                write!(f, "Slice<{value_type:?}>")
            }
            Self::SlicePair(key_type, value_type) => {
                write!(f, "SlicePair<{key_type:?}, {value_type:?}>")
            }
            Self::Blueprint(blueprint) => {
                write!(f, "{blueprint:?}")
            }
        }
    }
}

impl Type {
    #[must_use]
    pub fn assignable_type(&self, other: &Self) -> bool {
        if self.compatible_with(other) {
            true
        } else if let Self::Optional(inner_type) = self {
            inner_type.compatible_with(other)
        } else {
            false
        }
    }

    pub fn compatible_ignore_mutability_of(&self, other: &Self) -> bool {
        if let Self::MutableReference(other_reference) = other {
            self.compatible_with(other_reference)
        } else {
            self.compatible_with(other)
        }
    }

    #[must_use]
    pub fn compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Function(a), Self::Function(b)) => a.same_type(b),

            (_, Self::Never)
            | (Self::Never, _)
            | (Self::Int, Self::Int)
            | (Self::Float, Self::Float)
            | (Self::String, Self::String)
            | (Self::Bool, Self::Bool)
            | (Self::Unit, Self::Unit) => true,

            (Self::Enum(a), Self::Enum(b)) => a == b,

            (Self::NamedStruct(a), Self::NamedStruct(b)) => compare_struct_types(a, b),

            (Self::AnonymousStruct(a), Self::AnonymousStruct(b)) => {
                compare_anonymous_struct_types(a, b)
            }
            (Self::MutableReference(a), Self::MutableReference(b)) => a.compatible_with(b),

            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().zip(b.iter()).all(|(a, b)| a.compatible_with(b))
            }

            (Self::Optional(inner_type_a), Self::Optional(inner_type_b)) => {
                inner_type_a.compatible_with(inner_type_b)
            }

            (Self::Generic(blueprint_a, args_a), Self::Generic(blueprint_b, args_b)) => {
                blueprint_a == blueprint_b && (args_a == args_b)
            }

            (
                Self::SlicePair(a_key_type, a_value_type),
                Self::SlicePair(b_key_type, b_value_type),
            ) => a_key_type == b_key_type && (a_value_type == b_value_type),
            (Self::Slice(inner_type_a), Self::Slice(inner_type_b)) => {
                inner_type_a.compatible_with(inner_type_b)
            }

            (Self::Blueprint(a), Self::Blueprint(b)) => a == b,

            (Self::Variable(a), Self::Variable(b)) => a == b,

            _ => false,
        }
    }
}

fn compare_struct_types(a: &NamedStructType, b: &NamedStructType) -> bool {
    let a_borrow = a;
    let b_borrow = b;

    if a_borrow.assigned_name != b_borrow.assigned_name {
        return false;
    }

    compare_anonymous_struct_types(&a_borrow.anon_struct_type, &b_borrow.anon_struct_type)
}

#[must_use]
pub fn same_anon_struct_ref(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    compare_anonymous_struct_types(a, b)
}

pub fn same_named_struct_ref(a: &NamedStructType, b: &NamedStructType) -> bool {
    if a.assigned_name != b.assigned_name {
        return false;
    }

    compare_anonymous_struct_types(&a.anon_struct_type, &b.anon_struct_type)
}

#[must_use]
pub fn compare_anonymous_struct_types(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    if a.field_name_sorted_fields.len() != b.field_name_sorted_fields.len() {
        return false;
    }

    for ((a_name, a_type), (b_name, b_type)) in a
        .field_name_sorted_fields
        .iter()
        .zip(b.field_name_sorted_fields.clone())
    {
        if *a_name != b_name {
            return false;
        }

        if !a_type.field_type.compatible_with(&b_type.field_type) {
            return false;
        }
    }

    true
}

#[must_use]
pub fn check_assignable_anonymous_struct_types(
    a: &AnonymousStructType,
    b: &AnonymousStructType,
) -> bool {
    if a.field_name_sorted_fields.len() != b.field_name_sorted_fields.len() {
        return false;
    }

    for (name, field) in &a.field_name_sorted_fields {
        if let Some(found_field) = b.field_name_sorted_fields.get(name) {
            if !found_field.field_type.compatible_with(&field.field_type) {
                return false;
            }
        } else {
            return false;
        }
    }

    true
}

#[must_use]
pub fn comma_seq<K: Clone + Hash + Eq + Display, V: Display>(values: &SeqMap<K, V>) -> String {
    let mut result = String::new();
    for (i, (key, value)) in values.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(format!("{key}: {value}").as_str());
    }
    result
}

#[must_use]
pub fn comma_seq_nl<K: Clone + Hash + Eq + Display, V: Display>(
    values: &SeqMap<K, V>,
    prefix: &str,
) -> String {
    let mut result = String::new();
    for (key, value) in values.iter() {
        result.push_str(format!("{prefix}{key}: {value}\n").as_str());
    }
    result
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct StructTypeField {
    pub identifier: Option<Node>,
    pub field_type: Type,
}

impl Display for StructTypeField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}:{}", self.identifier, self.field_type)
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct AnonymousStructType {
    pub field_name_sorted_fields: SeqMap<String, StructTypeField>,
}

impl Debug for AnonymousStructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", comma_seq(&self.field_name_sorted_fields))
    }
}

#[must_use]
pub fn sort_struct_fields2(
    unordered_seq_map: &SeqMap<String, StructTypeField>,
) -> SeqMap<String, StructTypeField> {
    let mut sorted_pairs: Vec<(&String, &StructTypeField)> = unordered_seq_map.iter().collect();
    sorted_pairs.sort_by(|a, b| a.0.cmp(b.0));

    sorted_pairs
        .into_iter()
        .map(|(name, field)| (name.clone(), field.clone()))
        .collect()
}

impl AnonymousStructType {
    #[must_use]
    pub fn new_and_sort_fields(source_ordered_fields: &SeqMap<String, StructTypeField>) -> Self {
        Self {
            field_name_sorted_fields: sort_struct_fields2(source_ordered_fields),
        }
    }

    #[must_use]
    pub const fn new(defined_order: SeqMap<String, StructTypeField>) -> Self {
        Self {
            field_name_sorted_fields: defined_order,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct EnumVariantStructType {
    pub common: EnumVariantCommon,
    pub anon_struct: AnonymousStructType,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct EnumVariantTupleType {
    pub common: EnumVariantCommon,
    pub fields_in_order: Vec<Type>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct EnumType {
    pub name: Node,
    pub assigned_name: String,
    pub module_path: Vec<String>,
    pub variants: SeqMap<String, EnumVariantType>,
    pub instantiated_type_parameters: Vec<Type>,
}

impl Debug for EnumType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.assigned_name)?;
        let s = comma(
            &self
                .variants
                .iter()
                .map(|(name, _variant)| name)
                .collect::<Vec<&String>>(),
        );
        write!(f, "{{ {s} }}")
    }
}

impl EnumType {
    #[must_use]
    pub fn new(name: Node, assigned_name: &str, module_path: Vec<String>) -> Self {
        Self {
            name,
            assigned_name: assigned_name.to_string(),
            module_path,
            variants: SeqMap::new(),
            instantiated_type_parameters: Vec::default(),
        }
    }

    #[must_use]
    pub const fn name(&self) -> &Node {
        &self.name
    }

    #[must_use]
    pub fn get_variant(&self, name: &str) -> Option<&EnumVariantType> {
        self.variants.get(&name.to_string())
    }

    #[must_use]
    pub fn get_variant_from_index(&self, index: usize) -> Option<&EnumVariantType> {
        Some(self.variants.values().collect::<Vec<_>>()[index])
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct EnumVariantCommon {
    pub name: Node,
    pub assigned_name: String,
    pub container_index: u8,
}

impl Debug for EnumVariantCommon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}::{}", self.assigned_name, self.assigned_name)
    }
}

#[derive(Debug)]
pub struct EnumVariantStructFieldType {
    pub name: Node,
    pub enum_variant: EnumVariantType,
    pub resolved_type: Type,

    pub field_index: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub struct EnumVariantTupleFieldType {
    pub name: Node,
    pub enum_variant: EnumVariantType,
    pub resolved_type: Type,

    pub field_index: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct EnumVariantSimpleType {
    pub common: EnumVariantCommon,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum EnumVariantType {
    Struct(EnumVariantStructType),
    Tuple(EnumVariantTupleType),
    Nothing(EnumVariantSimpleType),
}
impl EnumVariantType {
    #[must_use]
    pub const fn common(&self) -> &EnumVariantCommon {
        match self {
            Self::Tuple(tuple) => &tuple.common,
            Self::Struct(c) => &c.common,
            Self::Nothing(c) => &c.common,
        }
    }
}

impl Debug for EnumVariantType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Struct(x) => write!(f, "{{ {x:?} }}"),
            Self::Tuple(x) => write!(f, "({x:?})"),
            Self::Nothing(_x) => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AliasType {
    pub name: Node,
    pub assigned_name: String,
    pub referenced_type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NamedStructType {
    pub name: Node,
    pub module_path: Vec<String>,
    pub assigned_name: String,
    pub anon_struct_type: AnonymousStructType,
    pub instantiated_type_parameters: Vec<Type>,
    pub blueprint_info: Option<ParameterizedTypeBlueprintInfo>,
}

impl Debug for NamedStructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "struct {} anon: {:?}",
            self.assigned_name, self.anon_struct_type
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructLikeType {
    pub assigned_name: String,
    pub anonymous_struct_type: AnonymousStructType,
}

impl NamedStructType {
    #[must_use]
    pub fn new(
        name: Node,
        assigned_name: &str,
        anon_struct_type: AnonymousStructType,
        module_path: &[String],
        blueprint_info: Option<ParameterizedTypeBlueprintInfo>,
    ) -> Self {
        Self {
            //defined_in_module,
            anon_struct_type,
            name,
            module_path: module_path.to_vec(),
            assigned_name: assigned_name.to_string(),
            instantiated_type_parameters: Vec::default(),
            blueprint_info,
        }
    }

    pub fn to_struct_like(&self) -> StructLikeType {
        StructLikeType {
            assigned_name: self.assigned_name.clone(),
            anonymous_struct_type: self.anon_struct_type.clone(),
        }
    }

    #[must_use]
    pub fn field_index(&self, field_name: &str) -> Option<usize> {
        self.anon_struct_type
            .field_name_sorted_fields
            .get_index(&field_name.to_string())
    }

    #[must_use]
    pub const fn name(&self) -> &Node {
        &self.name
    }
}

pub fn all_types_are_concrete(types: &[Type]) -> bool {
    for ty in types {
        if !ty.is_concrete() {
            return false;
        }
    }
    true
}

pub fn all_types_are_concrete_or_unit(types: &[Type]) -> bool {
    for ty in types {
        if !ty.is_concrete() && *ty != Type::Unit {
            return false;
        }
    }
    true
}

pub fn all_types_are_variables(types: &[Type]) -> bool {
    for ty in types {
        if let Type::Variable(_) = ty {
        } else {
            return false;
        }
    }
    true
}
