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

    DynamicSlice(Box<Type>),
    DynamicSlicePair(Box<Type>, Box<Type>),

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
    ImmutableReference(Box<Type>),

    VecStorage(Box<Type>, usize),
    Vec(Box<Type>),
    MapStorage(Box<Type>, Box<Type>, usize),
    Map(Box<Type>, Box<Type>),
}

impl Type {
    #[must_use] pub const fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }
}

impl Type {
    #[must_use] pub fn lowest_common_denominator(&self) -> Self {
        match self {
            Self::VecStorage(inner, _size) => Self::Vec(Box::from(*inner.clone())),
            _ => self.clone(),
        }
    }
}

impl Type {
    #[must_use] pub fn underlying(&self) -> &Self {
        match self {
            Self::MutableReference(x) => x,
            _ => self,
        }
    }
}

impl Type {}

impl Type {
    #[must_use] pub fn is_vec(&self) -> bool {
        match self {
            Self::NamedStruct(named_struct) => named_struct.is_vec(),
            _ => false,
        }
    }

    #[must_use] pub const fn is_primitive(&self) -> bool {
        matches!(
            self,
            Self::Int | Self::Float | Self::String | Self::Bool | Self::Unit
        )
    }

    #[must_use]
    pub const fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    #[must_use] pub fn is_map(&self) -> bool {
        match self {
            Self::NamedStruct(named_struct) => named_struct.is_map(),
            _ => false,
        }
    }

    #[must_use] pub fn is_stack(&self) -> bool {
        match self {
            Self::NamedStruct(named_struct) => named_struct.is_stack(),
            _ => false,
        }
    }

    #[must_use] pub fn is_range(&self) -> bool {
        match self {
            Self::NamedStruct(named_struct) => named_struct.is_range(),
            _ => false,
        }
    }

    fn is_core_type_with_name(&self, name: &str) -> bool {
        match self {
            Self::NamedStruct(named_struct) => named_struct.is_core_type_with_name(name),
            _ => false,
        }
    }

    #[must_use] pub fn is_grid(&self) -> bool {
        self.is_core_type_with_name("Grid")
    }

    #[must_use] pub fn is_vec_ns(named_struct: &NamedStructType) -> bool {
        named_struct.module_path == vec!["core-0.0.0".to_string()]
            && named_struct.assigned_name.starts_with("Vec<")
    }

    #[must_use] pub fn primary_element_type(&self) -> Option<&Self> {
        match self {
            Self::NamedStruct(ns) => {
                if ns.is_vec() || ns.is_stack() {
                    Some(&ns.instantiated_type_parameters[0])
                } else {
                    None
                }
            }
            Self::MutableReference(inner) => inner.primary_element_type(),
            _ => None,
        }
    }
}

impl Type {
    #[must_use] pub fn inner_optional_mut_or_immutable(&self) -> Option<&Self> {
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
    #[must_use] pub fn name(&self) -> String {
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
    #[must_use] pub fn info(&self) -> ParameterizedTypeBlueprintInfo {
        ParameterizedTypeBlueprintInfo {
            name: self.name(),
            defined_in_module_path: self.defined_in_module_path.clone(),
        }
    }
    #[must_use] pub fn name(&self) -> String {
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

impl Signature {
    #[must_use] pub fn is_self_mutable(&self) -> bool {
        self.parameters
            .first()
            .is_some_and(|x| x.name == "self" && x.is_mutable)
    }
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
    #[must_use] pub fn is_concrete_or_unit(&self) -> bool {
        if matches!(self, Self::Unit) {
            true
        } else {
            self.is_concrete()
        }
    }

    #[must_use]
    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Unit
            | Self::Never
            | Self::Function(_)
            | Self::Generic(_, _)
            | Self::Blueprint(_)
            | Self::Variable(_) => false,

            Self::Vec(_) => false,
            Self::VecStorage(_, _) => true,
            Self::Map(_, _) => false,
            Self::MapStorage(_, _, _) => true,

            Self::Float | Self::Int | Self::String | Self::Bool => true,
            Self::DynamicSlicePair(a, b) => a.is_concrete() && b.is_concrete(),

            Self::Optional(inner)
            | Self::MutableReference(inner)
            | Self::ImmutableReference(inner)
            | Self::DynamicSlice(inner) => inner.is_concrete(),

            Self::Tuple(types) => types.iter().all(Self::is_concrete),
            Self::NamedStruct(struct_type) => struct_type
                .anon_struct_type
                .field_name_sorted_fields
                .iter()
                .all(|(_name, field)| field.field_type.is_concrete()),
            Self::AnonymousStruct(anon_struct_type) => anon_struct_type
                .field_name_sorted_fields
                .iter()
                .all(|(_name, field)| field.field_type.is_concrete()),
            Self::Enum(enum_type) => enum_type
                .variants
                .iter()
                .all(|(_name, variant)| variant.types().iter().all(Self::is_concrete)),
        }
    }

    #[must_use] pub fn is_collection(&self) -> bool {
        self.is_vec() || self.is_grid() || self.is_stack() || self.is_map()
    }

    pub fn is_blittable_collection(&self) -> bool {
        match self {
            Self::NamedStruct(struct_type) => {
                if self.is_collection() {
                    struct_type
                        .instantiated_type_parameters
                        .iter()
                        .all(Self::is_blittable)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    #[must_use] pub const fn is_direct(&self) -> bool {
        match self {
            Self::Unit => true,
            Self::Float | Self::Int | Self::String | Self::Bool => true,
            _ => false,
        }
    }

    pub fn is_blittable(&self) -> bool {
        match self {
            Self::Never => true, // HACK: Just for now
            Self::Unit => true,

            //| Self::Never
            Self::Function(_)
            | Self::Generic(_, _)
            | Self::Blueprint(_)
            | Self::DynamicSlice(_)
            | Self::DynamicSlicePair(_, _)
            | Self::Vec(_)
            | Self::Map(_, _)
            | Self::Variable(_) => false,

            Self::VecStorage(_, _) => true,
            Self::MapStorage(_, _, _) => true,

            Self::Float | Self::Int | Self::String | Self::Bool => true,

            Self::Optional(inner)
            | Self::MutableReference(inner)
            | Self::ImmutableReference(inner) => inner.is_blittable(),

            Self::Tuple(types) => types.iter().all(Self::is_blittable),
            Self::NamedStruct(struct_type) => {
                if self.is_collection() {
                    false
                } else {
                    struct_type
                        .anon_struct_type
                        .field_name_sorted_fields
                        .iter()
                        .all(|(_name, field)| field.field_type.is_blittable())
                }
            }
            Self::AnonymousStruct(anon_struct_type) => anon_struct_type
                .field_name_sorted_fields
                .iter()
                .all(|(_name, field)| field.field_type.is_blittable()),
            Self::Enum(enum_type) => enum_type
                .variants
                .iter()
                .all(|(_name, variant)| variant.types().iter().all(Self::is_blittable)),
        }
    }

    #[must_use]
    pub fn can_be_return_type(&self) -> bool {
        if matches!(self, Self::Unit) {
            true
        } else {
            self.is_concrete()
        }
    }

    #[must_use]
    pub fn can_be_stored_in_variable(&self) -> bool {
        self.is_concrete()
    }

    // TODO: Fix this
    #[must_use]
    pub const fn can_be_parameter(&self) -> bool {
        if let Self::Function(_sign) = self {
            //let parameters_are_ok = sign.parameters.iter().all(|x| x.resolved_type.can_be_stored_in_variable() );
            //return parameters_are_ok; // && sign.return_type.can_be_return_type()
            true
        } else {
            true
        }
    }

    #[must_use]
    pub fn can_be_stored_in_field(&self) -> bool {
        match self {
            Self::Unit
            | Self::Never
            | Self::Function(_)
            | Self::Generic(_, _)
            | Self::Blueprint(_)
            | Self::DynamicSlicePair(_, _)
            | Self::DynamicSlice(_)
            | Self::MutableReference(_)
            | Self::ImmutableReference(_)
            | Self::Vec(_)
            | Self::Map(_, _)
            | Self::Variable(_) => false,

            Self::VecStorage(_, _) => true,
            Self::MapStorage(_, _, _) => true,

            Self::Float | Self::Int | Self::String | Self::Bool => true,

            Self::Optional(inner) => inner.can_be_stored_in_field(),

            Self::Tuple(types) => types.iter().all(Self::can_be_stored_in_field),
            Self::NamedStruct(struct_type) => struct_type
                .anon_struct_type
                .field_name_sorted_fields
                .iter()
                .all(|(_name, field)| field.field_type.can_be_stored_in_field()),
            Self::AnonymousStruct(anon_struct_type) => anon_struct_type
                .field_name_sorted_fields
                .iter()
                .all(|(_name, field)| field.field_type.can_be_stored_in_field()),
            Self::Enum(enum_type) => enum_type
                .variants
                .iter()
                .all(|(_name, variant)| variant.types().iter().all(Self::can_be_stored_in_field)),
        }
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
            Self::MutableReference(base_type) => write!(f, "mut & {base_type:?}"),
            Self::ImmutableReference(base_type) => write!(f, "const & {base_type:?}"),
            Self::Variable(variable_name) => write!(f, "<|{variable_name}|>"),
            Self::Generic(blueprint, non_concrete_arguments) => {
                write!(f, "{blueprint:?}<{non_concrete_arguments:?}>")
            }
            Self::DynamicSlice(value_type) => {
                write!(f, "Slice<{value_type:?}>")
            }
            Self::VecStorage(value_type, size) => {
                write!(f, "VecStorage<{value_type:?}, {size}>")
            }
            Self::Vec(value_type) => {
                write!(f, "Vec<{value_type:?}>")
            }
            Self::MapStorage(key_type, value_type, size) => {
                write!(f, "MapStorage<{key_type:?}, {value_type:?}, {size}>")
            }
            Self::Map(key_type, value_type) => {
                write!(f, "Map<{key_type:?}, {value_type:?}>")
            }
            Self::DynamicSlicePair(key_type, value_type) => {
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
            Self::MutableReference(base_type) => write!(f, "mut & {base_type:?}"),
            Self::ImmutableReference(base_type) => write!(f, "const & {base_type:?}"),
            Self::Variable(variable_name) => write!(f, "<|{variable_name}|>"),

            Self::Generic(blueprint, non_concrete_arguments) => {
                write!(f, "{blueprint:?}<{non_concrete_arguments:?}>")
            }
            Self::VecStorage(value_type, size) => {
                write!(f, "VecStorage<{value_type:?}, {size}>")
            }
            Self::Vec(value_type) => {
                write!(f, "Vec<{value_type:?}>")
            }
            Self::MapStorage(key_type, value_type, size) => {
                write!(f, "MapStorage<{key_type}, {value_type}, {size}>")
            }
            Self::Map(key_type, value_type) => {
                write!(f, "Map<{key_type}, {value_type}>")
            }
            Self::DynamicSlice(value_type) => {
                write!(f, "Slice<{value_type:?}>")
            }
            Self::DynamicSlicePair(key_type, value_type) => {
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

    #[must_use] pub fn compatible_ignore_mutability_of(&self, other: &Self) -> bool {
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

            (Self::Vec(element_a), Self::Vec(element_b)) => element_a.compatible_with(element_b),

            (Self::Vec(vec_element), Self::VecStorage(storage_element, _size)) => {
                vec_element.compatible_with(storage_element)
            }
            // TODO: These are not technically the same, so it should probably be in a can_be_converted from, in a special
            // analyze_assignment_like() helper
            (Self::VecStorage(storage_element, _size), Self::Vec(vec_element)) => {
                vec_element.compatible_with(storage_element)
            }

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

#[must_use] pub fn same_named_struct_ref(a: &NamedStructType, b: &NamedStructType) -> bool {
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

    #[must_use] pub fn are_all_variants_without_payload(&self) -> bool {
        self.variants
            .iter()
            .all(|(_name, variant)| matches!(variant, EnumVariantType::Nothing(_)))
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

    #[must_use] pub fn types(&self) -> Vec<Type> {
        match self {
            Self::Tuple(tuple) => tuple.fields_in_order.clone(),
            Self::Struct(c) => c
                .anon_struct
                .field_name_sorted_fields
                .iter()
                .map(|(_name, field)| field.field_type.clone())
                .collect(),
            Self::Nothing(_c) => vec![],
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

impl NamedStructType {}

impl NamedStructType {}

impl NamedStructType {}

impl NamedStructType {
    fn is_core_type_with_name(&self, name: &str) -> bool {
        self.module_path == vec!["core-0.0.0".to_string()] && self.assigned_name.starts_with(name)
    }

    #[must_use] pub fn is_vec(&self) -> bool {
        self.is_core_type_with_name("Vec")
    }

    #[must_use] pub fn is_stack(&self) -> bool {
        self.is_core_type_with_name("Stack")
    }

    #[must_use] pub fn is_queue(&self) -> bool {
        self.is_core_type_with_name("Queue")
    }

    #[must_use] pub fn is_grid(&self) -> bool {
        self.is_core_type_with_name("Grid")
    }
    #[must_use] pub fn is_range(&self) -> bool {
        self.is_core_type_with_name("Range")
    }

    #[must_use] pub fn is_map(&self) -> bool {
        self.is_core_type_with_name("Map<")
    }
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

    #[must_use] pub fn to_struct_like(&self) -> StructLikeType {
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

#[must_use] pub fn all_types_are_concrete(types: &[Type]) -> bool {
    for ty in types {
        if !ty.is_concrete() {
            return false;
        }
    }
    true
}

#[must_use] pub fn all_types_are_concrete_or_unit(types: &[Type]) -> bool {
    for ty in types {
        if !ty.is_concrete() && *ty != Type::Unit {
            return false;
        }
    }
    true
}

#[must_use] pub fn all_types_are_variables(types: &[Type]) -> bool {
    for ty in types {
        if let Type::Variable(_) = ty {
        } else {
            return false;
        }
    }
    true
}

#[derive(Debug, Clone, Default)]
pub struct Attributes(pub Vec<Attribute>);

impl Attributes {}

impl Attributes {
    #[must_use]
    pub fn get_attributes(&self, name: &str) -> Vec<&Attribute> {
        self.0
            .iter()
            .filter(|attr| attr.path.name == name)
            .collect()
    }

    #[must_use]
    pub fn has_attribute(&self, name: &str) -> bool {
        self.0.iter().any(|attr| attr.path.name == name)
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub path: AttributeIdentifier,
    pub args: Vec<AttributeArg>,
    pub node: Node,
}

#[derive(Debug, Clone)]
pub struct AttributeIdentifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum AttributeArg {
    Path(AttributeIdentifier), // e.g., Debug, unix, clippy::something
    Literal(AttributeValue),   // e.g., "foo", 42, true
    Function(AttributeIdentifier, Vec<AttributeArg>), // e.g., any(unix, windows)
}

#[derive(Debug, Clone)]
pub enum AttributeValue {
    String(String),
    Int(i32),
    Bool(bool),
}
