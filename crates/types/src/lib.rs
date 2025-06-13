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
use tracing::info;

#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Type {
    // Primitives
    Int,
    Float,
    String,
    Bool,

    Unit, // Empty or nothing

    // Aggregate type, Containers
    Tuple(Vec<Type>),
    NamedStruct(NamedStructType),
    AnonymousStruct(AnonymousStructType),
    Range(AnonymousStructType),

    Enum(EnumType),

    Function(Signature),

    Optional(Box<Type>),

    MutableReference(Box<Type>),

    // Fixed capacity `[T; N]
    FixedCapacityAndLengthArray(Box<Type>, usize),

    // View `[T]`
    SliceView(Box<Type>),

    // Collections
    VecStorage(Box<Type>, usize),    // `Vec<T;N>`
    DynamicLengthVecView(Box<Type>), // `Vec<T>`

    StackStorage(Box<Type>, usize), // `Stack<T;N>`
    StackView(Box<Type>),           // `Stack<T>`

    QueueStorage(Box<Type>, usize), // `Queue<T;N>`
    QueueView(Box<Type>),           // `Queue<T;N>`

    MapStorage(Box<Type>, Box<Type>, usize), // `Map<K, V; N>`
    DynamicLengthMapView(Box<Type>, Box<Type>), // Map<K, V>`
    SparseView(Box<Type>),
    SparseStorage(Box<Type>, usize),
    GridStorage(Box<Type>, usize, usize),
    GridView(Box<Type>),
}

impl Type {
    #[must_use]
    pub const fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }
    #[must_use]
    pub fn lowest_common_denominator_view(&self) -> Option<Self> {
        match self {
            Self::FixedCapacityAndLengthArray(inner, _size)
            | Self::QueueStorage(inner, _size)
            | Self::StackStorage(inner, _size)
            | Self::VecStorage(inner, _size) => Some(Self::SliceView(inner.clone())),
            Self::SliceView(inner)
            | Self::QueueView(inner)
            | Self::StackView(inner)
            | Self::DynamicLengthVecView(inner) => Some(Self::SliceView(inner.clone())),
            _ => None,
        }
    }

    #[must_use]
    pub fn lowest_common_denominator_storage(&self) -> Option<Self> {
        match self {
            Self::FixedCapacityAndLengthArray(inner, size)
            | Self::VecStorage(inner, size)
            | Self::QueueStorage(inner, size)
            | Self::StackStorage(inner, size) => {
                Some(Self::FixedCapacityAndLengthArray(inner.clone(), *size))
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn underlying(&self) -> &Self {
        match self {
            Self::MutableReference(x) => x,
            _ => self,
        }
    }

    #[must_use]
    pub const fn is_primitive(&self) -> bool {
        matches!(
            self,
            Self::Int | Self::Float | Self::String | Self::Bool | Self::Unit
        )
    }

    #[must_use]
    pub const fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    fn is_core_type_with_name(&self, name: &str) -> bool {
        match self {
            Self::NamedStruct(named_struct) => named_struct.is_core_type_with_name(name),
            _ => false,
        }
    }

    #[must_use]
    pub fn is_grid(&self) -> bool {
        self.is_core_type_with_name("Grid")
    }

    #[must_use]
    pub fn is_vec_ns(named_struct: &NamedStructType) -> bool {
        named_struct.module_path == vec!["core-0.0.0".to_string()]
            && named_struct.assigned_name.starts_with("Vec<")
    }

    #[must_use]
    pub fn iteration_primary_element_type(&self) -> Option<&Self> {
        match self {
            Self::SliceView(element_type) => Some(element_type),
            Self::FixedCapacityAndLengthArray(element_type, _) => Some(element_type),
            Self::DynamicLengthVecView(element_type) => Some(element_type),
            Self::VecStorage(element_type, _) => Some(element_type),
            Self::StackStorage(element_type, _) => Some(element_type),
            Self::QueueStorage(element_type, _) => Some(element_type),
            Self::StackView(element_type) => Some(element_type),
            Self::QueueView(element_type) => Some(element_type),
            Self::DynamicLengthVecView(element_type) => Some(element_type),
            Self::MutableReference(inner) => inner.iteration_primary_element_type(),
            _ => None,
        }
    }
}

impl Type {
    #[must_use]
    pub fn inner_optional_mut_or_immutable(&self) -> Option<&Self> {
        if let Self::Optional(normal) = self {
            Some(normal)
        } else if let Self::MutableReference(mutable_reference) = self {
            mutable_reference.inner_optional_mut_or_immutable()
        } else {
            None
        }
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
pub struct Signature {
    pub parameters: Vec<TypeForParameter>,
    pub return_type: Box<Type>,
}

impl Signature {
    #[must_use]
    pub fn is_self_mutable(&self) -> bool {
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
    #[must_use]
    pub fn is_concrete_or_unit(&self) -> bool {
        if matches!(self, Self::Unit) {
            true
        } else {
            self.is_concrete()
        }
    }

    pub fn is_concrete(&self) -> bool {
        info!("checking this for concrete {self:?}");
        let was_concrete = self.is_concrete_helper();
        info!(?was_concrete, "returned: {was_concrete} {self:?}");
        was_concrete
    }

    #[must_use]
    pub fn is_concrete_helper(&self) -> bool {
        match self {
            Self::Unit | Self::Function(_) => false,

            Self::SliceView(_) => false,
            Self::GridView(a) | Self::DynamicLengthVecView(a) => false,
            Self::QueueView(_) | Self::SparseView(_) | Self::StackView(_) => false,
            Self::DynamicLengthMapView(a, b) => {
                eprintln!("NOT CONCRETE DynLengthMapView {a:?}, {b:?}");
                false
            }

            Self::MapStorage(_, _, _)
            | Self::FixedCapacityAndLengthArray(_, _)
            | Self::VecStorage(_, _)
            | Self::QueueStorage(_, _)
            | Self::SparseStorage(_, _)
            | Self::GridStorage(_, _, _)
            | Self::StackStorage(_, _) => true,

            Self::Float | Self::Int | Self::String | Self::Bool => true,

            Self::Range(_) => true,

            Self::Optional(inner) | Self::MutableReference(inner) => inner.is_concrete(),

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

    #[must_use]
    pub const fn is_direct(&self) -> bool {
        match self {
            Self::Unit => true,
            Self::Float | Self::Int | Self::String | Self::Bool => true,
            _ => false,
        }
    }

    pub fn is_blittable(&self) -> bool {
        match self {
            Self::Unit => true,

            //| Self::Never
            Self::Function(_)
            | Self::SliceView(_)
            | Self::DynamicLengthMapView(_, _)
            | Self::StackView(_)
            | Self::SparseView(_)
            | Self::DynamicLengthVecView(_)
            | Self::GridView(_)
            | Self::QueueView(_) => false, // views should be blittable?

            Self::MapStorage(_, _, _)
            | Self::GridStorage(_, _, _)
            | Self::FixedCapacityAndLengthArray(_, _)
            | Self::VecStorage(_, _)
            | Self::QueueStorage(_, _)
            | Self::StackStorage(_, _)
            | Self::SparseStorage(_, _)
            | Self::Range(_) => true,

            Self::Float | Self::Int | Self::String | Self::Bool => true,

            Self::Optional(inner) | Self::MutableReference(inner) => inner.is_blittable(),

            Self::Tuple(types) => types.iter().all(Self::is_blittable),
            Self::NamedStruct(struct_type) => struct_type
                .anon_struct_type
                .field_name_sorted_fields
                .iter()
                .all(|(_name, field)| field.field_type.is_blittable()),
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
        self.is_concrete() || matches!(self, Self::SliceView(_))
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

    pub fn can_be_stored_in_field(&self) -> bool {
        info!(?self, "checking if stored in field");
        let x = self.helper_can_be_stored_in_field();
        if !x {
            info!("couldn't be stored in field: {self:?}");
        }
        x
    }

    #[must_use]
    pub fn helper_can_be_stored_in_field(&self) -> bool {
        match self {
            Self::Unit
            | Self::Function(_)
            | Self::MutableReference(_)
            | Self::SliceView(_)
            | Self::DynamicLengthMapView(_, _)
            | Self::StackView(_)
            | Self::QueueView(_)
            | Self::GridView(_)
            | Self::SparseView(_)
            | Self::DynamicLengthVecView(_) => {
                eprintln!("found a view {self:?}");
                false
            } // Views can not be stored in fields, since it is "views" / "pointers" to data stored elsewhere

            Self::VecStorage(_, _) => true,
            Self::MapStorage(_, _, _) => true,
            Self::GridStorage(_, _, _)
            | Self::SparseStorage(_, _)
            | Self::QueueStorage(_, _)
            | Self::StackStorage(_, _) => true,
            Self::FixedCapacityAndLengthArray(_, _) => true,
            Self::Range(_) => true,

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
            Self::Range(_) => write!(f, "Range"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "()"),
            Self::Tuple(tuple_type_ref) => write!(f, "( {tuple_type_ref:?} )"),
            Self::NamedStruct(struct_type_ref) => {
                write!(
                    f,
                    "{} {:?}",
                    struct_type_ref.assigned_name, struct_type_ref.anon_struct_type
                )
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
            Self::FixedCapacityAndLengthArray(element_type, size) => {
                write!(f, "[{element_type:?}; {size}]")
            }
            Self::GridStorage(value_type, width, height) => {
                write!(f, "GridStorage<{value_type:?}, ({width}, {height})>")
            }
            Self::GridView(value_type) => {
                write!(f, "Grid<{value_type:?}>")
            }
            Self::VecStorage(value_type, size) => {
                write!(f, "VecStorage<{value_type:?}, {size}>")
            }
            Self::DynamicLengthVecView(value_type) => {
                write!(f, "Vec<{value_type:?}>")
            }
            Self::SparseView(value_type) => {
                write!(f, "Sparse<{value_type:?}>")
            }
            Self::SliceView(value_type) => {
                write!(f, "[{value_type:?}]")
            }
            Self::MapStorage(key_type, value_type, size) => {
                write!(f, "MapStorage<{key_type:?}, {value_type:?}, {size}>")
            }
            Self::SparseStorage(value_type, size) => {
                write!(f, "SparseStorage< {value_type:?}, {size}>")
            }
            Self::StackStorage(key_type, size) => {
                write!(f, "Stack<{key_type:?}; {size}>")
            }
            Self::StackView(key_type) => {
                write!(f, "Stack<{key_type:?}>")
            }
            Self::QueueStorage(key_type, size) => {
                write!(f, "Queue<{key_type:?}; {size}>")
            }
            Self::QueueView(key_type) => {
                write!(f, "Queue<{key_type:?}>")
            }
            Self::DynamicLengthMapView(key_type, value_type) => {
                write!(f, "Map<{key_type:?}, {value_type:?}>")
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
            Self::Range(_) => write!(f, "Range"),
            Self::Unit => write!(f, "()"),
            Self::Tuple(tuple) => write!(f, "({})", comma(tuple)),
            Self::NamedStruct(struct_ref) => write!(f, "{}", struct_ref.assigned_name),
            Self::AnonymousStruct(struct_ref) => write!(f, "{struct_ref:?}"),
            Self::Enum(enum_type) => write!(f, "{}", enum_type.assigned_name),
            Self::Function(signature) => write!(f, "function {signature}"),
            Self::Optional(base_type) => write!(f, "{base_type}?"),
            Self::MutableReference(base_type) => write!(f, "mut & {base_type:?}"),
            Self::FixedCapacityAndLengthArray(value_type, size) => {
                write!(f, "[{value_type:?}; {size}]")
            }
            Self::VecStorage(value_type, size) => {
                write!(f, "VecStorage<{value_type:?}, {size}>")
            }
            Self::GridStorage(value_type, width, height) => {
                write!(f, "GridStorage<{value_type:?}; ({width}, {height})>")
            }
            Self::SparseStorage(value_type, size) => {
                write!(f, "Sparse<{value_type:?}, {size}>")
            }
            Self::StackStorage(value_type, size) => {
                write!(f, "StackStorage<{value_type:?}, {size}>")
            }
            Self::QueueStorage(value_type, size) => {
                write!(f, "QueueStorage<{value_type:?}, {size}>")
            }
            Self::DynamicLengthVecView(value_type) => {
                write!(f, "Vec<{value_type:?}>")
            }
            Self::GridView(value_type) => {
                write!(f, "Grid<{value_type:?}>")
            }
            Self::SparseView(value_type) => {
                write!(f, "Sparse<{value_type:?}>")
            }
            Self::StackView(value_type) => {
                write!(f, "Stack<{value_type:?}>")
            }
            Self::QueueView(value_type) => {
                write!(f, "Queue<{value_type:?}>")
            }
            Self::SliceView(value_type) => {
                write!(f, "[{value_type:?}]")
            }
            Self::MapStorage(key_type, value_type, size) => {
                write!(f, "MapStorage<{key_type}, {value_type}, {size}>")
            }
            Self::DynamicLengthMapView(key_type, value_type) => {
                write!(f, "Map<{key_type}, {value_type}>")
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

    #[must_use]
    pub fn compatible_ignore_mutability_of(&self, other: &Self) -> bool {
        if let Self::MutableReference(other_reference) = other {
            self.compatible_with(other_reference)
        } else {
            self.compatible_with(other)
        }
    }

    // Modified compatible_with to use strict checks for nested elements
    #[must_use]
    pub fn compatible_with(&self, other: &Self) -> bool {
        // Top-level compatibility check - permits capacity differences
        match (self, other) {
            (Self::Function(a), Self::Function(b)) => a.same_type(b),

            (Self::Int, Self::Int)
            | (Self::Float, Self::Float)
            | (Self::String, Self::String)
            | (Self::Bool, Self::Bool)
            | (Self::Unit, Self::Unit) => true,

            (
                Self::FixedCapacityAndLengthArray(a, _size),
                Self::FixedCapacityAndLengthArray(b, _),
            ) => a.strict_compatible_with_capacity(b),

            (Self::VecStorage(element_a, size_a), Self::VecStorage(element_b, size_b)) => {
                element_a.strict_compatible_with_capacity(element_b)
            }

            (Self::SparseStorage(element_a, size_a), Self::SparseStorage(element_b, size_b)) => {
                element_a.strict_compatible_with_capacity(element_b)
            }

            (
                Self::GridStorage(element_a, width_a, height_a),
                Self::GridStorage(element_b, width_b, height_b),
            ) => {
                width_a >= width_b
                    && height_a >= height_b
                    && element_a.strict_compatible_with_capacity(element_b)
            }

            (
                Self::StackStorage(storage_element, a_size),
                Self::StackStorage(vec_element, b_size),
            ) => a_size >= b_size && vec_element.strict_compatible_with_capacity(storage_element),

            (
                Self::QueueStorage(storage_element, a_size),
                Self::QueueStorage(vec_element, b_size),
            ) => a_size >= b_size && vec_element.strict_compatible_with_capacity(storage_element),

            (
                Self::DynamicLengthVecView(element_first),
                Self::DynamicLengthVecView(element_second),
            ) => element_first.strict_compatible_with_capacity(element_second),

            (Self::SparseView(element_first), Self::SparseView(element_second)) => {
                element_first.strict_compatible_with_capacity(element_second)
            }

            (Self::SliceView(element_a), Self::SliceView(element_b)) => {
                element_a.strict_compatible_with_capacity(element_b)
            }

            (
                Self::MapStorage(key_a, value_a, size_a),
                Self::MapStorage(key_b, value_b, size_b),
            ) => {
                size_a >= size_b
                    && key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (
                Self::MapStorage(key_a, value_a, _size_a),
                Self::DynamicLengthMapView(key_b, value_b),
            ) => {
                key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (
                Self::DynamicLengthMapView(key_a, value_a),
                Self::MapStorage(key_b, value_b, _size_b),
            ) => {
                key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (
                Self::DynamicLengthMapView(key_a, value_a),
                Self::DynamicLengthMapView(key_b, value_b),
            ) => {
                key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (Self::Enum(a), Self::Enum(b)) => a == b,

            (Self::NamedStruct(a), Self::NamedStruct(b)) => same_named_struct_ref(a, b),

            (Self::AnonymousStruct(a), Self::AnonymousStruct(b)) => same_anon_struct_ref(a, b),

            (Self::MutableReference(a), Self::MutableReference(b)) => {
                a.strict_compatible_with_capacity(b)
            }

            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter()
                    .zip(b.iter())
                    .all(|(a, b)| a.strict_compatible_with_capacity(b))
            }

            (Self::Optional(inner_type_a), Self::Optional(inner_type_b)) => {
                inner_type_a.strict_compatible_with_capacity(inner_type_b)
            }

            _ => {
                // Match those with clear layouts
                let storage = self.lowest_common_denominator_view();
                let view = other.lowest_common_denominator_view();

                if let Some(storage) = storage {
                    if let Some(view) = view {
                        return storage.compatible_with(&view);
                    }
                }

                false
            }
        }
    }

    #[must_use]
    pub fn strict_compatible_with_capacity(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Function(a), Self::Function(b)) => a.same_type(b),

            (Self::Int, Self::Int)
            | (Self::Float, Self::Float)
            | (Self::String, Self::String)
            | (Self::Bool, Self::Bool)
            | (Self::Unit, Self::Unit) => true,

            (
                Self::FixedCapacityAndLengthArray(a, size_a),
                Self::FixedCapacityAndLengthArray(b, size_b),
            ) => size_a == size_b && a.strict_compatible_with_capacity(b),

            (Self::VecStorage(element_a, size_a), Self::VecStorage(element_b, size_b)) => {
                size_a == size_b && element_a.strict_compatible_with_capacity(element_b)
            }

            (Self::SparseStorage(element_a, size_a), Self::SparseStorage(element_b, size_b)) => {
                size_a == size_b && element_a.strict_compatible_with_capacity(element_b)
            }

            (
                Self::GridStorage(element_a, width_a, height_a),
                Self::GridStorage(element_b, width_b, height_b),
            ) => {
                width_a == width_b
                    && height_a == height_b
                    && element_a.strict_compatible_with_capacity(element_b)
            }

            (
                Self::StackStorage(storage_element, a_size),
                Self::StackStorage(vec_element, b_size),
            ) => a_size == b_size && vec_element.strict_compatible_with_capacity(storage_element),

            (
                Self::QueueStorage(storage_element, a_size),
                Self::QueueStorage(vec_element, b_size),
            ) => a_size == b_size && vec_element.strict_compatible_with_capacity(storage_element),

            (
                Self::DynamicLengthVecView(element_first),
                Self::DynamicLengthVecView(element_second),
            ) => element_first.strict_compatible_with_capacity(element_second),

            (Self::SparseView(element_first), Self::SparseView(element_second)) => {
                element_first.strict_compatible_with_capacity(element_second)
            }

            (Self::SliceView(element_a), Self::SliceView(element_b)) => {
                element_a.strict_compatible_with_capacity(element_b)
            }

            (
                Self::MapStorage(key_a, value_a, size_a),
                Self::MapStorage(key_b, value_b, size_b),
            ) => {
                size_a == size_b
                    && key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (
                Self::MapStorage(key_a, value_a, _size_a),
                Self::DynamicLengthMapView(key_b, value_b),
            ) => {
                key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (
                Self::DynamicLengthMapView(key_a, value_a),
                Self::MapStorage(key_b, value_b, _size_b),
            ) => {
                key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (
                Self::DynamicLengthMapView(key_a, value_a),
                Self::DynamicLengthMapView(key_b, value_b),
            ) => {
                key_a.strict_compatible_with_capacity(key_b)
                    && value_a.strict_compatible_with_capacity(value_b)
            }

            (Self::Enum(a), Self::Enum(b)) => a == b,

            (Self::NamedStruct(a), Self::NamedStruct(b)) => same_named_struct_ref_strict(a, b),

            (Self::AnonymousStruct(a), Self::AnonymousStruct(b)) => {
                same_anon_struct_ref_strict(a, b)
            }

            (Self::MutableReference(a), Self::MutableReference(b)) => {
                a.strict_compatible_with_capacity(b)
            }

            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter()
                    .zip(b.iter())
                    .all(|(a, b)| a.strict_compatible_with_capacity(b))
            }

            (Self::Optional(inner_type_a), Self::Optional(inner_type_b)) => {
                inner_type_a.strict_compatible_with_capacity(inner_type_b)
            }

            _ => {
                let storage = self.lowest_common_denominator_view();
                let view = other.lowest_common_denominator_view();

                if let Some(storage) = storage {
                    if let Some(view) = view {
                        return storage.strict_compatible_with_capacity(&view);
                    }
                }

                false
            }
        }
    }
}

#[must_use]
pub fn same_anon_struct_ref_strict(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    compare_anonymous_struct_types_strict(a, b)
}

#[must_use]
pub fn same_named_struct_ref_strict(a: &NamedStructType, b: &NamedStructType) -> bool {
    if a.assigned_name != b.assigned_name {
        return false;
    }

    compare_anonymous_struct_types_strict(&a.anon_struct_type, &b.anon_struct_type)
}

#[must_use]
pub fn compare_anonymous_struct_types_strict(
    a: &AnonymousStructType,
    b: &AnonymousStructType,
) -> bool {
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

        if !a_type
            .field_type
            .strict_compatible_with_capacity(&b_type.field_type)
        {
            return false;
        }
    }

    true
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

#[must_use]
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
        write!(f, "{}", self.field_type)
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct AnonymousStructType {
    pub field_name_sorted_fields: SeqMap<String, StructTypeField>,
}

impl Debug for AnonymousStructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{{ {} }}", comma_seq(&self.field_name_sorted_fields))
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
    pub fn are_all_variants_without_payload(&self) -> bool {
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

impl EnumVariantCommon {
    #[must_use]
    pub const fn index(&self) -> u8 {
        self.container_index
    }
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

impl EnumVariantType {}

impl EnumVariantType {
    #[must_use]
    pub const fn common(&self) -> &EnumVariantCommon {
        match self {
            Self::Tuple(tuple) => &tuple.common,
            Self::Struct(c) => &c.common,
            Self::Nothing(c) => &c.common,
        }
    }

    #[must_use]
    pub fn types(&self) -> Vec<Type> {
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
}

impl NamedStructType {
    fn is_core_type_with_name(&self, name: &str) -> bool {
        self.module_path == vec!["core-0.0.0".to_string()] && self.assigned_name.starts_with(name)
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
    ) -> Self {
        Self {
            //defined_in_module,
            anon_struct_type,
            name,
            module_path: module_path.to_vec(),
            assigned_name: assigned_name.to_string(),
            instantiated_type_parameters: Vec::default(),
        }
    }

    #[must_use]
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

#[must_use]
pub fn all_types_are_concrete(types: &[Type]) -> bool {
    for ty in types {
        if !ty.is_concrete() {
            return false;
        }
    }
    true
}

#[must_use]
pub fn all_types_are_concrete_or_unit(types: &[Type]) -> bool {
    for ty in types {
        if !ty.is_concrete() && *ty != Type::Unit {
            return false;
        }
    }
    true
}

#[derive(Debug, Clone, Default)]
pub struct Attributes(pub Vec<Attribute>);

impl Attributes {
    #[must_use]
    pub fn get_attributes(&self, name: &str) -> Self {
        let vec: Vec<_> = self
            .0
            .iter()
            .filter(|attr| attr.path.name == name)
            .cloned()
            .collect();

        Self(vec)
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[must_use]
    pub fn has_attribute(&self, name: &str) -> bool {
        self.0.iter().any(|attr| attr.path.name == name)
    }

    #[must_use]
    pub fn get_attribute(&self, name: &str) -> Option<&Attribute> {
        self.0.iter().find(|attr| attr.path.name == name)
    }

    #[must_use]
    pub fn get_fn_arg_by_name(&self, attr_name: &str, arg_fn_name: &str) -> Option<&AttributeArg> {
        self.get_attribute(attr_name).and_then(|attr| {
            attr.args.iter().find(|arg| {
                if let AttributeArg::Function(id, _) = arg {
                    id.name == arg_fn_name
                } else {
                    false
                }
            })
        })
    }

    #[must_use]
    pub fn get_fn_arg_sub_args(
        &self,
        attr_name: &str,
        arg_fn_name: &str,
    ) -> Option<&Vec<AttributeArg>> {
        self.get_fn_arg_by_name(attr_name, arg_fn_name)
            .and_then(|arg| {
                if let AttributeArg::Function(_, sub_args) = arg {
                    Some(sub_args)
                } else {
                    None // This should ideally not happen if get_fn_arg_by_name already filtered for Function
                }
            })
    }

    #[must_use]
    pub fn get_arg(&self, name: &str, arg_index: usize) -> Option<AttributeArg> {
        self.get_args(name)
            .and_then(|a| Option::from(a[arg_index].clone()))
    }

    #[must_use]
    pub fn get_value(&self, name: &str, arg_index: usize) -> Option<AttributeValue> {
        self.get_arg(name, arg_index).and_then(|arg| match arg {
            AttributeArg::Literal(x) => Some(x),
            _ => None,
        })
    }

    #[must_use]
    pub fn get_int(&self, name: &str, arg_index: usize) -> Option<i32> {
        self.get_value(name, arg_index).and_then(|x| match x {
            AttributeValue::Int(v) => Some(v),
            _ => None,
        })
    }
    #[must_use]
    pub fn get_int_from_fn_arg(
        &self,
        attr_name: &str,
        arg_fn_name: &str,
        sub_arg_index: usize,
    ) -> Option<i32> {
        self.get_fn_arg_sub_args(attr_name, arg_fn_name)
            .and_then(|sub_args| {
                sub_args
                    .get(sub_arg_index)
                    .and_then(|sub_arg| match sub_arg {
                        AttributeArg::Literal(attr_value) => match attr_value {
                            AttributeValue::Int(v) => Some(*v),
                            _ => None,
                        },
                        _ => None,
                    })
            })
    }

    #[must_use]
    pub fn get_string_from_fn_arg(
        &self,
        attr_name: &str,
        arg_fn_name: &str,
        sub_arg_index: usize,
    ) -> Option<&String> {
        self.get_fn_arg_sub_args(attr_name, arg_fn_name)
            .and_then(|sub_args| {
                sub_args
                    .get(sub_arg_index)
                    .and_then(|sub_arg| match sub_arg {
                        AttributeArg::Literal(attr_value) => match attr_value {
                            AttributeValue::String(s) => Some(s),
                            _ => None,
                        },
                        _ => None,
                    })
            })
    }

    #[must_use]
    pub fn get_string(&self, name: &str, arg_index: usize) -> Option<String> {
        self.get_value(name, arg_index).and_then(|x| match x {
            AttributeValue::String(v) => Some(v),
            _ => None,
        })
    }
    #[must_use]
    pub fn get_args(&self, name: &str) -> Option<Vec<AttributeArg>> {
        self.get_attribute(name)
            .and_then(|a| Option::from(a.args.clone()))
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
