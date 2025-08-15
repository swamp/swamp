/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::Type;
use crate::supporting_types::{AnonymousStructType, EnumType, NamedStructType, Signature};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type TypeRef = Rc<Type>;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum TypeKind {
    // Primitives
    Byte,
    Short,
    Int,
    Codepoint,
    Float,
    StringView(Rc<Type>, Rc<Type>), // basically an "alias" for Vec<Byte>
    StringStorage(Rc<Type>, Rc<Type>, usize), // basically an "alias" for Vec<Byte; N>
    Bool,
    Unit, // Empty or nothing

    Pointer(Rc<Type>),

    // Aggregate type, Containers
    Tuple(Vec<Rc<Type>>),
    NamedStruct(NamedStructType),
    AnonymousStruct(AnonymousStructType),
    Range(TypeRef),

    Enum(EnumType),

    Function(Signature),

    Optional(Rc<Type>),

    // Fixed capacity `[T; N]
    FixedCapacityAndLengthArray(Rc<Type>, usize),

    // View `[T]`
    SliceView(Rc<Type>),

    // Collections
    VecStorage(Rc<Type>, usize),
    DynamicLengthVecView(Rc<Type>),

    StackStorage(Rc<Type>, usize),
    StackView(Rc<Type>),

    QueueStorage(Rc<Type>, usize),
    QueueView(Rc<Type>),

    MapStorage(Rc<Type>, Rc<Type>, usize),
    DynamicLengthMapView(Rc<Type>, Rc<Type>),

    SparseView(Rc<Type>),
    SparseStorage(Rc<Type>, usize),

    GridStorage(Rc<Type>, usize, usize),
    GridView(Rc<Type>),

    Any,
    Never,
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Never => write!(f, "!"),
            Self::Pointer(inner) => write!(f, "Ptr<inner>"),
            Self::Any => write!(f, "Any"),
            Self::Byte => write!(f, "byte"),
            Self::Short => write!(f, "short"),
            Self::Codepoint => write!(f, "codepoint"),
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::StringView(..) => write!(f, "string"),
            Self::StringStorage(_, size, _) => write!(f, "string[{size}]"),
            Self::Bool => write!(f, "bool"),
            Self::Unit => write!(f, "()"),
            Self::Tuple(types) => write!(f, "({})", seq_fmt::comma(types)),
            Self::NamedStruct(named_struct) => write!(f, "{named_struct}",),
            Self::AnonymousStruct(anon_struct) => write!(f, "{anon_struct}"),
            Self::Range(range) => {
                if let Self::AnonymousStruct(anon_struct) = &*range.kind {
                    write!(f, "range{anon_struct}")
                } else {
                    write!(f, "range<invalid>")
                }
            }
            Self::Enum(enum_type) => write!(f, "{enum_type}"),
            Self::Function(signature) => write!(f, "{signature}"),
            Self::Optional(inner) => write!(f, "option<{inner}>"),
            Self::FixedCapacityAndLengthArray(inner, size) => {
                write!(f, "[{inner}; {size}]")
            }
            Self::SliceView(inner) => write!(f, "[{inner}]"),
            Self::VecStorage(inner, capacity) => {
                write!(f, "Vec<{inner}, {capacity}>")
            }
            Self::DynamicLengthVecView(inner) => write!(f, "Vec<{inner}>"),
            Self::StackStorage(inner, capacity) => {
                write!(f, "Stack<{inner}, {capacity}>")
            }
            Self::StackView(inner) => write!(f, "Stack<{inner}>"),
            Self::QueueStorage(inner, capacity) => {
                write!(f, "Queue<{inner}, {capacity}>")
            }
            Self::QueueView(inner) => write!(f, "Queue<{inner}>"),
            Self::MapStorage(key, value, capacity) => {
                write!(f, "Map<{key}, {value}, {capacity}>")
            }
            Self::DynamicLengthMapView(key, value) => {
                write!(f, "Map<{key}, {value}>")
            }
            Self::SparseView(inner) => write!(f, "Sparse<{inner}>"),
            Self::SparseStorage(inner, capacity) => {
                write!(f, "Sparse<{inner}, {capacity}>")
            }
            Self::GridStorage(inner, width, height) => {
                write!(f, "Grid<{inner}, {width}, {height}>")
            }
            Self::GridView(inner) => write!(f, "Grid<{inner}>"),
        }
    }
}
