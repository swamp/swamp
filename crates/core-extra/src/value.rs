/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::extra::{SparseValueId, SparseValueMap};
use crate::grid::Grid;
use crate::map2::Map2;
use crate::map2::print_grid;
use core::any::Any;
use fixed32::Fp;
use seq_map::SeqMap;
use std::cell::Ref;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;
use swamp_semantic::{
    Expression, ExternalFunctionDefinitionRef, FormatSpecifierKind, InternalFunctionDefinitionRef,
    PrecisionType, VariableRef,
};
use swamp_types::StructLikeType;
use swamp_types::prelude::*;

pub type ValueRef = Rc<RefCell<Value>>;

pub trait RustType: Any + Debug + Display + QuickSerialize {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn eq_dyn(&self, other: &dyn RustType) -> bool;
}

// Blanket implementation
impl<T: Any + Debug + Display + QuickSerialize + PartialEq> RustType for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn eq_dyn(&self, other: &dyn RustType) -> bool {
        // Check if `other` is the same concrete type as `self`
        other.as_any().downcast_ref::<T>() == Some(self)
    }
}

pub trait QuickSerialize {
    fn quick_serialize(&self, _octets: &mut [u8]) -> usize {
        0
    }
}

pub trait QuickDeserialize {
    fn quick_deserialize(octets: &[u8]) -> (Self, usize)
    where
        Self: Sized;
}

impl<'a, T: QuickSerialize + ?Sized> QuickSerialize for Ref<'a, T> {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        (**self).quick_serialize(octets)
    }
}

impl<T: QuickSerialize> QuickSerialize for Box<T> {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        // Delegate serialization to the inner T
        (**self).quick_serialize(octets)
    }
}

impl QuickSerialize for Rc<RefCell<dyn RustType>> {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        self.borrow().quick_serialize(octets)
    }
}

#[derive(Debug, Default)]
pub enum Value {
    Int(i32),
    Float(Fp),
    String(String),
    Bool(bool),
    #[default]
    Unit, // Means 'no value' ()

    Slice(Type, Vec<ValueRef>),
    SlicePair(Type, SeqMap<Value, ValueRef>), // Do not change to HashMap, the order is important for it to be deterministic

    Option(Option<ValueRef>),

    // Containers
    Vec(Type, Vec<ValueRef>),
    Map(Type, SeqMap<Value, ValueRef>), // Do not change to HashMap, the order is important for it to be deterministic
    Tuple(Vec<Type>, Vec<ValueRef>),
    Grid(Grid<ValueRef>),
    Map2(Map2<Value, Value, ValueRef>),

    Sparse(Type, SparseValueMap),
    AnonymousStruct(StructLikeType, Vec<ValueRef>), // type of the struct, and the fields themselves in strict order

    EnumVariantSimple(EnumType, EnumVariantSimpleType),
    EnumVariantTuple(EnumType, EnumVariantTupleType, Vec<ValueRef>),
    EnumVariantStruct(EnumType, EnumVariantStructType, Vec<ValueRef>),

    // Higher order
    InternalFunction(InternalFunctionDefinitionRef),
    ExternalFunction(ExternalFunctionDefinitionRef),

    // Other
    RustValue(ExternalType, Rc<RefCell<Box<dyn RustType>>>),
    Lambda(Vec<VariableRef>, Box<Expression>),
}

#[allow(unused)]
fn quick_serialize_values(values: &[Value], buffer: &mut [u8], depth: usize) -> usize {
    let mut offset = 0;

    for value in values {
        let bytes_written = value.quick_serialize(&mut buffer[offset..], depth + 1);
        offset += bytes_written;
    }

    offset
}

impl Value {
    /// Serialize as quickly as possible
    /// Endian format is undefined. It is only used for serializing during a running application
    ///
    #[allow(clippy::too_many_lines)]
    #[inline]
    pub fn quick_serialize(&self, octets: &mut [u8], depth: usize) -> usize {
        match self {
            Self::Int(x) => {
                let value_octets = x.to_le_bytes();
                octets[..value_octets.len()].copy_from_slice(&value_octets);
                value_octets.len()
            }
            Self::Float(fp) => {
                let value_octets = fp.inner().to_le_bytes();
                octets[..value_octets.len()].copy_from_slice(&value_octets);
                value_octets.len()
            }
            Self::String(s) => {
                let len = s.len() as u16;
                let len_bytes = len.to_le_bytes();
                octets[..len_bytes.len()].copy_from_slice(&len_bytes);
                let mut offset = len_bytes.len();

                // Serialize the string bytes
                octets[offset..offset + len as usize].copy_from_slice(s.as_bytes());
                offset += len as usize;
                offset
            }

            Self::Bool(b) => {
                octets[0] = u8::from(*b);
                1
            }
            Self::Lambda(_, _) => todo!(),
            Self::Unit => 0,
            Self::Option(maybe_value) => match maybe_value {
                None => {
                    octets[0] = 0;
                    1
                }
                Some(inner_value) => {
                    octets[0] = 1;
                    let inner_size = inner_value
                        .borrow()
                        .quick_serialize(&mut octets[1..], depth + 1);
                    1 + inner_size
                }
            },
            Self::Vec(_array_ref, values) => {
                let mut offset = 0;

                let count: u16 = values.len() as u16;
                let count_octets = count.to_le_bytes();
                octets[offset..offset + 2].copy_from_slice(&count_octets);
                offset += count_octets.len();

                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);

                    offset += size;
                }
                offset
            }
            Self::Map(_key, values) => {
                let mut offset = 0;

                let count: u16 = values.len() as u16;
                let count_octets = count.to_le_bytes();
                octets[offset..offset + count_octets.len()].copy_from_slice(&count_octets);
                offset += count_octets.len();

                for (key, value_ref) in values {
                    offset += key.quick_serialize(&mut octets[offset..], depth + 1);

                    let value_val = value_ref.borrow();
                    offset += value_val.quick_serialize(&mut octets[offset..], depth + 1);
                }

                offset
            }

            Self::Tuple(_tuple_type_ref, values) => {
                let mut offset = 0;
                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }

            /*
            Self::NamedStruct(_struct_type, values) => {
                let mut offset = 0;
                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }

             */
            Self::AnonymousStruct(_struct_like_type, _values) => {
                todo!("anonymous structs not supported")
            }

            Self::Grid(_grid) => {
                todo!("grid not supported")
            }
            Self::Map2(_map2) => {
                todo!("map2 not supported")
            }
            Self::EnumVariantSimple(_, enum_variant) => {
                octets[0] = enum_variant.common.container_index;
                1
            }
            Self::EnumVariantTuple(_, enum_tuple_ref, values) => {
                let mut offset = 0;
                octets[offset] = enum_tuple_ref.common.container_index;
                offset += 1;
                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }
            Self::EnumVariantStruct(_, enum_struct_ref, values) => {
                let mut offset = 0;
                octets[offset] = enum_struct_ref.common.container_index;
                offset += 1;
                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }

            Self::SlicePair(_key, _values) => {
                panic!("slice pair is not supported ")
            }

            Self::Slice(_element_type, _values) => {
                panic!("slice is not supported ")
            }

            Self::InternalFunction(_) => {
                todo!("internal_functions are not supported yet")
            }
            Self::ExternalFunction(_) => {
                todo!("external_functions are not supported yet")
            }

            Self::RustValue(_rust_value, rust_value) => rust_value.borrow().quick_serialize(octets),
            Self::Sparse(_, _) => todo!(),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Int(i) => Self::Int(*i),
            Self::Float(f) => Self::Float(*f),
            Self::String(s) => Self::String(s.clone()),
            Self::Bool(b) => Self::Bool(*b),
            Self::Unit => Self::Unit,
            Self::Lambda(a, b) => Self::Lambda(a.clone(), b.clone()),

            Self::Option(opt) => {
                let cloned_opt = opt.as_ref().map(std::clone::Clone::clone);
                Self::Option(cloned_opt)
            }

            // Containers
            Self::Vec(resolved_ref, vec_refs) => {
                Self::Vec(resolved_ref.clone(), deep_clone_valrefs(vec_refs))
            }

            Self::Map(key, seq_map) => {
                let cloned_seq_map = seq_map
                    .iter()
                    .map(|(key, val_ref)| (key.clone(), deep_clone_valref(val_ref)))
                    .collect();

                Self::Map(key.clone(), cloned_seq_map)
            }
            Self::Map2(map2) => Self::Map2(map2.clone()),

            Self::Grid(grid) => Self::Grid(grid.clone()),

            Self::Slice(resolved_ref, vec_refs) => {
                Self::Slice(resolved_ref.clone(), deep_clone_valrefs(vec_refs))
            }

            Self::SlicePair(key, seq_map) => {
                let cloned_seq_map = seq_map
                    .iter()
                    .map(|(key, val_ref)| (key.clone(), deep_clone_valref(val_ref)))
                    .collect();

                Self::SlicePair(key.clone(), cloned_seq_map)
            }

            Self::Tuple(resolved_ref, vec_refs) => {
                Self::Tuple(resolved_ref.clone(), deep_clone_valrefs(vec_refs))
            }

            Self::AnonymousStruct(struct_like_type, vec_refs) => {
                Self::AnonymousStruct(struct_like_type.clone(), deep_clone_valrefs(vec_refs))
            }

            Self::EnumVariantSimple(enum_type, resolved_ref) => {
                Self::EnumVariantSimple(enum_type.clone(), resolved_ref.clone())
            }

            Self::EnumVariantTuple(enum_type, resolved_ref, vec_values) => {
                Self::EnumVariantTuple(enum_type.clone(), resolved_ref.clone(), vec_values.clone())
            }

            Self::EnumVariantStruct(enum_type, resolved_ref, vec_values) => {
                Self::EnumVariantStruct(enum_type.clone(), resolved_ref.clone(), vec_values.clone())
            }

            Self::InternalFunction(resolved_def_ref) => {
                Self::InternalFunction(resolved_def_ref.clone())
            }

            Self::ExternalFunction(external_fn) => Self::ExternalFunction(external_fn.clone()),

            Self::RustValue(resolved_rust_ref, rust_type_rc) => {
                Self::RustValue(resolved_rust_ref.clone(), rust_type_rc.clone())
            }
            Self::Sparse(_, _) => todo!(),
        }
    }
}

#[inline]
fn deep_clone_valrefs(vec_values: &[ValueRef]) -> Vec<ValueRef> {
    vec_values.iter().map(deep_clone_valref).collect()
}

#[inline]
fn deep_clone_valref(val_ref: &ValueRef) -> ValueRef {
    let cloned_value = val_ref.borrow().clone();
    Rc::new(RefCell::new(cloned_value))
}

pub fn to_rust_value<T: RustType + 'static>(type_ref: ExternalType, value: T) -> Value {
    Value::RustValue(
        type_ref,
        Rc::new(RefCell::new(Box::new(value) as Box<dyn RustType>)),
    )
}

#[derive(Debug, PartialEq, Eq)]
pub enum ValueError {
    NotAnIterator,
    NotSparseMap,
    CanNotCoerceToIterator,
    ConversionError(String),
    WrongNumberOfArguments { expected: usize, got: usize },
    TypeError(String),
}

//pub const SPARSE_TYPE_ID: TypeNumber = 999;
//pub const SPARSE_ID_TYPE_ID: TypeNumber = 998;

// Iterators

impl Value {
    /// # Errors
    ///
    /// # Panics
    ///
    #[allow(clippy::should_implement_trait)] // TODO: Fix this
    pub fn into_iter(self) -> Result<Box<dyn Iterator<Item = Self>>, ValueError> {
        match self {
            // TODO: Self::Reference(value_ref) => value_ref.borrow().clone().into_iter(is_mutable),
            Self::Vec(_, values) => Ok(Box::new(
                values.into_iter().map(|item| item.borrow().clone()),
            )),

            Self::String(values) => Ok(Box::new(
                values
                    .chars()
                    .map(|item| Self::String(item.to_string()))
                    .collect::<Vec<Self>>()
                    .into_iter(),
            )),
            Self::Map(_key, seq_map) => Ok(Box::new(
                seq_map.into_values().map(|item| item.borrow().clone()),
            )),
            Self::Sparse(_x, sparse_map) => {
                let values: Vec<_> = sparse_map
                    .values()
                    .iter()
                    .map(|item| item.borrow().clone())
                    .collect();
                Ok(Box::new(values.into_iter()))
            }
            Self::RustValue(ref _rust_type_ref, _) => {
                todo!()
            }
            Self::AnonymousStruct(struct_like_type, fields) => {
                // assume it is Range
                debug_assert_eq!(struct_like_type.assigned_name, "Range");
                let start = fields[0].borrow().expect_int()?;
                let end = fields[1].borrow().expect_int()?;
                let is_inclusive = fields[2].borrow().expect_bool()?;
                generate_range_value_iterator(start, end, is_inclusive)
            }
            _ => Err(ValueError::CanNotCoerceToIterator),
        }
    }

    /// # Errors
    ///
    /// # Panics
    ///
    pub fn into_iter_pairs(self) -> Result<Box<dyn Iterator<Item = (Self, Self)>>, ValueError> {
        let values = match self {
            Self::Map(_, seq_map) => {
                Box::new(seq_map.into_iter().map(|(k, v)| (k, v.borrow().clone())))
            }
            Self::Tuple(_type_ref, elements) => {
                let iter = elements
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Self::Int(i as i32), v.borrow().clone()));
                Box::new(iter) as Box<dyn Iterator<Item = (Self, Self)>>
            }
            Self::Vec(_type_ref, array) => {
                let iter = array
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Self::Int(i as i32), v.borrow().clone()));
                Box::new(iter) as Box<dyn Iterator<Item = (Self, Self)>>
            }
            Self::String(string) => {
                let iter = string
                    .chars()
                    .enumerate()
                    .map(|(i, v)| (Self::Int(i as i32), Self::String(v.to_string())))
                    .collect::<Vec<(Self, Self)>>()
                    .into_iter();
                Box::new(iter) as Box<dyn Iterator<Item = (Self, Self)> + 'static>
            }
            Self::Sparse(ref _rust_type_ref, ref sparse_map) => {
                let id_type_ref = sparse_map.rust_type_ref_for_id.clone();

                let pairs: Vec<_> = sparse_map
                    .iter()
                    .map(|(k, v)| {
                        (
                            Self::RustValue(
                                id_type_ref.clone(),
                                Rc::new(RefCell::new(Box::new(SparseValueId(k)))),
                            ),
                            v.borrow().clone(),
                        )
                    })
                    .collect();

                Box::new(pairs.into_iter()) as Box<dyn Iterator<Item = (Self, Self)>>
            }

            _ => return Err(ValueError::NotAnIterator),
        };

        Ok(values)
    }

    #[must_use]
    pub fn convert_to_string_if_needed(&self) -> String {
        match self {
            Self::String(string) => string.clone(),
            _ => self.to_string(),
        }
    }

    /// # Errors
    ///
    pub fn expect_string(&self) -> Result<String, ValueError> {
        match self {
            Self::String(s) => Ok(s.clone()),
            _ => Err(ValueError::ConversionError("Expected string value".into())),
        }
    }

    /*
    pub fn expect_struct(&self) -> Result<(NamedStructType, &Vec<ValueRef>), ValueError> {
        match self {
            Self::NamedStruct(struct_ref, fields) => Ok((struct_ref.clone(), fields)),
            _ => Err(ValueError::ConversionError("Expected struct value".into())),
        }
    }

    pub fn expect_anon_struct(&self) -> Result<(AnonymousStructType, &Vec<ValueRef>), ValueError> {
        match self {
            Self::NamedStruct(struct_ref, fields) => {
                Ok((struct_ref.anon_struct_type.clone(), fields))
            }
            Self::AnonymousStruct(anon_struct_type, fields) => {
                Ok((anon_struct_type.clone(), fields))
            }
            _ => Err(ValueError::ConversionError("Expected struct value".into())),
        }
    }


     */

    pub fn expect_anon_struct(&self) -> Result<(StructLikeType, &Vec<ValueRef>), ValueError> {
        match self {
            Self::AnonymousStruct(struct_like_type, fields) => {
                Ok((struct_like_type.clone(), fields))
            }
            _ => Err(ValueError::ConversionError("Expected struct value".into())),
        }
    }
    pub fn expect_array(&self) -> Result<(Type, &Vec<ValueRef>), ValueError> {
        match self {
            Self::Vec(resolved_array_ref, fields) => Ok((resolved_array_ref.clone(), fields)),
            _ => Err(ValueError::ConversionError("Expected array value".into())),
        }
    }

    pub fn expect_map(&self) -> Result<(Type, &SeqMap<Value, ValueRef>), ValueError> {
        match self {
            Self::Map(key, seq_map) => Ok((key.clone(), seq_map)),
            _ => Err(ValueError::ConversionError("Expected map value".into())),
        }
    }

    pub fn expect_map_mut(&mut self) -> Result<(Type, &mut SeqMap<Value, ValueRef>), ValueError> {
        match self {
            Self::Map(key, seq_map) => Ok((key.clone(), seq_map)),
            _ => Err(ValueError::ConversionError("Expected map value".into())),
        }
    }

    /// # Errors
    ///
    pub fn expect_option_value(&self) -> Result<&Option<ValueRef>, ValueError> {
        match self {
            Self::Option(v) => Ok(v),
            _ => Err(ValueError::ConversionError("Expected option value".into())),
        }
    }

    /// # Errors
    ///
    pub fn expect_int(&self) -> Result<i32, ValueError> {
        match self {
            Self::Int(v) => Ok(*v),
            _ => Err(ValueError::ConversionError("Expected int value".into())),
        }
    }

    /// # Errors
    ///
    pub fn expect_enum_simple_order(v: &Self) -> Result<u8, ValueError> {
        match v {
            Self::EnumVariantSimple(_v, s) => Ok(s.common.container_index),
            _ => Err(ValueError::ConversionError(
                "Expected enum simple value".into(),
            )),
        }
    }

    /// # Errors
    ///
    pub fn expect_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Bool(v) => Ok(*v),
            _ => Err(ValueError::ConversionError("Expected bool value".into())),
        }
    }

    /// # Errors
    ///
    pub fn expect_slice(&self) -> Result<(Type, Vec<ValueRef>), ValueError> {
        match self {
            Self::Slice(ty, items) => Ok((ty.clone(), items.clone())),
            _ => Err(ValueError::ConversionError("Expected slice value".into())),
        }
    }

    /// # Errors
    ///
    pub fn expect_slice_pair(&self) -> Result<(Type, &SeqMap<Value, ValueRef>), ValueError> {
        match self {
            Self::SlicePair(ty, seq_map) => Ok((ty.clone(), seq_map)),
            _ => Err(ValueError::ConversionError(
                "Expected slice_pair value".into(),
            )),
        }
    }

    /// # Errors
    ///
    pub fn expect_float(&self) -> Result<Fp, ValueError> {
        match self {
            Self::Float(v) => Ok(*v),
            _ => Err(ValueError::ConversionError("Expected float value".into())),
        }
    }

    /// # Errors
    ///
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Bool(b) => Ok(*b),
            _ => Err(ValueError::ConversionError("Expected bool value".into())),
        }
    }

    /// # Errors
    ///
    pub fn is_truthy(&self) -> Result<bool, ValueError> {
        let v = match self {
            Self::Bool(b) => *b,
            _ => {
                return Err(ValueError::ConversionError(format!(
                    "Expected bool value {}",
                    self
                )));
            }
        };

        Ok(v)
    }

    /// # Errors
    ///
    #[must_use]
    pub fn downcast_rust<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            Self::RustValue(_rust_type_ref, rc) => {
                let type_matches = {
                    let guard = rc.borrow();
                    (**guard).as_any().is::<T>()
                };

                if type_matches {
                    Some(unsafe { std::mem::transmute(rc.clone()) })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn downcast_hidden_rust<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            Self::AnonymousStruct(_struct_like_type, fields) => fields[0].borrow().downcast_rust(),
            _ => None,
        }
    }

    pub fn new_rust_value<T: RustType + 'static + PartialEq>(
        rust_type_ref: ExternalType,
        value: T,
    ) -> Self {
        let boxed = Box::new(Box::new(value)) as Box<dyn RustType>;
        Self::RustValue(rust_type_ref, Rc::new(RefCell::new(boxed)))
    }

    pub fn new_hidden_rust_struct<T: RustType + 'static + PartialEq>(
        struct_type: NamedStructType,
        rust_description: ExternalType,
        value: T,
    ) -> Self {
        let rust_value = Rc::new(RefCell::new(Self::new_rust_value(rust_description, value)));
        Self::AnonymousStruct(
            StructLikeType {
                assigned_name: "rust".to_string(),
                anonymous_struct_type: struct_type.anon_struct_type,
            },
            vec![rust_value],
        )
    }
}

fn generate_range_int_iterator(
    start: i32,
    end: i32,
    is_inclusive: bool,
) -> Box<dyn Iterator<Item = i32>> {
    if start > end {
        if is_inclusive {
            Box::new((end..=start).rev())
        } else {
            Box::new((end.saturating_add(1)..=start).rev())
        }
    } else if is_inclusive {
        Box::new(start..=end)
    } else {
        Box::new(start..end)
    }
}

fn generate_range_value_iterator(
    start: i32,
    exclusive_end: i32,
    is_inclusive: bool,
) -> Result<Box<dyn Iterator<Item = Value>>, ValueError> {
    Ok(Box::new(
        generate_range_int_iterator(start, exclusive_end, is_inclusive).map(Value::Int),
    ))
}

impl Display for Value {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Vec(_item_type, arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val.borrow())?;
                }
                write!(f, "]")
            }
            Self::Lambda(_a, _b) => {
                writeln!(f, "lambda")
            }
            Self::Grid(grid) => {
                writeln!(f, "[grid:")?;
                for row in grid.rows() {
                    write!(f, "[")?;
                    for (i, val) in row.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", val.borrow())?;
                    }
                    writeln!(f, "]")?;
                }
                writeln!(f, "]")
            }
            Self::Map2(map2) => {
                print_grid(map2);
                Ok(())
            }
            Self::Map(_key, items) => {
                write!(f, "[")?;
                for (i, (key, val)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {}", val.borrow())?;
                }
                write!(f, "]")
            }

            Self::Slice(_slice_type, arr) => {
                write!(f, "Slice[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val.borrow())?;
                }
                write!(f, "]")
            }
            Self::SlicePair(_ty, items) => {
                write!(f, "SlicePair[")?;
                for (i, (key, val)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {}", val.borrow())?;
                }
                write!(f, "]")
            }

            Self::Tuple(_tuple_type, arr) => {
                write!(f, "(")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val.borrow())?;
                }
                write!(f, ")")
            }

            Self::AnonymousStruct(struct_like_type, fields_in_strict_order) => {
                if !struct_like_type.assigned_name.is_empty() {
                    write!(f, "{} ", struct_like_type.assigned_name)?;
                }

                write!(f, "{{ ")?;

                let fields = struct_like_type
                    .anonymous_struct_type
                    .field_name_sorted_fields
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>();
                for (i, val) in fields_in_strict_order.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let field_name = &fields[i];
                    write!(f, "{field_name}: {}", val.borrow())?;
                }
                write!(f, " }}")
            }
            Self::InternalFunction(internal_fn) => write!(f, "[fn {}]", internal_fn.assigned_name),
            Self::Unit => write!(f, "()"),

            Self::ExternalFunction(external_function) => {
                write!(f, "[external_fn {}]", external_function.assigned_name)
            }

            // Enums ----
            Self::EnumVariantTuple(enum_type, enum_name, fields_in_order) => {
                write!(
                    f,
                    "{}::{}(",
                    enum_type.assigned_name, enum_name.common.assigned_name,
                )?;

                for (index, field) in fields_in_order.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field.borrow())?;
                }

                write!(f, ")")?;

                Ok(())
            }
            Self::EnumVariantStruct(enum_type, struct_variant, values) => {
                let decorated_values: Vec<(String, ValueRef)> = struct_variant
                    .anon_struct
                    .field_name_sorted_fields
                    .keys()
                    .cloned()
                    .zip(values.clone())
                    .collect();

                write!(
                    f,
                    "{}::{}{{",
                    enum_type.assigned_name, struct_variant.common.assigned_name,
                )?;

                for (index, (field_name, value)) in decorated_values.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{field_name}: {}", value.borrow())?;
                }

                write!(f, "}}")?;
                Ok(())
            }
            Self::EnumVariantSimple(enum_type, enum_variant_type_ref) => {
                write!(
                    f,
                    "{}::{}",
                    enum_type.assigned_name, enum_variant_type_ref.common.assigned_name,
                )
            }
            Self::RustValue(_rust_type, rust_type_pointer) => {
                write!(f, "{}", rust_type_pointer.borrow())
            }
            Self::Option(maybe_val) => {
                let inner_str = if let Some(inner_value) = maybe_val {
                    &*inner_value.borrow().to_string()
                } else {
                    "none"
                };
                write!(f, "Option({inner_str})")
            }
            Self::Sparse(_, _) => todo!(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Regular value comparisons
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Unit, Self::Unit) => true,
            (Self::EnumVariantSimple(_enum_type, a), Self::EnumVariantSimple(_enum_type_b, b)) => {
                a == b
            }
            (
                Self::EnumVariantTuple(_enum_type, a, a_values),
                Self::EnumVariantTuple(_enum_type_b, b, b_values),
            ) => (a == b) && a_values == b_values,
            (
                Self::EnumVariantStruct(_enum_type, a, a_values),
                Self::EnumVariantStruct(_enum_type_b, b, b_values),
            ) => (a == b) && a_values == b_values,
            (Self::Option(a), Self::Option(b)) => a == b,
            (Self::Vec(a, a_values), Self::Vec(b, b_values)) => (a == b) && a_values == b_values,
            (Self::Map(a, a_values), Self::Map(b, b_values)) => (a == b) && a_values == b_values,
            (Self::Tuple(a, a_values), Self::Tuple(b, b_values)) => {
                (a == b) && a_values == b_values
            }
            /*
            (Self::NamedStruct(a, a_values), Self::NamedStruct(b, b_values)) => {
                (a == b) && a_values == b_values
            }

             */
            (Self::AnonymousStruct(a, a_values), Self::AnonymousStruct(b, b_values)) => {
                (a == b) && a_values == b_values
            }
            (Self::InternalFunction(a), Self::InternalFunction(b)) => a == b,
            (Self::ExternalFunction(a), Self::ExternalFunction(b)) => a.id == b.id,
            (Self::RustValue(_a, a_val), Self::RustValue(_b, b_val)) => {
                a_val.borrow().eq_dyn(&**b_val.borrow())
            }

            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(n) => n.hash(state),
            Self::Float(f) => f.hash(state),
            Self::String(s) => s.hash(state),
            Self::Bool(b) => b.hash(state),
            Self::Unit => (),
            Self::Option(wrapped) => {
                if let Some(found) = wrapped {
                    1.hash(state);
                    found.borrow().hash(state);
                } else {
                    0.hash(state);
                }
            }
            Self::Lambda(_a, _b) => {
                todo!()
            }
            Self::Vec(_, _arr) => {}
            Self::Grid(_) => {}
            Self::Slice(_, _arr) => {}

            Self::AnonymousStruct(type_ref, values) => {
                type_ref.hash(state);
                // TODO: Not correct hash
                for v in values {
                    v.borrow().hash(state);
                }
            }
            Self::Map(_, values) => {
                for (key, val) in values {
                    key.hash(state);
                    val.borrow().hash(state);
                }
            }
            Self::Map2(_items) => {}
            Self::SlicePair(_, _items) => {}
            Self::Tuple(_, values) => {
                for v in values {
                    v.borrow().hash(state);
                }
            }
            Self::EnumVariantSimple(_, _) => (),
            Self::EnumVariantTuple(_, _, _fields) => todo!(),
            Self::EnumVariantStruct(_, _, _fields) => todo!(),
            Self::RustValue(_rust_type, _rust_val) => (),
            Self::InternalFunction(_) => (),
            Self::ExternalFunction(_) => (),
            Self::Sparse(_, _) => todo!(),
        }
    }
}

/// # Errors
///
pub fn format_value(value: &Value, spec: &FormatSpecifierKind) -> Result<String, String> {
    match (value, spec) {
        (Value::Int(n), FormatSpecifierKind::LowerHex) => Ok(format!("{n:x}")),
        (Value::Int(n), FormatSpecifierKind::UpperHex) => Ok(format!("{n:X}")),
        (Value::Int(n), FormatSpecifierKind::Binary) => Ok(format!("{n:b}")),

        (Value::Float(f), FormatSpecifierKind::Float) => Ok(format!("{f}")),
        (Value::Float(f), FormatSpecifierKind::Precision(prec, _node, PrecisionType::Float)) => {
            Ok(format!("{:.*}", *prec as usize, f))
        }

        (
            Value::String(s),
            FormatSpecifierKind::Precision(prec, _node, PrecisionType::String, ..),
        ) => Ok(format!("{:.*}", *prec as usize, s)),

        _ => Err(format!(
            "Unsupported format specifier {spec:?} for value type {value:?}"
        )),
    }
}

#[must_use]
pub fn convert_vec_to_rc_refcell(vec: Vec<Value>) -> Vec<Rc<RefCell<Value>>> {
    vec.into_iter().map(|v| Rc::new(RefCell::new(v))).collect()
}
