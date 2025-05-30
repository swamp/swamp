/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::prelude::Value;
use fixed32::Fp;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_types::{EnumVariantType, Type};

/// # Panics
///
#[inline]
#[allow(clippy::too_many_lines)]
#[must_use]
pub fn quick_deserialize(resolved_type: &Type, buf: &[u8], depth: usize) -> (Value, usize) {
    let (val, octet_size) = match resolved_type {
        Type::Int => {
            let i = i32::from_le_bytes(buf[0..4].try_into().expect("REASON"));
            (Value::Int(i), 4)
        }

        Type::Float => {
            let i = i32::from_le_bytes(buf[0..4].try_into().expect("couldn't convert to Fp"));
            (Value::Float(Fp::from_raw(i)), 4)
        }
        Type::String => {
            let octet_len =
                u16::from_le_bytes(buf[0..2].try_into().expect("could not convert strlen"));
            let str =
                String::from_utf8(buf[2..2 + octet_len as usize].to_owned()).expect("utf8 error");
            (Value::String(str), (octet_len + 2) as usize)
        }
        Type::Bool => (Value::Bool(buf[0] != 0), 1),
        Type::Unit => (Value::Unit, 0),
        Type::Never => panic!("can not deserialize never type"),
        Type::MutableReference(_) => todo!(),
        /*
        Type::Vec(element_type) => {
            let mut offset = 0;
            let count = u16::from_le_bytes(
                buf[offset..offset + 2]
                    .try_into()
                    .expect("should work with u16"),
            );
            offset += 2;

            let item_ref = &element_type;

            let mut values = Vec::new();
            for _index in 0..count {
                let (value, item_octet_size) =
                    quick_deserialize(item_ref, &buf[offset..], depth + 1);

                offset += item_octet_size;

                values.push(Rc::new(RefCell::new(value)));
            }

            (Value::Vec(*element_type.clone(), values), offset)
        }

         */
        Type::Tuple(tuple_types) => {
            let mut offset = 0;
            let mut values = Vec::new();
            for tuple_item_type in tuple_types {
                let (value, item_octet_size) =
                    quick_deserialize(tuple_item_type, &buf[offset..], depth + 1);
                values.push(Rc::new(RefCell::new(value)));
                offset += item_octet_size;
            }
            (Value::Tuple(tuple_types.clone(), values), offset)
        }
        Type::NamedStruct(struct_type_ref) => {
            todo!()
        }
        /*
        Type::NamedStruct(struct_type_ref) => {
            let mut values = Vec::new();
            let mut offset = 0;
            for struct_field_type in struct_type_ref
                .anon_struct_type
                .field_name_sorted_fields
                .values()
            {
                let (value, octet_size) =
                    quick_deserialize(&struct_field_type.field_type, &buf[offset..], depth + 1);
                values.push(Rc::new(RefCell::new(value)));
                offset += octet_size;
            }
            (Value::NamedStruct(struct_type_ref.clone(), values), offset)
        }

         */
        Type::AnonymousStruct(_anon_struct_type) => {
            todo!()
        }
        /*
        Type::Map(key_type, value_type) => {
            let mut offset = 0;
            let count = u16::from_le_bytes(
                buf[offset..offset + 2]
                    .try_into()
                    .expect("should work with u16"),
            );
            offset += 2;

            let mut seq_map = SeqMap::new(); //SeqMap<Value, ValueRef>
            for _map_index in 0..count {
                let (key_val, key_octet_size) =
                    quick_deserialize(key_type, &buf[offset..], depth + 1);
                offset += key_octet_size;

                let (value_val, value_octet_size) =
                    quick_deserialize(value_type, &buf[offset..], depth + 1);
                offset += value_octet_size;

                let value_ref = Rc::new(RefCell::new(value_val));

                seq_map
                    .insert(key_val, value_ref)
                    .expect("should work to insert");
            }
            (
                Value::Map(*key_type.clone(), *value_type.clone(), seq_map),
                offset,
            )
        }

         */
        Type::Enum(enum_type) => {
            let mut offset = 0;
            let enum_lookup_index = buf[offset];
            offset += 1;
            assert!(enum_lookup_index < 8);

            let borrowed_enum = enum_type.clone();

            let variant_type = borrowed_enum
                .get_variant_from_index(enum_lookup_index as usize)
                .expect("should be able to find variant");

            let val = match variant_type {
                EnumVariantType::Struct(_) => {
                    todo!("struct containers not done yet")
                }
                EnumVariantType::Tuple(tuple_type_ref) => {
                    let mut vals_in_order = Vec::new();
                    for tuple_type in &tuple_type_ref.fields_in_order {
                        let (tuple_value, tuple_octet_size) =
                            quick_deserialize(tuple_type, &buf[offset..], depth + 1);
                        vals_in_order.push(Rc::new(RefCell::new(tuple_value)));
                        offset += tuple_octet_size;
                    }
                    Value::EnumVariantTuple(
                        enum_type.clone(),
                        tuple_type_ref.clone(),
                        vals_in_order,
                    )
                }
                EnumVariantType::Nothing(x) => {
                    offset += 0;
                    Value::EnumVariantSimple(enum_type.clone(), x.clone())
                }
            };

            (val, offset)
        }
        /*
        Type::Sparse(value_type) => {
            let sparse_type_id_rust_type = Rc::new(ExternalType {
                type_name: "SparseId".to_string(),
                number: SPARSE_ID_TYPE_ID, // TODO: FIX hardcoded number
            });

            let (internal_map, sparse_value_map_octet_size) = SparseValueMap::quick_deserialize(
                sparse_type_id_rust_type,
                *value_type.clone(),
                buf,
            );

            let sparse_collection_rust_type = Rc::new(ExternalType {
                type_name: "Sparse".to_string(),
                number: SPARSE_TYPE_ID, // TODO: FIX hardcoded number
            });

            (
                Value::Sparse(
                    swamp_types::Type::External(sparse_collection_rust_type),
                    internal_map,
                ),
                sparse_value_map_octet_size,
            )
        }
        Type::Grid(type_parameter) => {
            todo!()
        }

         */
        Type::Function(_) => {
            panic!("can not serialize function")
        }
        Type::Optional(optional_type_ref) => {
            let mut offset = 0;
            let has_some = buf[0] != 0;
            offset += 1;
            if has_some {
                let (v, octet_size) = quick_deserialize(optional_type_ref, &buf[1..], depth + 1);
                offset += octet_size;
                (Value::Option(Some(Rc::new(RefCell::new(v)))), offset)
            } else {
                (Value::Option(None), offset)
            }
        }
        &swamp_types::Type::VecStorage(_, _) | &swamp_types::Type::SliceView(_) => todo!(),
        &swamp_types::Type::InternalInitializerList(_)
        | &swamp_types::Type::InternalInitializerPairList(_, _) => todo!(),
        &swamp_types::Type::ImmutableReference(_) => todo!(),
        &swamp_types::Type::MapStorage(_, _, _)
        | &swamp_types::Type::DynamicLengthMapView(_, _) => todo!(),
        &swamp_types::Type::Range(_) => todo!(),
        &swamp_types::Type::DynamicLengthVecView(_) => todo!(),
        &swamp_types::Type::FixedCapacityAndLengthArray(_, _) => todo!(),
    };

    (val, octet_size)
}
