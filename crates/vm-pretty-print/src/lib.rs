/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod prelude;

use fixed32::Fp;
use std::fmt;
use std::fmt::{Display, Formatter, Write};
use std::ops::Add;
use swamp_vm::Vm;
use swamp_vm::memory::Memory;
use swamp_vm_types::types::{BasicType, BasicTypeKind, OffsetMemoryItem};
use swamp_vm_types::{HeapMemoryAddress, MemoryOffset, StackMemoryAddress};

pub fn new_line_and_tab(f: &mut dyn Write, tabs: usize) -> std::fmt::Result {
    let tab_str = "..".repeat(tabs);
    writeln!(f)?;
    write!(f, "{tab_str}")
}

#[allow(clippy::too_many_lines)]
pub fn print_value(
    f: &mut dyn Write,
    frame: &[u8],
    heap: &Memory,
    origin: StackMemoryAddress,
    ty: &BasicType,
    name: &str,
) -> fmt::Result {
    let item = OffsetMemoryItem {
        offset: MemoryOffset(0),
        size: ty.total_size,
        name: name.to_string(),
        ty: ty.clone(),
    };
    print(
        f,
        frame,
        heap,
        PrintAddress::StackMemoryAddress(origin),
        ty,
        name,
        0,
    )?;
    writeln!(f)
}

fn slice_to_u32_le(data: &[u8]) -> u32 {
    assert!(data.len() >= 4);
    let first_four_bytes = &data[..4];

    let bytes: [u8; 4] = first_four_bytes.try_into().unwrap();

    u32::from_le_bytes(bytes)
}

fn slice_to_i32_le(data: &[u8]) -> i32 {
    assert!(data.len() >= 4);
    let first_four_bytes = &data[..4];

    let bytes: [u8; 4] = first_four_bytes.try_into().unwrap();

    i32::from_le_bytes(bytes)
}

fn slice_to_u16_le(data: &[u8]) -> u16 {
    assert!(data.len() >= 2);
    let first_two_bytes = &data[..2];

    let bytes: [u8; 2] = first_two_bytes.try_into().unwrap();

    u16::from_le_bytes(bytes)
}

#[derive(Copy, Clone)]
pub enum PrintAddress {
    StackMemoryAddress(StackMemoryAddress),
    HeapMemoryAddress(HeapMemoryAddress),
}

impl Display for PrintAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::StackMemoryAddress(stack_addr) => {
                write!(f, "${:X}", stack_addr.0)
            }
            Self::HeapMemoryAddress(heap_addr) => {
                write!(f, "%{:X}", heap_addr.0)
            }
        }
    }
}

impl Add<MemoryOffset> for PrintAddress {
    type Output = Self;

    fn add(self, rhs: MemoryOffset) -> Self::Output {
        match self {
            Self::StackMemoryAddress(stack_addr) => Self::StackMemoryAddress(stack_addr + rhs),
            Self::HeapMemoryAddress(heap_addr) => {
                Self::HeapMemoryAddress(HeapMemoryAddress(heap_addr.0 + u32::from(rhs.0)))
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
fn print(
    f: &mut dyn Write,
    frame: &[u8],
    heap: &Memory,
    debug_origin: PrintAddress,
    ty: &BasicType,
    name: &str,
    indent: usize,
) -> fmt::Result {
    write!(
        f,
        "{}:{:X} |>   {}: ",
        debug_origin,
        ty.total_size.0,
        tinter::blue(name),
    )?;
    match &ty.kind {
        BasicTypeKind::Empty => write!(f, "()"),
        BasicTypeKind::U8 => {
            let value = frame[0];
            write!(f, "{}", tinter::yellow(value))
        }
        BasicTypeKind::B8 => {
            let byte = frame[0];
            let value = match byte {
                0 => false,
                1 => true,
                _ => panic!("illegal value for bool {byte}"),
            };
            write!(f, "{}", tinter::yellow(value))
        }
        BasicTypeKind::U16 => {
            let value = slice_to_u16_le(frame);
            write!(f, "{}", tinter::yellow(value))
        }
        BasicTypeKind::S32 => {
            let value = slice_to_i32_le(frame);
            write!(f, "{}", tinter::yellow(value))
        }
        BasicTypeKind::Fixed32 => {
            let int_value = slice_to_i32_le(frame);

            write!(f, "{}", tinter::yellow(Fp::from_raw(int_value)))
        }
        BasicTypeKind::U32 => {
            let value = slice_to_u32_le(frame);
            write!(f, "{}", tinter::yellow(value))
        }

        BasicTypeKind::InternalStringPointer => {
            let heap_addr = slice_to_u32_le(frame);
            let str = Vm::read_string(heap_addr, heap);
            write!(f, "\"{str}\" (%{heap_addr:X})")
        }
        BasicTypeKind::DynamicLengthVecView(item_type)
        | BasicTypeKind::QueueStorage(item_type, _)
        | BasicTypeKind::StackStorage(item_type, _)
        | BasicTypeKind::VecStorage(item_type, _) => {
            write!(f, "[")?;
            let header_offset = slice_to_u32_le(frame);
            let header = Vm::vec_header_from_heap(heap, header_offset);
            //let buckets = heap.get_heap_const_ptr(header.heap_offset as usize);
            for i in 0..header.element_count {}
            write!(f, "]")
        }
        BasicTypeKind::FixedCapacityArray(item_type, size) => {
            write!(f, "[")?;
            let header_offset = slice_to_u32_le(frame);
            let header = Vm::vec_header_from_heap(heap, header_offset);
            //let buckets = heap.get_heap_const_ptr(header.heap_offset as usize);
            for i in 0..header.element_count {}
            write!(f, "; {size}]")
        }
        BasicTypeKind::SparseView(element_type) => {
            todo!()
        }
        BasicTypeKind::SparseStorage(element_type, capacity) => {
            todo!()
        }
        BasicTypeKind::GridView(element_type) => {
            todo!()
        }
        BasicTypeKind::GridStorage(element_type, width, height) => {
            todo!()
        }
        BasicTypeKind::MapStorage {
            tuple_type,
            logical_limit: logical_size,
            ..
        } => {
            todo!()
        }

        BasicTypeKind::DynamicLengthMapView(key_type, value_type) => {
            write!(f, "[")?;
            /*
            let map_header_heap_addr = slice_to_u32_le(frame);
            let header = Vm::read_map_header_from_heap(map_header_heap_addr, heap);
            let buckets_ptr = heap.get_heap_const_ptr(header.heap_offset as usize);
            let pair_size = 1 + header.key_size + header.value_size;
            write!(
                f,
                "header: {map_header_heap_addr:X} key_size:{} value_size:{} capacity:{}",
                header.key_size, header.value_size, header.capacity,
            )?;
            for i in 0..header.capacity {
                let base_item_offset = i as usize * pair_size as usize;
                let base_ptr = unsafe { buckets_ptr.add(base_item_offset) };
                let status = unsafe { *base_ptr };
                if status == Vm::BUCKET_OCCUPIED {
                    let status_size = 1;
                    let source_key_ptr = unsafe { base_ptr.add(status_size) };
                    let source_value_ptr = unsafe { source_key_ptr.add(header.key_size as usize) };
                    let source_key_heap_addr = header.heap_offset + 1;

                    let key_octet_slice =
                        unsafe { slice::from_raw_parts(source_key_ptr, header.key_size as usize) };
                    new_line_and_tab(f, indent + 1)?;
                    print(
                        f,
                        key_octet_slice,
                        heap,
                        PrintAddress::HeapMemoryAddress(HeapMemoryAddress(
                            source_key_heap_addr as u32,
                        )),
                        key_type,
                        &format!("{i}"),
                        indent + 1,
                    )?;

                    let source_value_heap_addr = source_key_heap_addr + header.key_size;
                    let value_octet_slice = unsafe {
                        slice::from_raw_parts(source_value_ptr, header.value_size as usize)
                    };
                    new_line_and_tab(f, indent + 2)?;
                    print(
                        f,
                        value_octet_slice,
                        heap,
                        PrintAddress::HeapMemoryAddress(HeapMemoryAddress(
                            source_value_heap_addr as u32,
                        )),
                        value_type,
                        &format!("{i}"),
                        indent + 2,
                    )?;
                }
            }

             */
            write!(f, "]")
        }
        BasicTypeKind::InternalGridPointer => todo!(),

        BasicTypeKind::Struct(struct_type) => {
            write!(f, "{} {{", tinter::magenta(&struct_type.name))?;
            for (index, field_item) in struct_type.fields.iter().enumerate() {
                new_line_and_tab(f, indent + 1)?;
                print(
                    f,
                    &frame[field_item.offset.0 as usize..],
                    heap,
                    debug_origin + field_item.offset,
                    &field_item.ty,
                    &field_item.name,
                    indent + 1,
                )?;
            }
            write!(f, " }}")
        }
        BasicTypeKind::TaggedUnion(tagged_union) => {
            assert_eq!(
                tagged_union.tag_size.0, 1,
                "only small unions supported for print"
            );
            let variant_index = frame[tagged_union.tag_offset.0 as usize] as usize;
            let variant = tagged_union.get_variant_as_offset_item(variant_index);
            if variant.size.0 != 0 {
                write!(f, "{}:", variant.name)?;
                new_line_and_tab(f, indent + 1)?;
                print(
                    f,
                    &frame[variant.offset.0 as usize..],
                    heap,
                    debug_origin + variant.offset,
                    &variant.ty,
                    &variant.name,
                    indent + 1,
                )
            } else {
                write!(f, "{}", variant.name)
            }
        }
        BasicTypeKind::Tuple(tuple_type) => {
            write!(f, "(")?;
            for (index, tuple_item) in tuple_type.fields.iter().enumerate() {
                new_line_and_tab(f, indent + 1)?;
                print(
                    f,
                    &frame[tuple_item.offset.0 as usize..],
                    heap,
                    debug_origin + tuple_item.offset,
                    &tuple_item.ty,
                    &tuple_item.name,
                    indent + 1,
                )?;
            }
            write!(f, ")")
        }
        BasicTypeKind::Optional(tagged_union) => {
            write!(f, "optional: ")?;
            assert_eq!(
                tagged_union.tag_size.0, 1,
                "only small unions supported for print"
            );
            let variant_index = frame[0] as usize;
            if variant_index == 0 {
                write!(f, "none")
            } else {
                let variant = tagged_union.get_variant_as_offset_item(variant_index);
                //                new_line_and_tab(f, indent + 1)?;
                print(
                    f,
                    &frame[variant.offset.0 as usize..],
                    heap,
                    debug_origin + variant.offset,
                    &variant.ty,
                    &variant.name,
                    indent + 1,
                )
            }
        }

        BasicTypeKind::MutablePointer(_) => todo!(),
        BasicTypeKind::SliceView(_) => panic!("slices should not be stored"),
        BasicTypeKind::DynamicLengthMapView(_, _) => panic!("slice pairs should not be stored"),
        BasicTypeKind::InternalVecIterator => panic!("vec iterators should not be stored"),
        BasicTypeKind::InternalMapIterator => panic!("map iterators should not be stored"),
        BasicTypeKind::InternalSparseIterator => panic!("sparse iterators should not be stored"),
        BasicTypeKind::InternalRangeIterator => panic!("iterators should not be stored"),
        BasicTypeKind::InternalRangeHeader => panic!("ranges can not be stored"),
    }
}
