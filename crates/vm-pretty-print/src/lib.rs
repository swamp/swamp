use fixed32::Fp;
use std::env::var;
use std::fmt::Write;
use std::{fmt, slice};
use swamp_vm::Vm;
use swamp_vm::frame::FrameMemory;
use swamp_vm::heap::HeapMemory;
use swamp_vm_types::types::{BasicType, BasicTypeKind, OffsetMemoryItem};
use swamp_vm_types::{MemoryOffset, StackMemoryAddress};

pub fn new_line_and_tab(f: &mut dyn Write, tabs: usize) -> std::fmt::Result {
    let tab_str = "..".repeat(tabs);
    writeln!(f)?;
    write!(f, "{tab_str}")
}

#[allow(clippy::too_many_lines)]
pub fn print_value(
    f: &mut dyn Write,
    frame: &[u8],
    heap: &HeapMemory,
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
    print(f, frame, heap, origin, ty, name, 0)?;
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

#[allow(clippy::too_many_lines)]
fn print(
    f: &mut dyn Write,
    frame: &[u8],
    heap: &HeapMemory,
    debug_origin: StackMemoryAddress,
    ty: &BasicType,
    name: &str,
    indent: usize,
) -> fmt::Result {
    write!(
        f,
        "{:08X}:{:X} |>   {}: ",
        debug_origin.0, ty.total_size.0, name,
    )?;
    match &ty.kind {
        BasicTypeKind::Empty => write!(f, "()"),
        BasicTypeKind::U8 => {
            let value = frame[0];
            write!(f, "{value}")
        }
        BasicTypeKind::B8 => {
            let byte = frame[0];
            let value = match byte {
                0 => false,
                1 => true,
                _ => panic!("illegal value for bool {byte}"),
            };
            write!(f, "{value}")
        }
        BasicTypeKind::U16 => {
            let value = slice_to_u16_le(frame);
            write!(f, "{value}")
        }
        BasicTypeKind::S32 => {
            let value = slice_to_i32_le(frame);
            write!(f, "{value}")
        }
        BasicTypeKind::Fixed32 => {
            let int_value = slice_to_i32_le(frame);

            write!(f, "{}", Fp::from_raw(int_value))
        }
        BasicTypeKind::U32 => {
            let value = slice_to_u32_le(frame);
            write!(f, "{value}")
        }

        BasicTypeKind::InternalStringPointer => write!(f, "str"),
        BasicTypeKind::InternalVecPointer(item_type) => {
            write!(f, "[")?;
            let header_offset = slice_to_u32_le(frame);
            let header = Vm::vec_header_from_heap(heap, header_offset);
            //let buckets = heap.get_heap_const_ptr(header.heap_offset as usize);
            for i in 0..header.count {
                let item_offset = i as usize * header.element_size as usize;

                let item_ptr = heap.get_heap_const_ptr(header.heap_offset as usize + item_offset);
                let octet_slice =
                    unsafe { slice::from_raw_parts(item_ptr, header.element_size as usize) };
            }
            write!(f, "]")
        }
        BasicTypeKind::InternalMapPointer(key_type, value_type) => {
            write!(f, "[")?;
            let map_header_heap_addr = slice_to_u32_le(frame);
            let header = Vm::read_heap_map_header_from_heap(map_header_heap_addr, heap);
            let buckets_ptr = heap.get_heap_const_ptr(header.heap_offset as usize);
            let pair_size = 1 + header.key_size + header.value_size;
            eprintln!(
                "header: {map_header_heap_addr:08X} {} {}",
                header.key_size, header.value_size
            );
            for i in 0..header.capacity {
                let base_item_offset = i as usize * pair_size as usize;
                let base_ptr = unsafe { buckets_ptr.add(base_item_offset) };
                let status = unsafe { *base_ptr };
                if status == Vm::BUCKET_OCCUPIED {
                    let status_size = 1;
                    let source_key_ptr = unsafe { base_ptr.add(status_size) };
                    let source_value_ptr = unsafe { source_key_ptr.add(header.key_size as usize) };

                    let key_octet_slice =
                        unsafe { slice::from_raw_parts(source_key_ptr, header.key_size as usize) };
                    new_line_and_tab(f, indent + 1)?;
                    print(
                        f,
                        key_octet_slice,
                        heap,
                        StackMemoryAddress(0),
                        key_type,
                        &format!("{i}:KEY"),
                        indent + 1,
                    )?;

                    let value_octet_slice = unsafe {
                        slice::from_raw_parts(source_value_ptr, header.value_size as usize)
                    };
                    new_line_and_tab(f, indent + 1)?;
                    print(
                        f,
                        value_octet_slice,
                        heap,
                        StackMemoryAddress(0),
                        value_type,
                        &format!("{i}:VALUE"),
                        indent + 1,
                    )?;
                }
            }
            write!(f, "]")
        }
        BasicTypeKind::InternalGridPointer => todo!(),

        BasicTypeKind::Struct(struct_type) => {
            write!(f, "{} {{", struct_type.name)?;
            for (index, field_item) in struct_type.fields.iter().enumerate() {
                new_line_and_tab(f, indent)?;
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
                if index > 0 {
                    new_line_and_tab(f, indent)?;
                }
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
            }
        }

        BasicTypeKind::IndirectHeapPointerOnFrame => panic!("heap pointers can not be stored"),
        BasicTypeKind::Slice(_) => panic!("slices should not be stored"),
        BasicTypeKind::SlicePair(_, _) => panic!("slice pairs should not be stored"),
        BasicTypeKind::InternalVecIterator => panic!("vec iterators should not be stored"),
        BasicTypeKind::InternalMapIterator => panic!("map iterators should not be stored"),
        BasicTypeKind::InternalRangeIterator => panic!("iterators should not be stored"),
        BasicTypeKind::InternalRangeHeader => panic!("ranges can not be stored"),
    }
}
