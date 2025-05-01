use fixed32::Fp;
use std::fmt::Write;
use std::{fmt, slice};
use swamp_vm::Vm;
use swamp_vm::frame::FrameMemory;
use swamp_vm::heap::HeapMemory;
use swamp_vm_types::StackMemoryAddress;
use swamp_vm_types::types::{BasicTypeKind, OffsetMemoryItem};

pub fn new_line_and_tab(f: &mut dyn Write, tabs: usize) -> std::fmt::Result {
    let tab_str = "..".repeat(tabs);
    writeln!(f)?;
    write!(f, "{tab_str}")
}

#[allow(clippy::too_many_lines)]
pub fn print(
    f: &mut dyn Write,
    frame: &FrameMemory,
    heap: &HeapMemory,
    origin: StackMemoryAddress,
    item: &OffsetMemoryItem,
    indent: usize,
) -> fmt::Result {
    let total_frame_addr = origin.0 as u16 + item.offset.0;
    let total_frame_addr_origin = StackMemoryAddress(total_frame_addr as u32);
    write!(
        f,
        "{:08X}:{:X} |>   {}: ",
        total_frame_addr, item.size.0, item.name
    )?;
    match &item.ty.kind {
        BasicTypeKind::Empty => write!(f, "()"),
        BasicTypeKind::U8 => {
            let value = frame.read_frame_u8(total_frame_addr);
            write!(f, "{value}")
        }
        BasicTypeKind::B8 => {
            let byte = frame.read_frame_u8(total_frame_addr);
            let value = match byte {
                0 => false,
                1 => true,
                _ => panic!("illegal value for bool {byte}"),
            };
            write!(f, "{value}")
        }
        BasicTypeKind::U16 => {
            let value = frame.read_frame_u16(total_frame_addr);
            write!(f, "{value}")
        }
        BasicTypeKind::S32 => {
            let value = frame.read_frame_i32(total_frame_addr);
            write!(f, "{value}")
        }
        BasicTypeKind::Fixed32 => {
            let int_value = frame.read_frame_i32(total_frame_addr);

            write!(f, "{}", Fp::from_raw(int_value))
        }
        BasicTypeKind::U32 => {
            let value = frame.read_frame_u32(total_frame_addr);
            write!(f, "{value}")
        }

        BasicTypeKind::InternalStringPointer => todo!(),
        BasicTypeKind::InternalVecPointer(item_type) => {
            write!(f, "[")?;
            let header = Vm::vec_header_from_indirect_heap(frame, total_frame_addr, heap);
            let buckets = heap.get_heap_const_ptr(header.heap_offset as usize);
            for i in 0..header.count {
                let item_offset = i as usize * header.element_size as usize;

                let item_ptr = heap.get_heap_const_ptr(header.heap_offset as usize + item_offset);
                let octet_slice =
                    unsafe { slice::from_raw_parts(item_ptr, header.element_size as usize) };
            }
            write!(f, "]")
        }
        BasicTypeKind::InternalMapPointer => write!(f, "map<K,V>"),
        BasicTypeKind::InternalGridPointer => todo!(),

        BasicTypeKind::Struct(struct_type) => {
            write!(f, "{} {{", struct_type.name)?;
            for (index, field_item) in struct_type.fields.iter().enumerate() {
                new_line_and_tab(f, indent)?;
                print(
                    f,
                    frame,
                    heap,
                    total_frame_addr_origin,
                    field_item,
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
            let variant_index = frame
                .read_frame_u8(total_frame_addr_origin.0 as u16 + tagged_union.tag_offset.0)
                as usize;
            let variant = tagged_union.get_variant_as_offset_item(variant_index);
            if variant.size.0 != 0 {
                write!(f, "{}:", variant.name)?;
                new_line_and_tab(f, indent + 1)?;
                print(
                    f,
                    frame,
                    heap,
                    total_frame_addr_origin,
                    &variant,
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
                    frame,
                    heap,
                    total_frame_addr_origin,
                    tuple_item,
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
            let variant_index = frame
                .read_frame_u8(total_frame_addr_origin.0 as u16 + tagged_union.tag_offset.0)
                as usize;
            if variant_index == 0 {
                write!(f, "none")
            } else {
                let variant = tagged_union.get_variant_as_offset_item(variant_index);
                write!(f, "{}:", variant.name)?;
                new_line_and_tab(f, indent + 1)?;
                print(
                    f,
                    frame,
                    heap,
                    total_frame_addr_origin,
                    &variant,
                    indent + 1,
                )
            }
        }

        BasicTypeKind::IndirectHeapPointerOnFrame => todo!(),
        BasicTypeKind::Bytes => todo!(),
        BasicTypeKind::Slice(_) => panic!("slices should not be stored"),
        BasicTypeKind::SlicePair(_, _) => panic!("slice pairs should not be stored"),
        BasicTypeKind::InternalVecIterator => panic!("vec iterators should not be stored"),
        BasicTypeKind::InternalMapIterator => panic!("map iterators should not be stored"),
        BasicTypeKind::InternalRangeIterator => panic!("iterators should not be stored"),
        BasicTypeKind::InternalRangeHeader => panic!("ranges can not be stored"),
    }
}
