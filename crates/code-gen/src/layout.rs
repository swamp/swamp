//! Layouts analyzed into Vm Types (`BasicType`)
use crate::FrameAndVariableInfo;
use crate::alloc::ScopeAllocator;
use crate::reg_pool::RegisterPool;
use seq_map::SeqMap;
use source_map_node::Node;
use std::cmp::max;
use std::fmt::Write;
use swamp_semantic::{VariableRef, VariableType};
use swamp_types::{AnonymousStructType, EnumVariantType, NamedStructType, Type};
use swamp_vm_types::aligner::align;
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, FrameAddressInfo, FrameMemoryInfo, OffsetMemoryItem, StructType,
    TaggedUnion, TaggedUnionVariant, TupleType, VariableInfo, VariableInfoKind, VariableRegister,
    VmType, range_type,
};
use swamp_vm_types::{
    FrameMemoryAddress, FrameMemoryRegion, MAP_HEADER_SIZE, MemoryAlignment, MemoryOffset,
    MemorySize, PTR_ALIGNMENT, PTR_SIZE, STRING_PTR_ALIGNMENT, STRING_PTR_SIZE, VEC_HEADER_SIZE,
    VEC_PTR_ALIGNMENT, VEC_PTR_SIZE, adjust_size_to_alignment, align_to,
};
use tracing::info;
use tracing::trace;

#[derive(Copy, Clone)]
struct VariantLayout {
    pub size: MemorySize,
    pub alignment: MemoryAlignment,
}

#[derive(Copy, Clone)]
pub struct TaggedUnionLayout {
    pub tag_offset: MemoryOffset,
    pub tag_size: MemorySize,
    pub tag_alignment: MemoryAlignment,
    pub payload_offset: MemoryOffset,
    pub payload_size: MemorySize,
    pub payload_alignment: MemoryAlignment,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

fn layout_tagged_union(variants: &[VariantLayout]) -> TaggedUnionLayout {
    let num_variants = variants.len();
    let (tag_size, tag_alignment) = if num_variants <= 0xFF {
        (MemorySize(1), MemoryAlignment::U8)
    } else if num_variants <= 0xFFFF {
        (MemorySize(2), MemoryAlignment::U16)
    } else {
        (MemorySize(4), MemoryAlignment::U32)
    };

    let max_payload_size = variants
        .iter()
        .map(|v| v.size)
        .max()
        .unwrap_or(MemorySize(0));
    let max_payload_alignment = variants
        .iter()
        .map(|v| v.alignment)
        .max()
        .unwrap_or(MemoryAlignment::U8);

    let payload_offset = align_to(MemoryOffset(tag_size.0), max_payload_alignment);
    let max_alignment = std::cmp::max(tag_alignment, max_payload_alignment);

    let complete_size_before_alignment = MemorySize(payload_offset.0 + max_payload_size.0);
    let total_size = adjust_size_to_alignment(complete_size_before_alignment, max_alignment);

    TaggedUnionLayout {
        tag_offset: MemoryOffset(0),
        tag_size,
        tag_alignment,
        payload_offset,
        payload_size: max_payload_size,
        payload_alignment: max_payload_alignment,
        total_size,
        max_alignment,
    }
}

#[allow(clippy::too_many_lines)]
#[must_use]
pub fn layout_enum_into_tagged_union(name: &str, variants: &[EnumVariantType]) -> TaggedUnion {
    let variant_infos = variants.iter().map(|variant| match variant {
        EnumVariantType::Struct(s) => {
            let struct_type = layout_struct_type(&s.anon_struct, &s.common.assigned_name);
            (
                VariantLayout {
                    size: struct_type.total_size,
                    alignment: struct_type.max_alignment,
                },
                TaggedUnionVariant {
                    name: s.common.assigned_name.clone(),
                    ty: BasicType {
                        total_size: struct_type.total_size,
                        max_alignment: struct_type.max_alignment,
                        kind: BasicTypeKind::Struct(struct_type),
                    },
                },
            )
        }
        EnumVariantType::Tuple(t) => {
            let tuple_type = layout_tuple_items(&t.fields_in_order);
            (
                VariantLayout {
                    size: tuple_type.total_size,
                    alignment: tuple_type.max_alignment,
                },
                TaggedUnionVariant {
                    name: t.common.assigned_name.clone(),
                    ty: BasicType {
                        total_size: tuple_type.total_size,
                        max_alignment: tuple_type.max_alignment,
                        kind: BasicTypeKind::Tuple(tuple_type),
                    },
                },
            )
        }

        EnumVariantType::Nothing(n) => (
            VariantLayout {
                size: MemorySize(0),
                alignment: MemoryAlignment::U8,
            },
            TaggedUnionVariant {
                name: n.common.assigned_name.clone(),
                ty: BasicType {
                    total_size: MemorySize(0),
                    max_alignment: MemoryAlignment::U8,
                    kind: BasicTypeKind::Empty,
                },
            },
        ),
    });

    let (variant_layouts, tagged_variants): (Vec<VariantLayout>, Vec<TaggedUnionVariant>) =
        variant_infos.into_iter().unzip();
    let tagged_union_layout = layout_tagged_union(&variant_layouts);

    TaggedUnion {
        name: name.to_string(),
        tag_offset: tagged_union_layout.tag_offset,
        tag_size: tagged_union_layout.tag_size,
        payload_max_size: tagged_union_layout.payload_size,
        payload_offset: tagged_union_layout.payload_offset,
        variants: tagged_variants,
        total_size: tagged_union_layout.total_size,
        max_alignment: tagged_union_layout.max_alignment,
    }
}

#[must_use]
pub fn layout_enum(name: &str, variants: &[EnumVariantType]) -> BasicType {
    let tagged_union = layout_enum_into_tagged_union(name, variants);

    BasicType {
        total_size: tagged_union.total_size,
        max_alignment: tagged_union.max_alignment,
        kind: BasicTypeKind::TaggedUnion(tagged_union),
    }
}

const fn create_basic_type(
    basic_type_kind: BasicTypeKind,
    size: MemorySize,
    alignment: MemoryAlignment,
) -> BasicType {
    BasicType {
        kind: basic_type_kind,
        total_size: size,
        max_alignment: alignment,
    }
}

/// Computes the memory layout for a type in the target architecture.
///
/// In compiler terminology:
///
/// - "layout" determines size, alignment, and field offsets of types
/// - "materialization" refers to how types are represented in memory
/// - "lowering" is the process of mapping source types to machine types
///
/// This function performs type lowering by mapping high-level types to their
/// concrete memory representations, handling:
///
/// - Primitive, scalar types (integers, floats, booleans)
/// - Aggregate types (structs, tuples, enums)
/// - Reference types (pointers, slices)
/// - Collection types (vectors, maps)
///
/// The layout process considers:
///
/// - Size: Total bytes needed for the type
/// - Alignment: Memory boundary requirements
/// - Padding: Gaps needed for proper field alignment
/// - ABI: Target architecture requirements
///
/// # Panics
///
#[allow(clippy::too_many_lines)]
#[must_use]
pub fn layout_type(ty: &Type) -> BasicType {
    match ty {
        Type::Int => create_basic_type(BasicTypeKind::S32, MemorySize(4), MemoryAlignment::U32),
        Type::Float => {
            create_basic_type(BasicTypeKind::Fixed32, MemorySize(4), MemoryAlignment::U32)
        }
        Type::Bool => create_basic_type(BasicTypeKind::B8, MemorySize(1), MemoryAlignment::U8),
        Type::Unit => create_basic_type(BasicTypeKind::Empty, MemorySize(0), MemoryAlignment::U8),
        Type::String => create_basic_type(
            BasicTypeKind::InternalStringPointer,
            STRING_PTR_SIZE,
            STRING_PTR_ALIGNMENT,
        ),
        Type::Range(_) => range_type(),
        Type::SliceView(element_type) => {
            let element_type_basic = layout_type(element_type);
            create_basic_type(
                BasicTypeKind::SliceView(Box::from(element_type_basic)),
                PTR_SIZE,
                PTR_ALIGNMENT,
            )
        }

        // Fixed Capacity Array and Vec Storage are the same when it comes to layout
        Type::FixedCapacityAndLengthArray(element_type, fixed_size_element_count) => {
            let element_type_basic = layout_type(element_type);
            let total_size = element_type_basic.total_size.0 as usize * fixed_size_element_count
                + VEC_HEADER_SIZE.0 as usize;
            let max_alignment = max(element_type_basic.max_alignment, MemoryAlignment::U16);
            create_basic_type(
                BasicTypeKind::FixedCapacityArray(
                    Box::from(element_type_basic),
                    *fixed_size_element_count,
                ),
                MemorySize(total_size as u16),
                max_alignment,
            )
        }

        Type::VecStorage(element_type, fixed_size_element_count) => {
            let element_type_basic = layout_type(element_type);
            let total_size = element_type_basic.total_size.0 as usize * fixed_size_element_count
                + VEC_HEADER_SIZE.0 as usize;
            let max_alignment = max(element_type_basic.max_alignment, MemoryAlignment::U16);
            create_basic_type(
                BasicTypeKind::VecStorage(Box::from(element_type_basic), *fixed_size_element_count),
                MemorySize(total_size as u16),
                max_alignment,
            )
        }
        Type::DynamicLengthMapView(key_type, element_type) => {
            let tuple_gen_type = layout_tuple_items(&[*key_type.clone(), *element_type.clone()]);
            create_basic_type(
                BasicTypeKind::DynamicLengthMapView(
                    Box::from(tuple_gen_type.fields[0].clone()),
                    Box::from(tuple_gen_type.fields[1].clone()),
                ),
                PTR_SIZE,
                PTR_ALIGNMENT,
            )
        }
        Type::MapStorage(key_type, element_type, fixed_size_element_count) => {
            let tuple_gen_type = layout_tuple_items(&[*key_type.clone(), *element_type.clone()]);
            let total_size = tuple_gen_type.total_size.0 as usize * fixed_size_element_count
                + MAP_HEADER_SIZE.0 as usize;
            let max_alignment = max(tuple_gen_type.max_alignment, MemoryAlignment::U16);
            create_basic_type(
                BasicTypeKind::MapStorage(Box::from(tuple_gen_type), *fixed_size_element_count),
                MemorySize(total_size as u16),
                max_alignment,
            )
        }

        Type::DynamicLengthVecView(inner_type) => {
            let inner_gen_type = layout_type(inner_type);
            create_basic_type(
                BasicTypeKind::DynamicLengthVecView(Box::from(inner_gen_type)),
                PTR_SIZE,
                PTR_ALIGNMENT,
            )
        }
        Type::InternalInitializerPairList(a, b) => todo!(),
        Type::Tuple(types) => layout_tuple(types),
        Type::NamedStruct(named_struct_type) => {
            layout_named_struct(named_struct_type) // NOTE: memory_offset removed
        }
        Type::AnonymousStruct(anon_struct_type) => {
            layout_struct(anon_struct_type, "") // NOTE: memory_offset removed
        }
        Type::Enum(a) => layout_enum(
            &a.assigned_name,
            &a.variants.values().cloned().collect::<Vec<_>>(),
        ),
        Type::Optional(inner_type) => layout_optional_type(inner_type),
        Type::MutableReference(inner_type) => layout_mutable_reference(inner_type),
        // ----------
        Type::InternalInitializerList(inner_type) => {
            panic!("InternalInitializerList should not be a part of codegen");
        }
        Type::Function(_) => panic!("function types should not be a part of codegen"),
        Type::Never => panic!("'never' should not be a part of codegen"),
    }
}

fn layout_mutable_reference(analyzed_type: &Type) -> BasicType {
    if analyzed_type.is_primitive() {
        // For primitives, just use the primitive type directly
        // The ABI passes primitives inside the register directly and
        // have a copy back scheme that copies the register back afterward.
        return layout_type(analyzed_type);
    }

    let inner_type = layout_type(analyzed_type);
    BasicType {
        total_size: inner_type.total_size,
        max_alignment: inner_type.max_alignment,
        kind: BasicTypeKind::MutablePointer(Box::from(inner_type)),
    }
}

fn layout_named_struct(named_struct_type: &NamedStructType) -> BasicType {
    if named_struct_type.is_vec() {
        return create_basic_type(
            BasicTypeKind::DynamicLengthVecView(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }

    if named_struct_type.is_grid() {
        return create_basic_type(
            BasicTypeKind::DynamicLengthVecView(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }
    if named_struct_type.is_stack() {
        return create_basic_type(
            BasicTypeKind::DynamicLengthVecView(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }

    if named_struct_type.is_queue() {
        return create_basic_type(
            BasicTypeKind::DynamicLengthVecView(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }

    layout_struct(
        &named_struct_type.anon_struct_type,
        //memory_offset,
        &named_struct_type.assigned_name,
    )
}

#[must_use]
pub fn layout_struct_type(struct_type: &AnonymousStructType, name: &str) -> StructType {
    let mut offset = MemoryOffset(0);
    let mut max_alignment = MemoryAlignment::U8;
    let mut items = Vec::with_capacity(struct_type.field_name_sorted_fields.len());

    for (field_name, field_type) in &struct_type.field_name_sorted_fields {
        let field_layout = layout_type(&field_type.field_type);

        offset = align_to(offset, field_layout.max_alignment);

        items.push(OffsetMemoryItem {
            offset,
            size: field_layout.total_size,
            name: field_name.clone(),
            ty: field_layout.clone(),
        });

        offset = offset + field_layout.total_size;

        if field_layout.max_alignment > max_alignment {
            max_alignment = field_layout.max_alignment;
        }
    }

    let total_size = adjust_size_to_alignment(offset.as_size(), max_alignment);

    StructType {
        name: name.to_string(),
        fields: items,
        total_size,
        max_alignment,
    }
}

#[must_use]
pub fn layout_struct(struct_type: &AnonymousStructType, name: &str) -> BasicType {
    let inner_struct = layout_struct_type(struct_type, name);
    BasicType {
        total_size: inner_struct.total_size,
        max_alignment: inner_struct.max_alignment,
        kind: BasicTypeKind::Struct(inner_struct),
    }
}

#[must_use]
pub fn layout_optional_type(inner_type: &Type) -> BasicType {
    let tagged_union_type = layout_optional_type_items(inner_type);
    BasicType {
        total_size: tagged_union_type.total_size,
        max_alignment: tagged_union_type.max_alignment,
        kind: BasicTypeKind::Optional(tagged_union_type),
    }
}

#[must_use]
pub fn layout_optional_type_items(inner_type: &Type) -> TaggedUnion {
    let gen_type = layout_type(inner_type);
    let payload_variant = VariantLayout {
        size: gen_type.total_size,
        alignment: gen_type.max_alignment,
    };
    let none_variant = VariantLayout {
        size: MemorySize(0),
        alignment: MemoryAlignment::U8,
    };
    let tagged = layout_tagged_union(&[none_variant, payload_variant]);

    let payload_tagged_variant = TaggedUnionVariant {
        name: "Some".to_string(),
        ty: layout_type(inner_type),
    };

    let none_tagged_variant = TaggedUnionVariant {
        name: "None".to_string(),
        ty: BasicType {
            kind: BasicTypeKind::Empty,
            total_size: MemorySize(0),
            max_alignment: MemoryAlignment::U8,
        },
    };

    TaggedUnion {
        name: "option".to_string(),
        tag_offset: tagged.tag_offset,
        tag_size: tagged.tag_size,
        payload_max_size: tagged.payload_size,
        payload_offset: tagged.payload_offset,
        variants: vec![none_tagged_variant, payload_tagged_variant],
        total_size: tagged.total_size,
        max_alignment: tagged.max_alignment,
    }
}

#[must_use]
pub fn layout_tuple_items(types: &[Type]) -> TupleType {
    let mut offset = MemoryOffset(0);
    let mut max_alignment = MemoryAlignment::U8;
    let mut items = Vec::with_capacity(types.len());

    for (i, ty) in types.iter().enumerate() {
        let elem_layout = layout_type(ty);

        offset = align_to(offset, elem_layout.max_alignment);

        items.push(OffsetMemoryItem {
            offset,
            size: elem_layout.total_size,
            name: i.to_string(),
            ty: elem_layout.clone(),
        });

        offset = offset + elem_layout.total_size;

        if elem_layout.max_alignment > max_alignment {
            max_alignment = elem_layout.max_alignment;
        }
    }

    let total_size = adjust_size_to_alignment(offset.as_size(), max_alignment);

    TupleType {
        fields: items,
        total_size,
        max_alignment,
    }
}

#[must_use]
pub fn layout_tuple(types: &[Type]) -> BasicType {
    let tuple_type = layout_tuple_items(types);

    BasicType {
        total_size: tuple_type.total_size,
        max_alignment: tuple_type.max_alignment,
        kind: BasicTypeKind::Tuple(tuple_type),
    }
}

/// # Errors
///
#[allow(clippy::too_many_lines)]
pub fn layout_variables(
    _node: &Node,
    parameters: &Vec<VariableRef>,
    variables: &Vec<VariableRef>,
    exp_return_type: &Type,
) -> FrameAndVariableInfo {
    const TEMPORARY_SIZE: MemorySize = MemorySize(8 * 1024);

    let mut local_frame_allocator = ScopeAllocator::new(FrameMemoryRegion::new(
        FrameMemoryAddress(0),
        MemorySize(16 * 1024),
    ));

    //    let return_placed_type_pointer = layout_type(exp_return_type).create_mutable_pointer();
    //let return_placed_type = allocator.allocate_type(return_placed_type_pointer); //reserve(return_placed_type_pointer, &mut allocator);

    let mut enter_comment = "variables:\n".to_string();
    let mut frame_memory_infos = Vec::new();

    let mut parameter_and_variable_registers = SeqMap::new();
    let mut frame_register_allocator = RegisterPool::new(1, 96); // Should start at register 7

    let mut parameter_registers = Vec::new();
    for var_ref in parameters {
        let parameter_basic_type = layout_type(&var_ref.resolved_type);

        let register = frame_register_allocator.alloc_register(
            VmType::new_contained_in_register(parameter_basic_type),
            &format!("param {}", var_ref.assigned_name),
        );

        parameter_registers.push(VariableRegister {
            variable: VariableInfo {
                is_mutable: var_ref.is_mutable(),
                name: var_ref.assigned_name.clone(),
            },
            register: register.clone(),
        });

        parameter_and_variable_registers
            .insert(var_ref.unique_id_within_function, register)
            .unwrap();
    }

    //info!(len = variables.len(), "variables");

    let mut variable_registers = Vec::new();
    for var_ref in variables {
        let basic_type = layout_type(&var_ref.resolved_type);
        let register = if basic_type.is_represented_as_a_pointer_in_reg() {
            // TODO: Should have a check if the variable needs the storage (if it is in an assignment in a copy)

            let var_frame_placed_type = local_frame_allocator.allocate_type(basic_type);
            trace!(?var_ref.assigned_name, ?var_frame_placed_type, "laying out");
            writeln!(
                &mut enter_comment,
                "  {}:{} {}",
                var_frame_placed_type.addr(),
                var_frame_placed_type.size().0,
                var_ref.assigned_name
            )
            .unwrap();

            let kind = match var_ref.variable_type {
                VariableType::Local => VariableInfoKind::Variable(VariableInfo {
                    is_mutable: var_ref.is_mutable(),
                    name: var_ref.assigned_name.clone(),
                }),
                VariableType::Parameter => VariableInfoKind::Parameter(VariableInfo {
                    is_mutable: var_ref.is_mutable(),
                    name: var_ref.assigned_name.clone(),
                }),
            };

            frame_memory_infos.push(FrameAddressInfo {
                kind,
                frame_placed_type: var_frame_placed_type.clone(),
            });

            frame_register_allocator.alloc_register(
                VmType::new_frame_placed(var_frame_placed_type),
                &format!("var mut {}", var_ref.assigned_name),
            )
        } else {
            frame_register_allocator.alloc_register(
                VmType::new_contained_in_register(basic_type),
                &format!("var immute {}", var_ref.assigned_name),
            )
        };

        variable_registers.push(VariableRegister {
            variable: VariableInfo {
                is_mutable: var_ref.is_mutable(),
                name: var_ref.assigned_name.clone(),
            },
            register: register.clone(),
        });

        parameter_and_variable_registers
            .insert(var_ref.unique_id_within_function, register)
            .unwrap();
    }

    let variable_space = local_frame_allocator.addr().as_size();
    let allocate_for_temp = if variable_space.0 > (TEMPORARY_SIZE.0 / 2) {
        TEMPORARY_SIZE
    } else {
        let aligned = TEMPORARY_SIZE.0 as usize - align(variable_space.0 as usize, 8);
        MemorySize(aligned as u16)
    };

    let temp_allocator_region = FrameMemoryRegion {
        addr: local_frame_allocator.allocate(allocate_for_temp, MemoryAlignment::U64),
        size: allocate_for_temp,
    };

    let frame_size = local_frame_allocator.addr().as_size();

    let highest_register_used = frame_register_allocator.current_index;

    let return_type = VmType::new_contained_in_register(layout_type(exp_return_type));

    FrameAndVariableInfo {
        frame_memory: FrameMemoryInfo {
            infos: frame_memory_infos,
            total_frame_size: frame_size,
            variable_frame_size: temp_allocator_region.addr.as_size(),
            frame_size_for_variables_except_temp: variable_space,
            variable_registers,
        },
        temp_allocator_region,
        return_type,
        parameters: parameter_registers.clone(),
        parameter_and_variable_offsets: parameter_and_variable_registers,
        frame_registers: frame_register_allocator,
        highest_register_used,
    }
}
