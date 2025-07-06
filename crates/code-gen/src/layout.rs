/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::FrameAndVariableInfo;
use crate::alloc::StackFrameAllocator;
use seq_map::SeqMap;
use source_map_node::Node;
use std::fmt::Write;
use swamp_semantic::{VariableScopes, VariableType};
use swamp_types::TypeRef;
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::{
    FrameAddressInfo, FrameMemoryInfo, TypedRegister, VariableInfo, VariableInfoKind,
    VariableRegister, VmType,
};
use swamp_vm_types::{FrameMemoryAddress, FrameMemoryRegion, MemorySize};

/// # Errors
///
#[allow(clippy::too_many_lines)]
#[must_use]
pub fn layout_variables(
    layout_cache: &mut LayoutCache,
    _node: &Node,
    scopes: &VariableScopes,
    exp_return_type: &TypeRef,
) -> FrameAndVariableInfo {
    let mut local_frame_allocator = StackFrameAllocator::new(FrameMemoryRegion::new(
        FrameMemoryAddress(0),
        MemorySize(3 * 1024 * 1024 * 1024),
    ));

    let mut enter_comment = "variables:\n".to_string();
    let mut frame_memory_infos = Vec::new();

    let mut all_variable_unique_to_register = SeqMap::new();

    let mut variable_registers_for_debug_info = Vec::new();
    for var_ref in &scopes.all_variables {
        let basic_type = layout_cache.layout(&var_ref.resolved_type);

        let vm_type = if basic_type.is_aggregate() {
            // TODO: Should have a check if the variable needs the storage (if it is in an assignment in a copy)
            swamp_vm_layout::check_type_size(
                &basic_type,
                &format!("variable '{}'", var_ref.assigned_name),
            );

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

            if matches!(var_ref.variable_type, VariableType::Local) {
                let var_frame_placed_type = local_frame_allocator.allocate_type(&basic_type);
                //trace!(?var_ref.assigned_name, ?var_frame_placed_type, "laying out");
                writeln!(
                    &mut enter_comment,
                    "  {}:{} {}",
                    var_frame_placed_type.addr(),
                    var_frame_placed_type.size().0,
                    var_ref.assigned_name
                )
                .unwrap();

                frame_memory_infos.push(FrameAddressInfo {
                    kind,
                    frame_placed_type: var_frame_placed_type.clone(),
                });
                VmType::new_frame_placed(var_frame_placed_type)
            } else {
                // even if it is an aggregate, parameters do not need any frame memory, they are allocated elsewhere
                VmType::new_contained_in_register(basic_type)
            }
        } else {
            // If it is a scalar, then it is in a register no matter if it is a parameter or local
            VmType::new_contained_in_register(basic_type)
        };

        let typed_register = TypedRegister {
            index: var_ref.virtual_register,
            ty: vm_type,
            comment: String::new(),
        };

        //info!(unique_id=?var_ref.unique_id_within_function, name=?var_ref.assigned_name, "insert variable");
        all_variable_unique_to_register
            .insert(var_ref.unique_id_within_function, typed_register.clone())
            .unwrap();

        let var_register = VariableRegister {
            unique_id_in_function: var_ref.unique_id_within_function,
            variable: VariableInfo {
                is_mutable: var_ref.is_mutable(),
                name: var_ref.assigned_name.clone(),
            },
            register: typed_register.clone(),
        };
        variable_registers_for_debug_info.push(var_register);
    }

    let variable_space = local_frame_allocator.addr().as_size();

    let frame_size = local_frame_allocator.addr().as_size();

    let return_type_ref = TypeRef::from(exp_return_type.clone());
    let return_type = VmType::new_contained_in_register(layout_cache.layout(&return_type_ref));

    FrameAndVariableInfo {
        frame_memory: FrameMemoryInfo {
            infos: frame_memory_infos,
            total_frame_size: frame_size,
            variable_frame_size: frame_size,
            frame_size_for_variables_except_temp: variable_space,
            variable_registers: variable_registers_for_debug_info,
        },
        local_frame_allocator,
        return_type,
        parameter_and_variable_offsets: all_variable_unique_to_register,
    }
}
