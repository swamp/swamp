use crate::FrameAndVariableInfo;
use crate::alloc::ScopeAllocator;
use crate::reg_pool::RegisterPool;
use seq_map::SeqMap;
use source_map_node::Node;
use std::fmt::Write;
use swamp_semantic::{VariableRef, VariableType};
use swamp_types::TypeRef;
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::{
    FrameAddressInfo, FrameMemoryInfo, VariableInfo, VariableInfoKind, VariableRegister, VmType,
};
use swamp_vm_types::{FrameMemoryAddress, FrameMemoryRegion, MemorySize};

/// # Errors
///
#[allow(clippy::too_many_lines)]
#[must_use]
pub fn layout_variables(
    layout_cache: &mut LayoutCache,
    _node: &Node,
    parameters: &Vec<VariableRef>,
    variables: &Vec<VariableRef>,
    exp_return_type: &TypeRef,
) -> FrameAndVariableInfo {
    let mut local_frame_allocator = ScopeAllocator::new(FrameMemoryRegion::new(
        FrameMemoryAddress(0),
        MemorySize(3 * 1024 * 1024 * 1024),
    ));

    //    let return_placed_type_pointer = layout_type(exp_return_type).create_mutable_pointer();
    //let return_placed_type = allocator.allocate_type(return_placed_type_pointer); //reserve(return_placed_type_pointer, &mut allocator);

    let mut enter_comment = "variables:\n".to_string();
    let mut frame_memory_infos = Vec::new();

    let mut parameter_and_variable_registers = SeqMap::new();
    let mut frame_register_allocator = RegisterPool::new(1, 127);

    let mut parameter_registers = Vec::new();
    for var_ref in parameters {
        let parameter_basic_type = layout_cache.layout(&var_ref.resolved_type);
        swamp_vm_layout::check_type_size(
            &parameter_basic_type,
            &format!("parameter '{}'", var_ref.assigned_name),
        );

        let register = frame_register_allocator.alloc_register(
            VmType::new_contained_in_register(parameter_basic_type),
            &format!("param {}", var_ref.assigned_name),
        );

        parameter_registers.push(VariableRegister {
            unique_id_in_function: var_ref.unique_id_within_function,
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
    let mut log_this_function = false;
    
    for var_ref in variables {
        let basic_type = layout_cache.layout(&var_ref.resolved_type);

        //info!(var_ref.assigned_name, ?frame_register_allocator, "allocating local variable");
        let register = if basic_type.is_aggregate() {
            // TODO: Should have a check if the variable needs the storage (if it is in an assignment in a copy)
            swamp_vm_layout::check_type_size(
                &basic_type,
                &format!("variable '{}'", var_ref.assigned_name),
            );

            let var_frame_placed_type = local_frame_allocator.allocate_type(&basic_type);
            if log_this_function {
                eprintln!("parameter {var_ref:?} {var_frame_placed_type:?}");
            }
            if var_ref.assigned_name == "id_gen" {
                eprintln!("you got memory {var_frame_placed_type:?}");
                log_this_function = true;
                
            }
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
            unique_id_in_function: var_ref.unique_id_within_function,
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

    let frame_size = local_frame_allocator.addr().as_size();

    let highest_register_used = frame_register_allocator.current_index;

    let return_type_ref = TypeRef::from(exp_return_type.clone());
    let return_type = VmType::new_contained_in_register(layout_cache.layout(&return_type_ref));

    FrameAndVariableInfo {
        frame_memory: FrameMemoryInfo {
            infos: frame_memory_infos,
            total_frame_size: frame_size,
            variable_frame_size: frame_size,
            frame_size_for_variables_except_temp: variable_space,
            variable_registers,
        },
        local_frame_allocator,
        return_type,
        parameters: parameter_registers.clone(),
        parameter_and_variable_offsets: parameter_and_variable_registers,
        frame_registers: frame_register_allocator,
        highest_register_used,
    }
}
