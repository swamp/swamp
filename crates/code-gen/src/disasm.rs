/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMapWrapper;
use source_map_node::FileId;
use std::fmt;
use std::fmt::Write;
use swamp_vm_debug_info::DebugInfo;
use swamp_vm_disasm::disasm_instructions_color;
use swamp_vm_types::types::{VariableRegister, VmType, show_frame_memory, write_basic_type};
use swamp_vm_types::{BinaryInstruction, FrameMemoryAddress, InstructionPositionOffset};

#[must_use]
pub const fn is_valid_file_id(file_id: FileId) -> bool {
    file_id != 0 && file_id != 0xffff
}
pub fn show_parameters_and_variables(
    return_type: &VmType,
    variables: &[VariableRegister],
    f: &mut dyn Write,
) -> Result<(), fmt::Error> {
    if !return_type.is_scalar() {
        writeln!(f, "{}: {}", tinter::blue("r0"), &return_type,)?;
        write_basic_type(&return_type.basic_type, FrameMemoryAddress(0), f, 0)?;
        writeln!(f)?;
    }

    for variable_register in variables {
        writeln!(
            f,
            "var{}: ({}): {} {}",
            tinter::yellow(format!("{}", variable_register.unique_id_in_function)),
            tinter::magenta(variable_register),
            variable_register.register.ty,
            variable_register.register.comment
        )?;
    }

    Ok(())
}

/// # Panics
///
#[must_use]
pub fn disasm_function(
    return_type: &VmType,
    instructions: &[BinaryInstruction],
    ip_offset: &InstructionPositionOffset,
    debug_info: &DebugInfo,
    source_map_wrapper: &SourceMapWrapper,
) -> String {
    let mut header_output = String::new();

    let info = debug_info.fetch(ip_offset.0 as usize).unwrap();

    show_frame_memory(&info.function_debug_info.frame_memory, &mut header_output).unwrap();

    show_parameters_and_variables(
        return_type,
        &info.function_debug_info.frame_memory.variable_registers,
        &mut header_output,
    )
    .expect("should work");

    format!(
        "{}\n{}",
        header_output,
        disasm_instructions_color(instructions, ip_offset, debug_info, source_map_wrapper,)
    )
}

pub fn disasm_whole_program(
    debug_info: &DebugInfo,
    source_map_wrapper: &SourceMapWrapper,
    instructions: &[BinaryInstruction],
) {
    let mut current_ip: u32 = 0;

    while current_ip < (instructions.len() - 1) as u32 {
        if let Some(debug_info_for_pc) = debug_info.fetch(current_ip as usize) {
            // log to stdout since this is a feature "asked" by the user
            println!(
                "{} ==========================================================================",
                debug_info_for_pc.function_debug_info.name
            );
            let end_ip = current_ip + debug_info_for_pc.function_debug_info.ip_range.count.0;
            let instructions_slice = &instructions[current_ip as usize..end_ip as usize];

            let output_string = disasm_function(
                &debug_info_for_pc.function_debug_info.return_type,
                instructions_slice,
                &InstructionPositionOffset(current_ip),
                debug_info,
                source_map_wrapper,
            );
            println!("{output_string}"); // log to stdout since this is a feature "asked" by the user
            current_ip = end_ip;
        } else {
            panic!("instruction pointer that is not covered")
        }
    }
}
