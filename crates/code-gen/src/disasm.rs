use seq_map::SeqMap;
use source_map_cache::{FileLineInfo, SourceMapLookup, SourceMapWrapper};
use source_map_node::FileId;
use std::fmt;
use std::fmt::Write;
use swamp_vm_disasm::{SourceFileLineInfo, disasm_instructions_color};
use swamp_vm_types::types::{FrameMemoryInfo, FunctionInfo, VariableRegister, show_frame_memory};
use swamp_vm_types::{BinaryInstruction, InstructionPosition, InstructionPositionOffset, Meta};

fn different_file_info(span_a: &FileLineInfo, span_b: &FileLineInfo) -> bool {
    span_a.line != span_b.line
}

pub fn is_valid_file_id(file_id: FileId) -> bool {
    file_id != 0 && file_id != 0xffff
}
pub fn show_parameters_and_variables(
    parameters: &[VariableRegister],
    variables: &[VariableRegister],
    f: &mut dyn Write,
) -> Result<(), fmt::Error> {
    for reg in parameters {
        writeln!(
            f,
            "{}: {}: {} ",
            tinter::yellow(format!("r{}", reg.register.index)),
            tinter::bright_cyan(&reg.variable.name),
            &reg.register.ty,
        )?;
    }

    for reg in variables {
        writeln!(
            f,
            "{}: {}: {} {}",
            tinter::yellow(format!("r{}", reg.register.index)),
            tinter::blue(&reg.variable.name),
            reg.register.ty,
            reg.register.comment
        )?;
    }

    Ok(())
}

pub fn disasm_function(
    frame_relative_infos: &FrameMemoryInfo,
    parameters: &[VariableRegister],
    instructions: &[BinaryInstruction],
    meta: &[Meta],
    ip_offset: InstructionPositionOffset,
    source_map_wrapper: &SourceMapWrapper,
) -> String {
    let mut header_output = String::new();

    show_frame_memory(frame_relative_infos, &mut header_output).unwrap();

    show_parameters_and_variables(
        parameters,
        &frame_relative_infos.variable_registers,
        &mut header_output,
    )
    .expect("should work");

    let mut ip_infos = SeqMap::new();

    let mut previous_node: Option<FileLineInfo> = None;

    for (offset, _inst) in instructions.iter().enumerate() {
        let absolute_ip = ip_offset.0 + offset as u32;
        let meta = &meta[offset];
        let file_line_info = if is_valid_file_id(meta.node.span.file_id) {
            Some(source_map_wrapper.get_line(&meta.node.span))
        } else {
            None
        };

        if let Some(line_info) = file_line_info {
            let is_different_line = previous_node
                .as_ref()
                .is_none_or(|previous| different_file_info(&line_info, previous));

            // TODO: Add clone to FileLineInfo
            previous_node = Some(FileLineInfo {
                row: line_info.row,
                col: line_info.col,
                line: line_info.line.clone(),
                relative_file_name: line_info.relative_file_name.clone(),
            });

            if is_different_line {
                assert_ne!(
                    line_info.row, 0,
                    "file_info: {}",
                    line_info.relative_file_name
                );
                assert_ne!(
                    line_info.row, 0,
                    "file_info: {}",
                    line_info.relative_file_name
                );
                let mapped = SourceFileLineInfo {
                    row: line_info.row,
                    file_id: meta.node.span.file_id as usize,
                };
                ip_infos
                    .insert(InstructionPosition(absolute_ip), mapped)
                    .unwrap();
            }
        }
    }

    format!(
        "{}\n{}",
        header_output,
        disasm_instructions_color(
            instructions,
            &ip_offset,
            meta,
            frame_relative_infos,
            &ip_infos,
            source_map_wrapper,
        )
    )
}

pub fn disasm_whole_program(
    function_debug_infos: &SeqMap<InstructionPosition, FunctionInfo>,
    source_map_wrapper: &SourceMapWrapper,
    instructions: &[BinaryInstruction],
    meta: &[Meta],
) {
    let mut current_ip: u32 = 0;

    let instruction_count = instructions.len();
    while current_ip < (instructions.len() - 1) as u32 {
        if let Some(function_debug_info) =
            function_debug_infos.get(&InstructionPosition(current_ip))
        {
            eprintln!(
                "{} ==========================================================================",
                function_debug_info.name
            );
            let end_ip = current_ip + function_debug_info.ip_range.count.0;
            let instructions_slice = &instructions[current_ip as usize..end_ip as usize];
            let meta_slice = &meta[current_ip as usize..end_ip as usize];

            let output_string = disasm_function(
                &function_debug_info.frame_memory,
                &function_debug_info.parameters,
                instructions_slice,
                meta_slice,
                InstructionPositionOffset(current_ip),
                source_map_wrapper,
            );
            eprintln!("{output_string}");
            current_ip = end_ip;
        } else {
            panic!("instruction pointer that is not covered")
        }
    }
}
