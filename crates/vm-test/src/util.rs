/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use source_map_cache::SourceMapWrapper;
use swamp_code_gen::top_state::TopLevelGenState;
use swamp_code_gen_program::{CodeGenOptions, code_gen_program};
use swamp_compile::Program;
use swamp_compile::compile_string;
use swamp_vm::host::HostArgs;
use swamp_vm::{Vm, VmSetup};
use swamp_vm_types::InstructionPosition;

fn emit_internal(code: &str) -> (TopLevelGenState, Program) {
    let (program, _main_module, source_map) = compile_string(code).unwrap();

    let source_map_wrapper = SourceMapWrapper {
        source_map: &source_map,
        current_dir: Default::default(),
    };
    let code_gen = code_gen_program(
        &program,
        &source_map_wrapper,
        &CodeGenOptions { show_disasm: false },
    );

    (code_gen, program)
}

fn emit_internal_debug(code: &str) -> (TopLevelGenState, Program) {
    let (code_gen, program) = emit_internal(code);
    /*
    let disassembler_output = disasm_instructions_color(
        code_gen.instructions(),
        &InstructionPosition(0),
        code_gen.meta(),
        &code_gen.create_function_sections(),
    );

    eprintln!("{disassembler_output}");

     */

    (code_gen, program)
}

fn exec_code_gen_state(code_gen_state: TopLevelGenState) -> Vm {
    let (instructions, constant_functions, functions, constants_memory) =
        code_gen_state.take_instructions_and_constants();

    for (_constant_id, constant_func) in constant_functions {
        let setup = VmSetup {
            stack_memory_size: 1024,
            heap_memory_size: 1024,
            constant_memory: constants_memory.clone(),
            debug_stats_enabled: false,
            debug_opcodes_enabled: false,
        };

        let mut vm = Vm::new(instructions.clone(), setup);

        vm.execute_from_ip(&constant_func.ip_range.start);
    }

    let setup = VmSetup {
        stack_memory_size: 1024,
        heap_memory_size: 1024,
        constant_memory: constants_memory,
        debug_stats_enabled: false,
        debug_opcodes_enabled: false,
    };
    let mut vm = Vm::new(instructions, setup);

    vm.execute_from_ip(&InstructionPosition(0));

    vm
}

#[must_use]
pub fn exec_internal(code: &str) -> Vm {
    let (code_gen, _program) = emit_internal_debug(code);

    exec_code_gen_state(code_gen)
}

fn exec_internal_debug(code: &str) -> Vm {
    exec_internal(code)
}

fn trim_lines(text: &str) -> String {
    text.lines()
        .map(|line| {
            // Ignore comments that starts with ;
            line.split(';').next().unwrap_or("").trim()
        })
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

fn compare_line_outputs(encountered: &str, expected: &str) {
    let encountered_trimmed = trim_lines(encountered);
    let expected_trimmed = trim_lines(expected);

    eprintln!("{encountered}");
    assert_eq!(encountered_trimmed, expected_trimmed);
}

fn compare_hex_outputs(memory: &[u8], expected_hex: &str) {
    let encountered_hexed = hexify::format_hex(memory);
    let expected_hex_trimmed = expected_hex.trim();

    compare_line_outputs(&encountered_hexed, expected_hex_trimmed);
}

fn exec(code: &str, expected_hex: &str) {
    let vm = exec_internal_debug(code);

    compare_hex_outputs(&vm.stack_memory()[..16], expected_hex);
}

pub fn exec_with_assembly(code: &str, expected_assembly: &str, expected_hex: &str) {
    let (generator, _program) = emit_internal_debug(code);

    /*
    let disassembler_output = disasm_instructions_no_color(
        generator.instructions(),
        generator.meta(),
        &generator.create_function_sections(),
        false,
    );
    compare_line_outputs(&disassembler_output, expected_assembly);

     */

    let vm = exec_code_gen_state(generator);

    compare_hex_outputs(&vm.frame_memory()[..16], expected_hex);
}

/// # Panics
///
pub fn exec_with_host_function<F>(
    code: &str,
    expected_assembly: &str,
    expected_hex: &str,
    id: &str,
    callback: F,
) where
    F: 'static + FnMut(HostArgs),
{
    let vm = exec_with_host_function_internal(code, expected_assembly, id, callback);
    compare_hex_outputs(&vm.frame_memory()[..16], expected_hex);
}

/// # Panics
///
pub fn exec_with_host_function_internal<F>(
    code: &str,
    expected_assembly: &str,
    id: &str,
    callback: F,
) -> Vm
where
    F: 'static + FnMut(HostArgs),
{
    let (generator, program) = emit_internal_debug(code);

    /*
    let disassembler_output = disasm_instructions_no_color(
        generator.instructions(),
        generator.meta(),
        &generator.create_function_sections(),
        false,
    );
    compare_line_outputs(&disassembler_output, expected_assembly);

     */

    let module = program
        .modules
        .get(&["crate".to_string(), "test".to_string()])
        .unwrap();

    let external_id = module
        .symbol_table
        .get_external_function_declaration(id)
        .unwrap();

    let (instructions, _constants, _functions, constant_memory) =
        generator.take_instructions_and_constants();
    let setup = VmSetup {
        stack_memory_size: 1024,
        heap_memory_size: 1024,
        constant_memory,
        debug_stats_enabled: false,
        debug_opcodes_enabled: false,
    };
    let mut vm = Vm::new(instructions, setup);

    vm.add_host_function(external_id.id as u16, callback);

    vm.execute_from_ip(&InstructionPosition(0));

    vm
}

/// # Panics
///
pub fn exec_with_host_function_show_heap<F>(
    code: &str,
    expected_assembly: &str,
    start: usize,
    count: usize,
    expected_hex: &str,
    id: &str,
    callback: F,
) where
    F: 'static + FnMut(HostArgs),
{
    let vm = exec_with_host_function_internal(code, expected_assembly, id, callback);
    compare_hex_outputs(&vm.heap_memory()[start..start + count], expected_hex);
}

fn exec_vars(code: &str, expected_hex: &str) {
    let vm = exec_internal_debug(code);

    compare_hex_outputs(&vm.frame_memory()[..16], expected_hex);
}

fn emit_code(code: &str, expected_output: &str) {
    let (generator, _program) = emit_internal_debug(code);

    /*
    let disassembler_output = disasm_instructions_no_color(
        generator.instructions(),
        generator.meta(),
        &generator.create_function_sections(),
        false,
    );

    compare_line_outputs(&disassembler_output, expected_output);

     */
}

pub fn exec_show_constants(code: &str, expected_hex: &str, expected_constants: &str) {
    let vm = exec_internal_debug(code);

    compare_hex_outputs(&vm.stack_memory()[..16], expected_hex);
    //compare_hex_outputs(&vm.constants()[..16], expected_constants);
}
