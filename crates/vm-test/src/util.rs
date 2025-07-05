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

fn emit_internal(code: &str) -> (TopLevelGenState, Program) {
    let (program, _main_module, source_map) = compile_string(code).unwrap();

    let source_map_wrapper = SourceMapWrapper {
        source_map: &source_map,
        current_dir: Default::default(),
    };
    let code_gen = code_gen_program(
        &program,
        &source_map_wrapper,
        &CodeGenOptions {
            show_disasm: false,
            disasm_filter: None,
            show_debug: false,
            show_types: false,
            ignore_host_call: false,
        },
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

const fn exec(code: &str, expected_hex: &str) {}

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
}

/// # Panics
///

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
}

const fn exec_vars(code: &str, expected_hex: &str) {}

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

pub const fn exec_show_constants(code: &str, expected_hex: &str, expected_constants: &str) {}
