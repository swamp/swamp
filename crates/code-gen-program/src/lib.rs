/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMapWrapper;
use swamp_code_gen::code_bld::CodeBuilderOptions;
use swamp_code_gen::disasm::{disasm_function, disasm_whole_program};
use swamp_code_gen::top_state::TopLevelGenState;
use swamp_compile::Program;
use swamp_semantic::Function;
use swamp_vm_types::FrameMemoryAddress;
use swamp_vm_types::InstructionPositionOffset;
use swamp_vm_types::types::write_basic_type;
use time_dilation::ScopedTimer;

pub struct CodeGenOptions {
    pub show_disasm: bool,
    pub disasm_filter: Option<String>,
    pub show_debug: bool,
    pub show_types: bool,
    pub ignore_host_call: bool,
}

/// Function name matching logic similar to test pattern matching
/// Supports patterns like:
/// - "`exact_name`" - exact match
/// - "`prefix::`" - prefix match
/// - "*suffix" or "prefix*" - wildcard match
#[must_use]
pub fn matches_pattern(function_name: &str, pattern: &str) -> bool {
    if pattern.ends_with("::") {
        function_name.starts_with(pattern)
    } else if pattern.contains('*') {
        let parts: Vec<&str> = pattern.split('*').collect();
        if parts.len() > 2 {
            return false;
        }

        let prefix = parts[0];
        let suffix = if parts.len() == 2 { parts[1] } else { "" };

        if !function_name.starts_with(prefix) {
            return false;
        }

        let remaining_name = &function_name[prefix.len()..];
        remaining_name.ends_with(suffix)
    } else {
        function_name == pattern
    }
}

/// Check if a function name matches any of the patterns in the filter string
#[must_use]
pub fn function_name_matches_filter(function_name: &str, filter_string: &str) -> bool {
    if filter_string.trim().is_empty() {
        return true;
    }

    let patterns: Vec<&str> = filter_string.split(',').collect();

    for pattern in patterns {
        let trimmed_pattern = pattern.trim();
        let simplified_function_name = function_name
            .strip_prefix("crate::")
            .map_or(function_name, |stripped| stripped);

        if matches_pattern(simplified_function_name, trimmed_pattern) {
            return true;
        }
    }

    false
}

/// # Errors
///
#[must_use]
pub fn code_gen_program(
    program: &Program,
    source_map_lookup: &SourceMapWrapper,
    options: &CodeGenOptions,
) -> TopLevelGenState {
    let _compile_all_modules_timer = ScopedTimer::new("code generate whole program");
    let mut code_gen = TopLevelGenState::new(CodeBuilderOptions {
        should_show_debug: options.show_debug,
    });

    code_gen.reserve_space_for_constants(&program.state.constants_in_dependency_order);

    /*
    // TODO: Should support main function in the future
    if let Some(found_main_expression) = &main_module.main_expression {
        let halt_function = GenOptions {
            is_halt_function: true,
        };
        code_gen.emit_main_function(found_main_expression, &halt_function, source_map_lookup)?;
    }

     */

    for (_path, module) in program.modules.modules() {
        for internal_function_def in &module.symbol_table.internal_functions() {
            code_gen.emit_function_def(
                internal_function_def,
                source_map_lookup,
                options.ignore_host_call,
            );
        }
    }

    for (_associated_on_type, impl_functions) in &program.state.associated_impls.functions {
        for (_name, func) in &impl_functions.functions {
            match &**func {
                Function::Internal(int_fn) => {
                    code_gen.emit_function_def(int_fn, source_map_lookup, options.ignore_host_call);
                }

                Function::External(_ext_fn) => {
                    //
                }

                Function::Intrinsic(_) => {
                    //
                }
            }
        }
    }

    code_gen.emit_constants_expression_functions_in_order(
        &program.state.constants_in_dependency_order,
        source_map_lookup,
    );

    code_gen.finalize();

    if options.show_types {
        for (id, basic_type) in &code_gen.codegen_state.layout_cache.id_to_layout {
            let mut str = String::new();
            write_basic_type(basic_type, FrameMemoryAddress(0), &mut str, 0).expect("output error");
            eprintln!("{id}>{str}");
        }
    }

    if options.show_disasm {
        if let Some(filter) = &options.disasm_filter {
            // Filter specific functions
            let mut current_ip: u32 = 0;
            let instructions = code_gen.instructions();

            while current_ip < (instructions.len() - 1) as u32 {
                if let Some(debug_info_for_pc) = code_gen.debug_info().fetch(current_ip as usize) {
                    let function_name = &debug_info_for_pc.function_debug_info.name;
                    //                    eprintln!("COMPARE {function_name} vs {filter}");

                    if function_name_matches_filter(function_name, filter) {
                        // log to stdout since this is a feature "asked" by the user
                        println!(
                            "{function_name} ==========================================================================",
                        );
                        let end_ip =
                            current_ip + debug_info_for_pc.function_debug_info.ip_range.count.0;
                        let instructions_slice =
                            &instructions[current_ip as usize..end_ip as usize];

                        let output_string = disasm_function(
                            &debug_info_for_pc.function_debug_info.return_type,
                            instructions_slice,
                            &InstructionPositionOffset(current_ip),
                            code_gen.debug_info(),
                            source_map_lookup,
                        );
                        println!("{output_string}"); // log to stdout since this is a feature "asked" by the user
                    }

                    current_ip += debug_info_for_pc.function_debug_info.ip_range.count.0;
                } else {
                    panic!("instruction pointer that is not covered")
                }
            }
        } else {
            // Show all functions (existing behavior)
            disasm_whole_program(
                code_gen.debug_info(),
                source_map_lookup,
                code_gen.instructions(),
            );
        }
    }

    code_gen
}
