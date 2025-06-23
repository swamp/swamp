/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMapWrapper;
use swamp_code_gen::code_bld::CodeBuilderOptions;
use swamp_code_gen::disasm::disasm_whole_program;
use swamp_code_gen::top_state::TopLevelGenState;
use swamp_compile::Program;
use swamp_semantic::Function;
use swamp_vm_types::FrameMemoryAddress;
use swamp_vm_types::types::write_basic_type;
use time_dilation::ScopedTimer;

pub struct CodeGenOptions {
    pub show_disasm: bool,
    pub show_debug: bool,
    pub show_types: bool,
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
    if let Some(found_main_expression) = &main_module.main_expression {
        let halt_function = GenOptions {
            is_halt_function: true,
        };
        code_gen.emit_main_function(found_main_expression, &halt_function, source_map_lookup)?;
    }

     */

    for (_path, module) in program.modules.modules() {
        for internal_function_def in &module.symbol_table.internal_functions() {
            code_gen.emit_function_def(internal_function_def, source_map_lookup);
        }
    }

    for (_associated_on_type, impl_functions) in &program.state.associated_impls.functions {
        for (_name, func) in &impl_functions.functions {
            match &**func {
                Function::Internal(int_fn) => {
                    code_gen.emit_function_def(int_fn, source_map_lookup);
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
        disasm_whole_program(
            code_gen.debug_info(),
            source_map_lookup,
            code_gen.instructions(),
        );
    }

    code_gen
}
