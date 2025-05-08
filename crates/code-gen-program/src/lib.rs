/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMapWrapper;
use swamp_code_gen::disasm::disasm_whole_program;
use swamp_code_gen::top_state::TopLevelGenState;
use swamp_compile::Program;
use swamp_semantic::Function;

pub struct CodeGenOptions {
    pub show_disasm: bool,
}

/// # Errors
///
pub fn code_gen_program(
    program: &Program,
    source_map_lookup: &SourceMapWrapper,
    options: &CodeGenOptions,
) -> TopLevelGenState {
    let mut code_gen = TopLevelGenState::new();

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

    for (associated_on_type, impl_functions) in
        &program.state.instantiator.associated_impls.functions
    {
        if !associated_on_type.is_concrete() {
            for (_name, func) in &impl_functions.functions {
                if let Function::Internal(_internal) = &**func {
                    /*
                    warn!(
                        name = internal.assigned_name,
                        path = ?internal.defined_in_module_path,
                        id = internal.program_unique_id,
                        "skipping generation of this function"
                    );

                     */
                }
            }
            continue;
        }

        for (_name, func) in &impl_functions.functions {
            match &**func {
                Function::Internal(int_fn) => {
                    if int_fn
                        .assigned_name
                        .starts_with("instantiated new_from_slice")
                    {
                        continue;
                    }

                    if int_fn
                        .assigned_name
                        .starts_with("instantiated new_from_slice_pair")
                    {
                        continue;
                    }

                    if !code_gen
                        .codegen_state
                        .function_infos
                        .contains_key(&int_fn.program_unique_id)
                        && int_fn.all_parameters_and_variables_are_concrete()
                    {
                        code_gen.emit_function_def(int_fn, source_map_lookup);
                    } else {
                        // info!(int_fn.assigned_name, ?int_fn.defined_in_module_path, "skipping due to strange parameters");
                    }
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

    if options.show_disasm {
        disasm_whole_program(
            //code_gen.function_ips(),
            code_gen.function_debug_infos(),
            source_map_lookup,
            code_gen.instructions(),
            code_gen.meta(),
        );
    }

    code_gen
}
