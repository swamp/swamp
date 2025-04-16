/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMapWrapper;
use swamp_code_gen::{Error, GenOptions, TopLevelGenState, disasm_whole_program};
use swamp_compile::Program;
use swamp_semantic::Function;
use tracing::{info, warn};

/// # Errors
///
pub fn code_gen_program(
    program: &Program,
    source_map_lookup: &SourceMapWrapper,
) -> Result<TopLevelGenState, Error> {
    let mut code_gen = TopLevelGenState::new();

    code_gen.reserve_space_for_constants(&program.state.constants_in_dependency_order)?;

    /*
    if let Some(found_main_expression) = &main_module.main_expression {
        let halt_function = GenOptions {
            is_halt_function: true,
        };
        code_gen.gen_main_function(found_main_expression, &halt_function, source_map_lookup)?;
    }

     */

    let normal_function = GenOptions {
        is_halt_function: false,
    };

    for (path, module) in program.modules.modules() {
        for internal_function_def in &module.symbol_table.internal_functions() {
            code_gen.gen_function_def(
                internal_function_def,
                &normal_function,
                source_map_lookup,
            )?;
        }
    }

    for (associated_on_type, impl_functions) in
        &program.state.instantiator.associated_impls.functions
    {
        if !associated_on_type.is_concrete() {
            for (name, func) in &impl_functions.functions {
                if let Function::Internal(internal) = &**func {
                    warn!(
                        name,
                        id = internal.program_unique_id,
                        "skipping generation of this function"
                    );
                }
            }
            continue;
        }

        for (_name, func) in &impl_functions.functions {
            if func.name().clone().starts_with("instantiated ") {
                continue;
            }
            match &**func {
                Function::Internal(int_fn) => {
                    code_gen.gen_function_def(int_fn, &normal_function, source_map_lookup)?;
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

    code_gen.gen_constants_expression_functions_in_order(
        &program.state.constants_in_dependency_order,
        source_map_lookup,
    )?;

    code_gen.finalize();
    disasm_whole_program(
        code_gen.function_ips(),
        code_gen.function_debug_infos(),
        source_map_lookup,
        code_gen.instructions(),
        code_gen.meta(),
    );

    Ok(code_gen)
}
