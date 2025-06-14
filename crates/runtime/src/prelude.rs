/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub use crate::{
    CodeGenAndVmResult, CompileAndCodeGenOptions, CompileAndVmResult, CompileCodeGenVmResult,
    RunConstantsOptions, RunOptions, compile_and_code_gen, compile_codegen_and_create_vm,
    run_first_time, run_function, run_function_with_debug,
};

pub use swamp_code_gen_program::CodeGenOptions;
pub use swamp_compile::CompileOptions;
