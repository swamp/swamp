/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use regex::Regex;
use seq_map::SeqMap;
use source_map_cache::SourceMap;
use source_map_cache::SourceMapWrapper;
use std::env::current_dir;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;
use swamp_analyzer::Analyzer;
pub use swamp_analyzer::prelude::Program;
use swamp_dep_loader::{
    DependencyParser, ParsedAstModule, parse_local_modules_and_get_order, parse_single_module,
    parse_single_module_from_text, swamp_registry_path,
};
use swamp_error_report::{ScriptResolveError, prelude::show_script_resolve_error};
use swamp_modules::modules::{ModuleRef, Modules, pretty_print};
use swamp_modules::prelude::Module;
use swamp_modules::symtbl::{SymbolTable, SymbolTableRef};
use swamp_pretty_print::{ImplsDisplay, SourceMapDisplay, SymbolTableDisplay};
use swamp_program_analyzer::analyze_modules_in_order;
use swamp_semantic::{AssociatedImpls, ProgramState, formal_module_name, pretty_module_name};
use time_dilation::ScopedTimer;
use tiny_ver::TinyVersion;
use tracing::{info, trace};

pub const COMPILER_VERSION: &str = "0.0.0";
pub const CORE_VERSION: &str = "core-0.0.0";

/// # Errors
///
pub fn analyze_ast_module_skip_expression(
    analyzer: &mut Analyzer,
    parsed_ast_module: &ParsedAstModule,
) {
    for definition in &parsed_ast_module.ast_module.definitions {
        analyzer.analyze_definition(definition);
    }
}

#[must_use]
pub fn analyze_single_module(
    state: &mut ProgramState,
    default_symbol_table: SymbolTable,
    modules: &Modules,
    core_symbol_table: SymbolTableRef,
    parsed_ast_module: &ParsedAstModule,
    source_map: &SourceMap,
    versioned_module_path: &[String],
) -> SymbolTable {
    let mut analyzer = Analyzer::new(
        state,
        modules,
        core_symbol_table,
        source_map,
        versioned_module_path,
        parsed_ast_module.file_id,
    );

    analyzer.shared.lookup_table = default_symbol_table;

    //    trace!(lookup_table=?analyzer.shared.lookup_table, "analyzer lookup_table");

    analyze_ast_module_skip_expression(&mut analyzer, parsed_ast_module);

    analyzer.shared.definition_table
}

pub fn create_source_map(registry_path: &Path, local_path: &Path) -> io::Result<SourceMap> {
    trace!(?registry_path, ?local_path, "mounting source map");

    let mut mounts = SeqMap::new();
    mounts
        .insert("crate".to_string(), local_path.to_path_buf())
        .unwrap();

    mounts
        .insert("registry".to_string(), registry_path.to_path_buf())
        .unwrap();

    SourceMap::new(&mounts)
}

pub fn create_registry_source_map(registry_path: &Path) -> io::Result<SourceMap> {
    trace!(?registry_path, "mounting registry path source map");

    let mut mounts = SeqMap::new();
    mounts
        .insert("registry".to_string(), registry_path.to_path_buf())
        .unwrap();

    SourceMap::new(&mounts)
}

#[derive(Debug)]
pub struct BootstrapResult {
    pub program: Program,
    pub core_module_path: Vec<String>,
}

fn std_text() -> String {
    "external 1 fn print(output: String)".to_string()
}

#[allow(clippy::too_many_lines)]
fn core_text() -> String {
    let text = r#"
  /// DO NOT EDIT!

/// # Swamp Core Module
/// Welcome to Swamp! This module provides basic implementations for common types.
/// **Note on Intrinsics:**
/// Some functions in this module, such as `int_rnd` and `float_rnd`, are not normal functions.
/// They are compiler intrinsics that are replaced with optimized code during compilation.

type SparseId = Int

impl Bool {
  fn default() -> Bool {
    false
  }

  fn to_string(self) -> String {
      bool_to_string(self)
  }
}

impl Float {
    fn default() -> Float {
        0.0
    }

    fn to_string(self) -> String {
        float_to_string(self)
    }

    /// Rounds the Float value down.
    ///
    /// Uses intrinsic function `float_floor` to return the largest integer value not greater than the Float.
    ///
    /// # Returns
    /// An `Int` representing the floor value.
    fn floor(self) -> Int {
        float_floor(self)
    }

    /// Rounds the Float value to the nearest integer.
    ///
    /// Uses intrinsic function `float_round` to round the Float.
    ///
    /// # Returns
    /// An `Int` representing the rounded value.
    fn round(self) -> Int {
        float_round(self)
    }

    /// Computes the square root.
    ///
    /// Uses intrinsic function `float_sqrt` to calculate the square root of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the square root.
    fn sqrt(self) -> Float {
        float_sqrt(self)
    }

    /// Determines the sign of the Float.
    ///
    /// Uses intrinsic function `float_sign` to indicate whether the value is positive or negative.
    ///
    /// # Returns
    /// A `Float` representing the sign.
    fn sign(self) -> Float {
        float_sign(self)
    }

    /// Computes the absolute value.
    ///
    /// Uses intrinsic function `float_abs` to return the absolute (non-negative) value of the Float.
    ///
    /// # Returns
    /// A `Float` with the absolute value.
    fn abs(self) -> Float {
        float_abs(self)
    }

    /// Returns a pseudo-random number between 0.0 and 1.0.
    ///
    /// Uses intrinsic function `float_rnd` to generate the number.
    /// **Note:** This function is by design 100% deterministic and not designed for cryptographic or security-sensitive use.
    ///
    /// # Returns
    /// A `Float` between 0.0 and 1.0.
    fn rnd(self) -> Float {
        float_rnd(self)
    }

    /// Calculates the cosine.
    ///
    /// Uses intrinsic function `float_cos` to compute the cosine of the `Float` value.
    ///
    /// # Returns
    /// A `Float` representing the cosine.
    fn cos(self) -> Float {
        float_cos(self)
    }

    /// Calculates the sine.
    ///
    /// Uses intrinsic function `float_sin` to compute the sine of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the sine.
    fn sin(self) -> Float {
        float_sin(self)
    }

    /// Calculates the arc cosine.
    ///
    /// Uses intrinsic function `float_acos` to compute the arc cosine (inverse cosine) of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the arc cosine.
    fn acos(self) -> Float {
        float_acos(self)
    }

    /// Calculates the arc sine.
    ///
    /// Uses intrinsic function `float_asin` to compute the arc sine (inverse sine) of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the arc sine.
    fn asin(self) -> Float {
        float_asin(self)
    }

    /// Computes the angle from the x-axis.
    ///
    /// # Parameters
    /// - `x`: The second coordinate as a `Float`.
    ///
    /// Uses intrinsic function `float_atan2` to calculate the angle (in radians) from the x-axis to the point `(self, x)`.
    ///
    /// # Returns
    /// A `Float` representing the computed angle.
    fn atan2(self, x: Float) -> Float {
        float_atan2(self, x)
    }

    /// Returns the minimum of two Floats.
    ///
    /// # Parameters
    /// - `x`: Another `Float` to compare.
    ///
    /// Uses intrinsic function `float_min` to return the smaller of the two values.
    ///
    /// # Returns
    /// A `Float` representing the minimum value.
    fn min(self, x: Float) -> Float {
        float_min(self, x)
    }

    /// Returns the maximum of two Floats.
    ///
    /// # Parameters
    /// - `x`: Another `Float` to compare.
    ///
    /// Uses intrinsic function `float_max` to return the larger of the two values.
    ///
    /// # Returns
    /// A `Float` representing the maximum value.
    fn max(self, x: Float) -> Float {
        float_max(self, x)
    }

    /// Clamps the Float within a range.
    ///
    /// # Parameters
    /// - `min`: The minimum allowed value.
    /// - `max`: The maximum allowed value.
    ///
    /// Uses intrinsic function `float_clamp` to restrict the Float to the given range.
    ///
    /// # Returns
    /// A `Float` that is clamped between `min` and `max`.
    fn clamp(self, min: Float, max: Float) -> Float {
        float_clamp(self, min, max)
    }

}

impl Int {
    fn default() -> Int {
        0
    }

    fn to_string(self) -> String {
        int_to_string(self)
    }

    /// Computes the absolute value.
    ///
    /// Uses intrinsic function `int_abs` to return the non-negative value of the Int.
    ///
    /// # Returns
    /// An `Int` with the absolute value.
    fn abs(self) -> Int {
        int_abs(self)
    }

    /// Returns a pseudo-random number between 0 and 32767.
    ///
    /// Uses intrinsic function `int_rnd` to generate the number.
    ///
    /// **Note:** This function is by design 100% deterministic and not designed for cryptographic or security-sensitive use.
    ///
    /// # Returns
    /// An `Int` between 0 and 32767.
    fn rnd(self) -> Int {
        int_rnd(self)
    }

    /// Returns the maximum of two Int values.
    ///
    /// # Parameters
    /// - `x`: Another `Int` to compare.
    ///
    /// Uses intrinsic function `int_max` to return the larger of the two values.
    ///
    /// # Returns
    /// An `Int` representing the maximum value.
    fn max(self, x: Int) -> Int {
        int_max(self, x)
    }

    /// Returns the minimum of two Int values.
    ///
    /// # Parameters
    /// - `x`: Another `Int` to compare.
    ///
    /// Uses intrinsic function `int_min` to return the smaller of the two values.
    ///
    /// # Returns
    /// An `Int` representing the minimum value.
    fn min(self, x: Int) -> Int {
        int_min(self, x)
    }

    fn clamp(self, min: Int, max: Int) -> Int {
        int_clamp(self, min, max)
    }

    /// Converts an Int to a Float.
    ///
    /// Uses intrinsic function `int_to_float` to convert the integer value to its floating-point representation.
    ///
    /// # Returns
    /// A `Float` representing the integer.
    fn to_float(self) -> Float {
        int_to_float(self)
    }
}

impl String {
    fn default() -> String {
        ""
    }

    fn to_string(self) -> String {
        "\"todo\""
    }

    /// Computes the length of a String.
    ///
    /// # Parameters
    /// - `s`: The `String` whose length is to be determined.
    ///
    /// Uses intrinsic function `string_len` to return the number of characters in the string.
    ///
    /// # Returns
    /// An `Int` representing the length of the string.
    fn len(self) -> Int {
        string_len(self)
    }
}

struct Range {
    start: Int,
    end: Int,
    is_inclusive: Bool,
}

impl Range {
    fn new(start: Int, end: Int, is_inclusive: Bool) -> Range {
        Range {
            start: start,
            end: end,
            is_inclusive: is_inclusive,
        }
    }

    fn iter(self) -> (Int, Int) {
        //range_iter(self)
        (0, 0)
    }
}

/// Immediately terminates program execution with an error message
///
/// # Parameters
/// - `message`: The error message to display
///
/// Uses intrinsic function `runtime_panic` to terminate execution.
fn panic(message: String) {
    runtime_panic(message)
}

/// Asserts that a condition is true, panics with a message if false
///
/// # Parameters
/// - `condition`: The condition to check
/// - `message`: The error message to display if condition is false
fn assert(condition: Bool, message: String) {
    if !condition {
        panic(message)
    }
}


fn halt() {
    runtime_halt()
}

fn step() {
    runtime_step()
}

    "#;

    text.to_string()
}

/// Bootstraps the core and ffi modules and creates a default symbol table
///
/// # Errors
///
/// # Panics
/// In theory it can panic, but should be safe.
pub fn bootstrap_modules(
    source_map: &mut SourceMap,
) -> Result<BootstrapResult, ScriptResolveError> {
    let compiler_version = TinyVersion::from_str(COMPILER_VERSION).unwrap();
    trace!(%compiler_version, "booting up compiler");
    let mut state = ProgramState::new();

    let mut modules = Modules::new();

    let core_module_with_intrinsics =
        swamp_core::create_module(&compiler_version, &mut state.types);

    let core_path = core_module_with_intrinsics.symbol_table.module_path();
    let core_parsed_ast_module =
        parse_single_module_from_text(source_map, &core_path, &core_text())?;

    let half_completed_core_symbol_table = core_module_with_intrinsics.symbol_table.clone();
    let default_symbol_table_for_core_with_intrinsics = half_completed_core_symbol_table.clone();

    let mut analyzed_core_symbol_table = analyze_single_module(
        &mut state,
        default_symbol_table_for_core_with_intrinsics.clone(),
        &modules,
        half_completed_core_symbol_table.clone().into(),
        &core_parsed_ast_module,
        source_map,
        &core_module_with_intrinsics.symbol_table.module_path(),
    );

    // After this the core symbol table is done
    analyzed_core_symbol_table
        .extend_basic_from(&core_module_with_intrinsics.symbol_table)
        .expect("couldn't extend core");
    let mut default_module = swamp_core::create_module_with_name(&[]);
    default_module
        .symbol_table
        .extend_alias_from(&analyzed_core_symbol_table)
        .expect("extend basic alias and functions from core");

    let core_module = Module::new(analyzed_core_symbol_table, vec![], None);
    modules.add(ModuleRef::from(core_module));

    // ---------

    let source_map_display = SourceMapDisplay {
        source_map: &SourceMapWrapper {
            source_map,
            current_dir: PathBuf::default(),
        },
    };

    let symbol_table_display = SymbolTableDisplay {
        symbol_table: &default_module.symbol_table,
        source_map_display: &source_map_display,
    };

    info!(%symbol_table_display, "default module ready");

    //    let std_path = &["std".to_string(), "lib".to_string()];
    let std_path = &["std".to_string()];
    let std_module_with_intrinsics = swamp_core::create_module_with_name(std_path);
    let std_ast_module = parse_single_module_from_text(source_map, std_path, &std_text())?;
    let analyzed_std_symbol_table = analyze_single_module(
        &mut state,
        default_symbol_table_for_core_with_intrinsics.clone(),
        &modules,
        half_completed_core_symbol_table.clone().into(),
        &std_ast_module,
        source_map,
        &std_module_with_intrinsics.symbol_table.module_path(),
    );
    default_module
        .symbol_table
        .extend_basic_from(&analyzed_std_symbol_table)
        .expect("extend basics from core");

    let analyzed_std_module = Module::new(analyzed_std_symbol_table, vec![], None);

    modules.add(ModuleRef::from(analyzed_std_module));

    let bootstrap_program = Program::new(state, modules, default_module.symbol_table);

    let result = BootstrapResult {
        program: bootstrap_program,
        core_module_path: core_path,
    };

    Ok(result)

    /*

    let half_completed_core_symbol_table = core_module_with_intrinsics.symbol_table.clone();
    let default_symbol_table_for_core_with_intrinsics = half_completed_core_symbol_table.clone();

    let mut core_analyzed_definition_table = analyze_single_module(
        &mut state,
        default_symbol_table_for_core_with_intrinsics.clone(),
        &modules,
        half_completed_core_symbol_table.clone().into(),
        &core_ast_module,
        source_map,
        &core_module_with_intrinsics.symbol_table.module_path(),
    )?;

    let mut std_analyzed_definition_table = analyze_single_module(
        &mut state,
        default_symbol_table_for_core_with_intrinsics.clone(),
        &modules,
        half_completed_core_symbol_table.clone().into(),
        &std_ast_module,
        source_map,
        &std_module_with_intrinsics.symbol_table.module_path(),
    )?;

    // Overwrite the default lookup table to the definition table
    core_analyzed_definition_table
        .extend_intrinsic_functions_from(&default_symbol_table_for_core_with_intrinsics)
        .expect("core extend");

    core_analyzed_definition_table
        .extend_basic_from(&default_symbol_table_for_core_with_intrinsics)
        .expect("extend basics from core");

    /*
    let source_map_lookup = SourceMapWrapper {
        source_map,
        current_dir: current_dir().unwrap(),
    };

    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let display_core_analyzed_definition_table = SymbolTableDisplay {
        symbol_table: &core_analyzed_definition_table,
        source_map_display: &pretty_printer,
    };

    //trace!(%display_core_analyzed_definition_table, "core analyzed symbol table");
    */

    core_module_with_intrinsics.symbol_table = core_analyzed_definition_table;

    // core module is done, so add it read only to the modules
    let core_module_ref = Rc::new(core_module_with_intrinsics);
    modules.add(core_module_ref.clone());

    std_module_with_intrinsics.symbol_table = std_analyzed_definition_table;
    let std_module_ref = Rc::new(std_module_with_intrinsics);
    modules.add(std_module_ref.clone());

    let mut default_symbol_table_for_others = SymbolTable::new(&[]);

    // Prelude for the core module
    // Expose the basic primitive types, like `Int`, `String`, `Float`, `Bool`
    // so they can be references without a `use core::{Int, String, Float, Bool}` statement.
    default_symbol_table_for_others
        .extend_alias_from(&core_module_ref.symbol_table)
        .unwrap();

    // Add `core` module without the version number, so they can be referenced from code
    default_symbol_table_for_others
        .add_package_version(swamp_core::PACKAGE_NAME, compiler_version)
        .expect("should work");

    /*
    let source_map_lookup = SourceMapWrapper {
        source_map,
        current_dir: current_dir().unwrap(),
    };

    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let symbol_table_display = SymbolTableDisplay {
        symbol_table: &default_symbol_table_for_others,
        source_map_display: &pretty_printer,
    };

     */

    let program = Program::new(state, modules, default_symbol_table_for_others);

    let result = BootstrapResult {
        program,
        core_module_path: core_module_ref.symbol_table.module_path().clone(),
    };
    Ok(result)

    Ok()
     */
}

pub fn compile_and_analyze_all_modules(
    module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
    core_symbol_table: SymbolTableRef,
) -> Result<(), ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order =
        parse_local_modules_and_get_order(module_path, &mut dependency_parser, source_map)?;

    analyze_modules_in_order(
        &mut resolved_program.state,
        &resolved_program.default_symbol_table,
        &mut resolved_program.modules,
        &core_symbol_table,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(())
}

pub fn remove_version_from_package_name_regex(package_name_with_version: &str) -> String {
    let re = Regex::new(
        r"-(?P<version>[0-9]+(?:\.[0-9]+)*(?:-[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*)?)?(?:\+.*)?$",
    )
    .unwrap();
    re.replace(package_name_with_version, "").to_string()
}

/// # Errors
///
/// # Panics
///
pub fn compile_analyze_and_link_without_version(
    root_module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
    core_symbol_table: SymbolTableRef,
) -> Result<(), ScriptResolveError> {
    let mangrove_render_result = compile_and_analyze(
        root_module_path,
        resolved_program,
        source_map,
        core_symbol_table,
    );

    match mangrove_render_result {
        Ok(..) => {}
        Err(err) => {
            show_script_resolve_error(&err, source_map, Path::new(""));
            Err(err)?;
        }
    }
    let mangrove_render_module = resolved_program.modules.get(root_module_path).unwrap();

    let first_part = remove_version_from_package_name_regex(&root_module_path[0]);
    let mut without_version_path: Vec<String> = root_module_path.to_vec();
    without_version_path[0] = first_part;

    resolved_program
        .modules
        .link_module(&without_version_path, mangrove_render_module.clone());

    Ok(())
}

/// # Errors
///
pub fn compile_and_analyze(
    root_module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
    core_symbol_table: SymbolTableRef,
) -> Result<(), ScriptResolveError> {
    compile_and_analyze_all_modules(
        root_module_path,
        resolved_program,
        source_map,
        core_symbol_table,
    )
}

pub fn current_path() -> PathBuf {
    current_dir().unwrap()
}

pub struct CompileOptions {
    pub show_semantic: bool,
    pub show_modules: bool,
}

/// # Errors
///
/// # Panics
///
pub fn bootstrap_and_compile(
    source_map: &mut SourceMap,
    root_path: &[String],
    options: &CompileOptions,
) -> Result<Program, ScriptResolveError> {
    let bootstrap_timer = ScopedTimer::new("bootstrap");
    let bootstrap_result = bootstrap_modules(source_map).inspect_err(|err| {
        show_script_resolve_error(err, source_map, &current_path());
    })?;
    drop(bootstrap_timer);
    let mut program = bootstrap_result.program;

    let core_symbol_table = program
        .modules
        .get(&bootstrap_result.core_module_path)
        .unwrap()
        .symbol_table
        .clone();

    /*
       let mut modules = Modules::new();
       let program_state = ProgramState::new();
       let default_symbol_table = SymbolTable::new(&[]);
       let core_symbol_table = SymbolTable::new(&[]);
       let mut program = Program::new(program_state, modules, default_symbol_table);

    */
    let compile_all_modules_timer = ScopedTimer::new("compile all modules");
    compile_and_analyze_all_modules(
        root_path,
        &mut program,
        source_map,
        core_symbol_table.into(),
    )
    .inspect_err(|err| {
        show_script_resolve_error(err, source_map, &current_path());
    })?;

    drop(compile_all_modules_timer);

    if options.show_modules {
        debug_all_modules(&program.modules, source_map);
    }

    if options.show_semantic {
        debug_all_impl_functions(&program.state.associated_impls, source_map);
    }

    Ok(program)
}

pub fn debug_all_modules(modules: &Modules, source_map: &SourceMap) {
    for (_name, module) in modules.modules() {
        debug_module(&module.symbol_table, source_map);
    }
}
pub fn debug_module(symbol_table: &SymbolTable, source_map: &SourceMap) {
    let source_map_lookup = SourceMapWrapper {
        source_map,
        current_dir: current_dir().unwrap(),
    };
    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let symbol_table_display = SymbolTableDisplay {
        symbol_table,
        source_map_display: &pretty_printer,
    };

    info!(
        "module: {}{}",
        formal_module_name(&symbol_table.module_path()),
        symbol_table_display
    );
}

fn debug_all_impl_functions(all_impls: &AssociatedImpls, source_map: &mut SourceMap) {
    let source_map_lookup = SourceMapWrapper {
        source_map,
        current_dir: current_dir().unwrap(),
    };
    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let symbol_table_display = ImplsDisplay {
        all_impls,
        source_map: &pretty_printer,
    };

    info!("impls: {}", symbol_table_display);
}

pub fn compile_string(script: &str) -> Result<(Program, ModuleRef, SourceMap), ScriptResolveError> {
    let mut source_map = SourceMap::new(&SeqMap::default()).unwrap();
    let file_id = 0xffff;

    if let Some(swamp_home) = swamp_registry_path() {
        source_map.add_mount("registry", &swamp_home).unwrap();
    }

    source_map.add_mount("crate", Path::new("/tmp/")).unwrap();
    source_map.add_to_cache("crate", Path::new("test.swamp"), script, file_id);

    let resolved_path_str = vec!["crate".to_string(), "test".to_string()];
    let compile_options = CompileOptions {
        show_semantic: false,
        show_modules: false,
    };
    let program = bootstrap_and_compile(&mut source_map, &resolved_path_str, &compile_options)?;
    let main_module = program.modules.get(&resolved_path_str).unwrap().clone();

    Ok((program, main_module, source_map))
}
