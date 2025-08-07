/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use regex::Regex;
use seq_map::SeqMap;
use source_map_cache::SourceMap;
use source_map_cache::SourceMapWrapper;
use std::env::current_dir;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;
use swamp_analyzer::Analyzer;
pub use swamp_analyzer::prelude::Program;
use swamp_core::text::core_text;
use swamp_dep_loader::{
    DependencyParser, ParsedAstModule, RunMode, parse_local_modules_and_get_order,
    parse_single_module_from_text, swamp_registry_path,
};
use swamp_error_report::analyze::show_analyzer_error;
use swamp_error_report::prelude::Kind;
use swamp_error_report::{ScriptResolveError, prelude::show_script_resolve_error};
use swamp_modules::modules::{ModuleRef, Modules};
use swamp_modules::prelude::Module;
use swamp_modules::symtbl::{DefinitionTable, SymbolTableRef};
use swamp_pretty_print::{ImplsDisplay, SourceMapDisplay, SymbolTableDisplay};
use swamp_program_analyzer::analyze_modules_in_order;
use swamp_semantic::err::Error;
use swamp_semantic::{AssociatedImpls, ProgramState, formal_module_name};
use swamp_std::std_text;
use swamp_types::prelude::print_types;
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
    default_symbol_table: DefinitionTable,
    modules: &Modules,
    core_symbol_table: SymbolTableRef,
    parsed_ast_module: &ParsedAstModule,
    source_map: &SourceMap,
    versioned_module_path: &[String],
) -> DefinitionTable {
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

pub fn create_default_source_map(local_path: &Path) -> io::Result<SourceMap> {
    create_source_map(&local_path.join("packages/"), &local_path.join("scripts/"))
}

pub fn create_default_source_map_from_scripts_dir(local_path: &Path) -> io::Result<SourceMap> {
    let root = local_path.join("..");
    create_default_source_map(&root)
}

pub fn create_default_source_map_crate_only(local_path: &Path) -> io::Result<SourceMap> {
    let mut mounts = SeqMap::new();
    mounts
        .insert("crate".to_string(), local_path.to_path_buf())
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
        swamp_core::create_module(&compiler_version, &mut state.types, 0);

    let core_path = core_module_with_intrinsics.definition_table.module_path();
    let core_parsed_ast_module =
        parse_single_module_from_text(source_map, &core_path, &core_text())?;

    let half_completed_core_symbol_table = core_module_with_intrinsics.definition_table.clone();
    let default_symbol_table_for_core_with_intrinsics = half_completed_core_symbol_table.clone();

    let mut analyzed_core_symbol_table = analyze_single_module(
        &mut state,
        default_symbol_table_for_core_with_intrinsics.clone(),
        &modules,
        half_completed_core_symbol_table.clone().into(),
        &core_parsed_ast_module,
        source_map,
        &core_module_with_intrinsics.definition_table.module_path(),
    );

    // After this the core symbol table is done
    analyzed_core_symbol_table
        .extend_basic_from(&core_module_with_intrinsics.definition_table)
        .expect("couldn't extend core");
    let mut default_module = swamp_core::create_module_with_name(&[], 0);
    default_module
        .definition_table
        .extend_alias_from(&analyzed_core_symbol_table)
        .expect("extend basic alias and functions from core");

    let core_module = Module::new(analyzed_core_symbol_table, vec![], None, 0);
    modules.add(ModuleRef::from(core_module));

    // ---------

    let std_path = &["std".to_string()];
    let std_module_with_intrinsics = swamp_core::create_module_with_name(std_path, 0);
    let std_ast_module = parse_single_module_from_text(source_map, std_path, &std_text())?;
    let analyzed_std_symbol_table = analyze_single_module(
        &mut state,
        default_symbol_table_for_core_with_intrinsics,
        &modules,
        half_completed_core_symbol_table.into(),
        &std_ast_module,
        source_map,
        &std_module_with_intrinsics.definition_table.module_path(),
    );
    default_module
        .definition_table
        .extend_basic_from(&analyzed_std_symbol_table)
        .expect("extend basics from core");

    let analyzed_std_module = Module::new(analyzed_std_symbol_table, vec![], None, 0);

    modules.add(ModuleRef::from(analyzed_std_module));

    let bootstrap_program = Program::new(state, modules, default_module.definition_table);

    let result = BootstrapResult {
        program: bootstrap_program,
        core_module_path: core_path,
    };

    Ok(result)
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

#[must_use]
pub fn remove_version_from_package_name_regex(package_name_with_version: &str) -> String {
    let re = Regex::new(
        r"-(?P<version>[0-9]+(?:\.[0-9]+)*(?:-[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*)?)?(?:\+.*)?$",
    )
    .unwrap();
    re.replace(package_name_with_version, "").to_string()
}

#[must_use]
pub fn current_path() -> PathBuf {
    current_dir().unwrap()
}

pub struct CompileOptions {
    pub show_semantic: bool,
    pub show_modules: bool,
    pub show_errors: bool,
    pub show_warnings: bool,
    pub show_hints: bool,
    pub show_information: bool,
    pub show_types: bool,
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
        .definition_table
        .clone();

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

    if options.show_types {
        let mut str = String::new();
        print_types(&mut str, &program.state.types).expect("should work");
        eprintln!("{str}");
        //info!(str, "types");
    }

    let source_map_wrapper = SourceMapWrapper {
        source_map,
        current_dir: current_dir().unwrap(),
    };

    if options.show_errors {
        show_errors(&program.state.errors, &source_map_wrapper);
    }

    if options.show_warnings {
        /* TODO: Not working perfectly yet
        for x in program.state.symbols.iter_all() {
            if !program.state.refs.is_used(&x.id) {
                let name = source_map_wrapper.get_text(&x.name);
                if x.name.span.file_id <= 2 {
                    eprintln!("name: {name} {:?} {}", x.kind, x.name.span.file_id);
                    continue;
                }
                let line_info = source_map_wrapper.get_line(&x.name.span);
                eprintln!("not used: '{}' ({:?}) file://{}:{}:{}\n{}", name, x.kind, line_info.relative_file_name, line_info.row, line_info.col, line_info.line);
            }
        }

         */
    }

    if options.show_hints {
        show_hints(&program.state.hints, &source_map_wrapper);
    }

    if options.show_information {
        show_information(&program.state.infos, &source_map_wrapper);
    }

    Ok(program)
}

fn show_errors(errors: &[Error], source_map_wrapper: &SourceMapWrapper) {
    for err in errors.iter().take(4) {
        show_analyzer_error(
            err,
            Kind::Error,
            source_map_wrapper.source_map,
            &source_map_wrapper.current_dir,
        );
    }
}

fn show_hints(hints: &[Error], source_map_wrapper: &SourceMapWrapper) {
    for err in hints {
        show_analyzer_error(
            err,
            Kind::Warning,
            source_map_wrapper.source_map,
            &source_map_wrapper.current_dir,
        );
    }
}

fn show_information(infos: &[Error], source_map_wrapper: &SourceMapWrapper) {
    for err in infos {
        show_analyzer_error(
            err,
            Kind::Help,
            source_map_wrapper.source_map,
            &source_map_wrapper.current_dir,
        );
    }
}

pub fn debug_all_modules(modules: &Modules, source_map: &SourceMap) {
    for (_name, module) in modules.modules() {
        debug_module(&module.definition_table, source_map);
    }
}
pub fn debug_module(symbol_table: &DefinitionTable, source_map: &SourceMap) {
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

#[must_use]
pub fn compile_string(script: &str, run_mode: &RunMode) -> (Program, ModuleRef, SourceMap) {
    let mut source_map = SourceMap::new(&SeqMap::default()).unwrap();
    let file_id = 0xffff;

    if let Some(swamp_home) = swamp_registry_path(run_mode) {
        source_map.add_mount("registry", &swamp_home).unwrap();
    }

    source_map.add_mount("crate", Path::new("/tmp/")).unwrap();
    source_map.add_to_cache("crate", Path::new("test.swamp"), script, file_id);

    let resolved_path_str = vec!["crate".to_string(), "test".to_string()];
    let compile_options = CompileOptions {
        show_semantic: false,
        show_modules: false,
        show_errors: true,
        show_warnings: true,
        show_hints: false,
        show_information: false,
        show_types: false,
    };
    let program =
        bootstrap_and_compile(&mut source_map, &resolved_path_str, &compile_options).unwrap();
    let main_module = program.modules.get(&resolved_path_str).unwrap().clone();

    (program, main_module, source_map)
}
