/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMap;
use swamp_analyzer::prelude::Program;
use swamp_analyzer::Analyzer;
use swamp_dep_loader::{parse_local_modules_and_get_order, DependencyParser, ParsedAstModule};
use swamp_modules::prelude::*;
use swamp_modules::symtbl::SymbolTableRef;
use swamp_semantic::prelude::Error;
use swamp_semantic::{InternalMainExpression, ProgramState, SemanticError};
use time_dilation::ScopedTimer;
use tracing::debug;

#[derive(Debug)]
pub enum LoaderErr {
    CouldNotLoad,
    SemanticError(SemanticError),
    AnalyzerError(Error),
}

impl From<Error> for LoaderErr {
    fn from(value: Error) -> Self {
        Self::AnalyzerError(value)
    }
}

/// # Errors
///
pub fn analyze_module(
    state: &mut ProgramState,
    default_lookup_symbol_table: &DefinitionTable,
    modules: &mut Modules,
    core_symbol_table: &SymbolTableRef,
    source_map: &SourceMap,
    module_path: &[String],
    ast_module: &ParsedAstModule,
) -> Result<(DefinitionTable, Vec<Error>, Option<InternalMainExpression>), LoaderErr> {
    //debug!(?module_path, "analyze module");
    let debug_string = format!("analyze module {module_path:?}");
    let _analyze_timer = ScopedTimer::new(&debug_string);

    let mut analyzer = Analyzer::new(
        state,
        modules,
        core_symbol_table.clone(),
        source_map,
        module_path,
        ast_module.file_id,
    );

    analyzer.shared.lookup_table = default_lookup_symbol_table.clone();

    let statements = {
        for ast_def in ast_module.ast_module.definitions() {
            //debug!(?ast_def, "analyze definition");
            analyzer.analyze_definition(ast_def);
        }

        if let Some(expr) = ast_module.ast_module.expression() {
            let internal_main = analyzer.analyze_main_expression(expr);
            Some(internal_main)
        } else {
            None
        }
    };

    Ok((
        analyzer.shared.definition_table,
        analyzer.shared.state.errors().clone(),
        statements,
    ))
}

/// # Errors
///
/// # Panics
///
pub fn analyze_modules_in_order(
    state: &mut ProgramState,
    default_lookup_symbol_table: &DefinitionTable,
    modules: &mut Modules,
    core_symbol_table: &SymbolTableRef,
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), LoaderErr> {
    debug!(?module_paths_in_order, "analyzing modules in order");
    for module_path in module_paths_in_order {
        if *module_path == ["core"] || *module_path == ["std"] {
            continue;
        }
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            //let process_span = span!(Level::TRACE, "analyze mod", path = ?module_path);
            //let _enter = process_span.enter();

            let (analyzed_symbol_table, errors, maybe_expression) = analyze_module(
                state,
                default_lookup_symbol_table,
                modules,
                core_symbol_table,
                source_map,
                module_path,
                parse_module,
            )?;

            let analyzed_module = Module::new(analyzed_symbol_table, errors, maybe_expression, parse_module.file_id);
            modules.add(analyzed_module.into());
        } else {
            panic!("could not load")
        }
    }
    Ok(())
}

/// # Errors
///
/// # Panics
///
pub fn compile_and_analyze_all_modules(
    module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
    core_symbol_table: &SymbolTableRef,
) -> Result<(), LoaderErr> {
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order =
        parse_local_modules_and_get_order(module_path, &mut dependency_parser, source_map).unwrap(); // TODO: FIX THIS

    analyze_modules_in_order(
        &mut resolved_program.state,
        &resolved_program.default_symbol_table,
        &mut resolved_program.modules,
        core_symbol_table,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(())
}
