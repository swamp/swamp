/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;
use dirs::home_dir;
use seq_map::SeqMap;
use source_map_cache::{FileId, SourceMap};
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::{env, io};
use swamp_ast::Function;
use swamp_ast::prelude::*;
use swamp_parser::{AstParser, SpecificError};
use time_dilation::ScopedTimer;
use tracing::error;

pub struct ParseRoot;

#[derive(Debug)]
pub enum ParseRootError {
    ParserError(ParserError),
}

#[derive(Debug)]
pub struct ParsedAstModule {
    pub ast_module: swamp_ast::Module,
    pub file_id: FileId,
}

impl ParsedAstModule {
    // TODO: HACK: declare_external_function() should be removed
    pub fn declare_external_function(
        &mut self,
        node: &Node,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
    ) {
        let fake_identifier = Node::default();

        let signature = FunctionDeclaration {
            name: fake_identifier,
            params: parameters,
            self_parameter: None,
            return_type,
        };
        let external_signature = Function::External(Node::default(), signature);

        let fake_def = Definition {
            node: node.clone(),
            kind: DefinitionKind::FunctionDef(external_signature),
            attributes: vec![],
        };

        self.ast_module.definitions.insert(
            0, // add it first
            fake_def,
        );
    }
}

#[derive(Debug)]
pub struct RelativePath(pub String);

#[derive(Debug)]
pub struct ParserError {
    pub node: Node,
    pub specific: SpecificError,
    pub file_id: FileId,
}

impl Default for ParseRoot {
    fn default() -> Self {
        Self::new()
    }
}

impl ParseRoot {
    #[must_use]
    pub const fn new() -> Self {
        Self {}
    }

    pub fn parse(
        &self,
        contents: String,
        file_id: FileId,
    ) -> Result<ParsedAstModule, ParseRootError> {
        let ast_program = AstParser.parse_module(&contents).map_err(|err| {
            let new_err = ParserError {
                node: Node { span: err.span },
                specific: err.specific,
                file_id,
            };
            ParseRootError::ParserError(new_err)
        })?;

        Ok(ParsedAstModule {
            ast_module: ast_program,
            file_id,
        })
    }
}

#[derive(Clone)]
#[allow(unused)]
pub struct ModuleInfo {
    path: Vec<String>,
    imports: Vec<Vec<String>>,
    uses: Vec<Vec<String>>,
    parsed: bool,
    analyzed: bool,
}

pub struct DependencyParser {
    pub import_scanned_modules: SeqMap<Vec<String>, ModuleInfo>,
    already_parsed_modules: SeqMap<Vec<String>, ParsedAstModule>,
    pub already_resolved_modules: HashSet<Vec<String>>,
}

impl Default for DependencyParser {
    fn default() -> Self {
        Self::new()
    }
}

impl DependencyParser {
    #[must_use]
    pub fn new() -> Self {
        Self {
            import_scanned_modules: SeqMap::new(),
            already_parsed_modules: SeqMap::new(),
            already_resolved_modules: HashSet::new(),
        }
    }

    pub fn add_resolved_module(&mut self, module_path: Vec<String>) {
        self.already_resolved_modules.insert(module_path);
    }

    pub fn add_ast_module(&mut self, module_path: Vec<String>, parsed_module: ParsedAstModule) {
        self.already_parsed_modules
            .insert(module_path, parsed_module)
            .expect("insert");
    }
}

#[derive(Debug)]
pub enum DependencyError {
    CircularDependency(Vec<String>),
    ParseRootError(ParseRootError),
    ReadFileError(io::Error),
}

impl From<ParseRootError> for DependencyError {
    fn from(err: ParseRootError) -> Self {
        Self::ParseRootError(err)
    }
}

pub const LOCAL_ROOT_PACKAGE_PATH: &str = "crate";

#[must_use]
pub fn get_all_local_paths(
    source_map: &SourceMap,
    parsed_module: &ParsedAstModule,
) -> (Vec<Vec<String>>, Vec<Vec<String>>) {
    let mut imports = vec![];
    let mut uses = vec![];

    for def in parsed_module.ast_module.definitions() {
        match &def.kind {
            DefinitionKind::Mod(import) => {
                let mut sections = Vec::new();
                sections.push(LOCAL_ROOT_PACKAGE_PATH.to_string());
                for section_node in &import.module_path.0 {
                    let import_path = source_map
                        .get_span_source(
                            parsed_module.file_id,
                            section_node.span.offset as usize,
                            section_node.span.length.into(),
                        )
                        .to_string();
                    sections.push(import_path);
                }

                imports.push(sections);
            }

            DefinitionKind::Use(import) => {
                let mut sections = Vec::new();
                for section_node in &import.module_path.0 {
                    let import_path = source_map
                        .get_span_source(
                            parsed_module.file_id,
                            section_node.span.offset as usize,
                            section_node.span.length.into(),
                        )
                        .to_string();
                    sections.push(import_path);
                }
                uses.push(sections);
            }
            _ => continue,
        }
    }

    (imports, uses)
}

#[must_use]
pub fn module_path_to_relative_swamp_file(module_path_vec: &[String]) -> PathBuf {
    let mut path_buf = PathBuf::new();

    let orig_len = module_path_vec.len();

    let converted_path = if module_path_vec[0] == "crate" {
        &module_path_vec[1..]
    } else {
        module_path_vec
    };

    path_buf.push(converted_path.join("/"));
    if orig_len == 1 {
        path_buf.push("lib"); // lib is default if the path only contains the package root
    }

    path_buf.set_extension("swamp");

    path_buf
}

#[must_use]
pub fn module_path_to_relative_swamp_file_string(module_path_vec: &[String]) -> String {
    module_path_to_relative_swamp_file(module_path_vec)
        .to_str()
        .unwrap()
        .into()
}

#[must_use]
pub fn mount_name_from_path(path: &[String]) -> &str {
    if path[0] == "crate" {
        "crate"
    } else {
        "registry"
    }
}

/// Parses just a single module. Any `mod` keywords in this file will be ignored. So this
/// is mainly for internal use.
pub fn parse_single_module(
    source_map: &mut SourceMap,
    module_path: &[String],
) -> Result<ParsedAstModule, DependencyError> {
    let debug = format!("parse module {module_path:?}");
    let _parse_module_timer = ScopedTimer::new(&debug);

    let mount_name = mount_name_from_path(module_path);

    let (file_id, script) = source_map
        .read_file_relative(
            mount_name,
            &module_path_to_relative_swamp_file_string(module_path),
        )
        .map_err(DependencyError::ReadFileError)?;

    let parse_module = ParseRoot.parse(script, file_id)?;

    Ok(parse_module)
}

pub fn parse_single_module_from_text(
    source_map: &mut SourceMap,
    module_path: &[String],
    script: &str,
) -> Result<ParsedAstModule, DependencyError> {
    let debug = format!("parse module {module_path:?}");
    let _parse_module_timer = ScopedTimer::new(&debug);

    let mount_name = mount_name_from_path(module_path);

    let file_id = source_map.add_manual_no_id(
        mount_name,
        module_path_to_relative_swamp_file_string(module_path).as_ref(),
        script,
    );

    let parse_module = ParseRoot.parse(script.to_string(), file_id)?;

    Ok(parse_module)
}

impl DependencyParser {
    pub fn parse_local_modules(
        &mut self,
        module_path: &[String],
        source_map: &mut SourceMap,
    ) -> Result<(), DependencyError> {
        let mut to_parse = vec![module_path.to_vec()];

        while let Some(path) = to_parse.pop() {
            let module_path_vec = &path.clone();
            if self.import_scanned_modules.contains_key(module_path_vec) {
                continue;
            }

            let parsed_module_to_scan =
                if let Some(parsed_module) = self.already_parsed_modules.get(module_path_vec) {
                    parsed_module
                } else if self.already_resolved_modules.contains(module_path_vec) {
                    continue;
                } else if path == ["core"] || path == ["std"] {
                    continue;
                } else {
                    let parsed_ast_module = parse_single_module(source_map, &path)?;

                    self.already_parsed_modules
                        .insert(path.clone(), parsed_ast_module)
                        .expect("insert");

                    self.already_parsed_modules
                        .get(&path.clone())
                        .expect("we just inserted it")
                };

            let (imports, uses) = get_all_local_paths(source_map, parsed_module_to_scan);
            let filtered_imports: Vec<Vec<String>> = imports
                .into_iter()
                .filter(|import| !self.already_resolved_modules.contains(import))
                .collect();

            let filtered_uses: Vec<Vec<String>> = uses
                .into_iter()
                .filter(|import| !self.already_resolved_modules.contains(import))
                .collect();

            self.import_scanned_modules
                .insert(
                    path.clone(),
                    ModuleInfo {
                        path: path.clone(),
                        imports: filtered_imports.clone(),
                        uses: filtered_uses.clone(),
                        parsed: false,
                        analyzed: false,
                    },
                )
                .expect("insert");

            to_parse.extend(filtered_imports.clone());

            to_parse.extend(filtered_uses.clone());
        }
        Ok(())
    }

    #[must_use]
    pub fn get_parsed_module(&self, path: &[String]) -> Option<&ParsedAstModule> {
        self.already_parsed_modules.get(&path.to_vec())
    }

    pub fn get_parsed_module_mut(&mut self, path: &[String]) -> Option<&mut ParsedAstModule> {
        self.already_parsed_modules.get_mut(&path.to_vec())
    }

    pub(crate) fn get_analysis_order(&self) -> Result<Vec<Vec<String>>, DependencyError> {
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();

        fn visit(
            graph: &DependencyParser,
            path: &[String],
            visited: &mut HashSet<Vec<String>>,
            temp_visited: &mut HashSet<Vec<String>>,
            order: &mut Vec<Vec<String>>,
            depth: usize,
        ) -> Result<(), DependencyError> {
            if temp_visited.contains(path) {
                error!(?path, depth, "already visited this path");
                return Err(DependencyError::CircularDependency(Vec::from(path)));
            }

            if visited.contains(path) {
                return Ok(());
            }

            //trace!(?path, depth, "visit path");
            temp_visited.insert(Vec::from(path));

            if let Some(module) = graph.import_scanned_modules.get(&path.to_vec()) {
                for import in &module.uses {
                    visit(graph, import, visited, temp_visited, order, depth + 1)?;
                }
                for import in &module.imports {
                    visit(graph, import, visited, temp_visited, order, depth + 1)?;
                }
            }

            order.push(Vec::from(path));
            visited.insert(Vec::from(path));

            temp_visited.remove(path);

            Ok(())
        }

        for path in self.import_scanned_modules.keys() {
            if !visited.contains(path) {
                visit(self, path, &mut visited, &mut temp_visited, &mut order, 0)?;
            }
        }

        Ok(order)
    }
}

#[derive(Debug)]
pub enum DepLoaderError {
    DependencyError(DependencyError),
}

impl From<DependencyError> for DepLoaderError {
    fn from(e: DependencyError) -> Self {
        Self::DependencyError(e)
    }
}

/// # Errors
///
fn os_home_relative_path(project_name: &str) -> Option<PathBuf> {
    home_dir().map(|home_path| home_path.join(format!(".{project_name}")))
}

fn current_directory_relative_path(project_name: &str) -> PathBuf {
    Path::new(".").join(format!(".{}", project_name))
}

#[must_use]
pub fn path_from_environment_variable() -> Option<PathBuf> {
    env::var("SWAMP_HOME")
        .map(|string_value| Path::new(&string_value).to_path_buf())
        .ok()
}

#[must_use]
pub fn swamp_home(run_mode: &RunMode) -> Option<PathBuf> {
    // First try environment variable
    match run_mode {
        RunMode::Development => {
            path_from_environment_variable().or_else(|| os_home_relative_path("swamp"))
        }
        RunMode::Deployed => Some(Path::new(".").to_path_buf()),
    }
}

// Verifies the structure of swamp_home to make sure if we can use it
#[must_use]
pub fn verify_if_swamp_home_seems_correct(swamp_home: &Path) -> bool {
    let mut swamp_packages_dir = swamp_home.to_path_buf();

    swamp_packages_dir.push("packages");

    swamp_packages_dir.exists() && swamp_packages_dir.is_dir()
}

pub enum RunMode {
    Development,
    Deployed,
}

/// # Errors
///
#[must_use]
pub fn swamp_registry_path(run_mode: &RunMode) -> Option<PathBuf> {
    let swamp_home = swamp_home(run_mode)?;

    if verify_if_swamp_home_seems_correct(&swamp_home) {
        let mut packages_path = swamp_home;
        packages_path.push("packages");

        Some(packages_path)
    } else {
        None
    }
}

pub fn parse_local_modules_and_get_order(
    module_path: &[String],
    dependency_parser: &mut DependencyParser,
    source_map: &mut SourceMap,
) -> Result<Vec<Vec<String>>, DepLoaderError> {
    dependency_parser.parse_local_modules(module_path, source_map)?;

    let module_paths_in_order = dependency_parser.get_analysis_order()?;

    Ok(module_paths_in_order)
}
