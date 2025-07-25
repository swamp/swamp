/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::conn::SendConnection;
use crate::{to_relative_key, uri_to_path};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams, Hover,
    HoverContents, HoverParams, MarkupContent, MarkupKind, Position, PublishDiagnosticsParams,
    Range, Uri,
};
use source_map_cache::SourceMap;
use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;
use swamp::prelude::{Error, ErrorKind};
use tracing::{debug, error};

fn determine_mount_and_key(uri: &Uri, workspace_roots: &[&Path]) -> (String, String) {
    if let Some(file_path) = uri_to_path(uri) {
        // Try to find which workspace root this file belongs to
        for &workspace_root in workspace_roots {
            if let Ok(relative_key) = to_relative_key(workspace_root, &file_path) {
                // Determine mount based on the relative path and strip the prefix
                if let Some(stripped) = relative_key.strip_prefix("scripts/") {
                    return ("crate".to_string(), stripped.to_string());
                } else if let Some(stripped) = relative_key.strip_prefix("packages/") {
                    return ("registry".to_string(), stripped.to_string());
                } else {
                    // Default fallback - assume it's in crate mount
                    return ("crate".to_string(), relative_key);
                }
            }
        }
    }

    // Fallback if we can't determine the relative path
    ("crate".to_string(), uri.to_string())
}

pub struct Server {
    pub source_map: SourceMap,
    workspace_roots: Vec<std::path::PathBuf>,
    // Cache of diagnostics per file URI - tracks what we've sent to avoid unnecessary updates
    diagnostics_cache: HashMap<Uri, Vec<Diagnostic>>,
    // Map from file_id to URI for diagnostics
    file_id_to_uri: HashMap<u32, Uri>,
}

impl Server {
    pub fn new(source_map: SourceMap, workspace_roots: Vec<std::path::PathBuf>) -> Self {
        Self {
            source_map,
            workspace_roots,
            diagnostics_cache: HashMap::new(),
            file_id_to_uri: HashMap::new(),
        }
    }

    pub(crate) fn on_did_open(
        &mut self,
        params: &DidOpenTextDocumentParams,
        connection: &dyn SendConnection,
    ) {
        let uri = params.text_document.uri.clone();
        let text = &params.text_document.text;

        let workspace_refs: Vec<&Path> = self.workspace_roots.iter().map(|p| p.as_path()).collect();
        let (mount_name, relative_key) = determine_mount_and_key(&uri, &workspace_refs);

        let file_id = self.source_map.set(&mount_name, (&relative_key).as_ref(), text);

        // Track the mapping from file_id to URI for diagnostics
        let file_id_u32: u32 = file_id.into();
        self.file_id_to_uri.insert(file_id_u32, uri.clone());

        debug!(path=?uri_to_path(&uri).unwrap(), bytes=text.len(), mount=%mount_name, key=%relative_key, file_id=%file_id, file_id_u32=%file_id_u32, "did_open");

        self.recompile(connection);
    }

    pub fn send_diagnostics(&mut self, severity: DiagnosticSeverity, errors: &[Error], connection: &dyn SendConnection) {
        debug!(error_count=%errors.len(), "send_diagnostics called");

        // Group errors by file_id
        let mut errors_by_file: HashMap<u32, Vec<&Error>> = HashMap::new();
        for error in errors {
            let file_id = error.node.span.file_id.into();
            errors_by_file.entry(file_id).or_default().push(error);
            debug!(file_id=%file_id, error=?error.kind, "grouping error");
        }

        debug!(files_with_errors=%errors_by_file.len(), known_files=%self.file_id_to_uri.len(), "diagnostics summary");

        // Send diagnostics for files with errors
        for (file_id, file_errors) in &errors_by_file {
            if let Some(uri) = self.file_id_to_uri.get(&file_id) {
                let mut diagnostics = Vec::new();

                for err in file_errors {
                    let (line, col) = self.source_map.get_span_location_utf8(err.node.span.file_id, err.node.span.offset as usize);

                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: Position {
                                line: line as u32,
                                character: col as u32,
                            },
                            end: Position {
                                line: line as u32,
                                character: (col + err.node.span.length as usize) as u32,
                            },
                        },
                        severity: Some(severity),
                        code: None,
                        code_description: None,
                        source: Some("swamp-lsp".into()),
                        message: err.kind.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }

                // Update cache and send diagnostics
                self.diagnostics_cache.insert(uri.clone(), diagnostics.clone());

                let params = PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics,
                    version: None,
                };

                debug!(uri=?uri, count=params.diagnostics.len(), "sending diagnostics");
                connection.send_publish_diagnostics(params);
            } else {
                debug!(file_id=%file_id, "no URI mapping found for file_id - fix compiler file ID generation");
            }
        }

        // Clear diagnostics for files that no longer have errors
        for (uri, cached_diagnostics) in &self.diagnostics_cache.clone() {
            if !cached_diagnostics.is_empty() && !errors_by_file.keys().any(|file_id| self.file_id_to_uri.get(file_id) == Some(uri)) {
                let params = PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics: Vec::new(),
                    version: None,
                };
                self.diagnostics_cache.insert(uri.clone(), Vec::new());
                connection.send_publish_diagnostics(params);
                debug!(uri=?uri, "cleared diagnostics");
            }
        }
    }

    pub fn recompile(&mut self, connection: &dyn SendConnection) {
        debug!("starting recompile");
        debug!(file_mappings=?self.file_id_to_uri, "current file mappings");

        let result = swamp::compile_and_analyze(&["crate".to_string(), "main".to_string()], &mut self.source_map);
        match result {
            Ok(program) => {
                debug!(error_count=%program.state.errors.len(), "compilation completed");
                self.send_diagnostics(DiagnosticSeverity::ERROR, &program.state.errors, connection);
            }
            Err(err) => {
                error!(?err, "internal compilation error");
            }
        }
    }

    pub(crate) fn on_did_change(
        &mut self,
        params: &DidChangeTextDocumentParams,
        connection: &dyn SendConnection,
    ) {
        let uri = params.text_document.uri.clone();
        let new_text = params.content_changes[0].text.clone();

        let workspace_refs: Vec<&Path> = self.workspace_roots.iter().map(|p| p.as_path()).collect();
        let (mount_name, relative_key) = determine_mount_and_key(&uri, &workspace_refs);

        let file_id = self.source_map.set(&mount_name, (&relative_key).as_ref(), &new_text);

        // Update the mapping for changed files
        self.file_id_to_uri.insert(file_id.into(), uri.clone());

        debug!(path=?uri_to_path(&uri).unwrap(), bytes=new_text.len(), mount=%mount_name, key=%relative_key, file_id=%file_id, "did_change");
        self.recompile(connection);
    }

    pub(crate) fn on_hover(&self, params: &HoverParams, connection: &dyn SendConnection) -> Hover {
        debug!("on_over");
        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: "# Markdown".to_string(),
        });

        Hover {
            contents,
            range: None,
        }
    }

    fn publish_fake_diagnostics(uri: Uri, text: &str, connection: &dyn SendConnection) {
        let mut diagnostics = Vec::new();
        for (i, line) in text.lines().enumerate() {
            if let Some(col) = line.find("error") {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position {
                            line: i as u32,
                            character: col as u32,
                        },
                        end: Position {
                            line: i as u32,
                            character: (col + 5) as u32,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("swamp-lsp".into()),
                    message: "Found error in code".into(),
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }
        }
        let params = PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        };

        connection.send_publish_diagnostics(params);
    }
}
