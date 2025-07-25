/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::conn::SendConnection;
use crate::{to_relative_key, uri_to_path};
use lsp_types::lsif::Vertex::DiagnosticResult;
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams, Hover,
    HoverContents, HoverParams, MarkupContent, MarkupKind, Position, PublishDiagnosticsParams,
    Range, Uri,
};
use source_map_cache::SourceMap;
use std::path::Path;
use std::str::FromStr;
use swamp::prelude::{Error, ErrorKind};
use tracing::{debug, error};

fn determine_mount_and_key(uri: &Uri, workspace_roots: &[&Path]) -> (String, String) {
    if let Some(file_path) = uri_to_path(uri) {
        // Try to find which workspace root this file belongs to
        for &workspace_root in workspace_roots {
            if let Ok(relative_key) = to_relative_key(workspace_root, &file_path) {
                // Determine mount based on the relative path
                let mount_name = if relative_key.starts_with("scripts/") {
                    "crate"
                } else if relative_key.starts_with("packages/") {
                    "registry"
                } else {
                    "crate" // default fallback
                };

                return (mount_name.to_string(), relative_key);
            }
        }
    }

    // Fallback if we can't determine the relative path
    ("crate".to_string(), uri.to_string())
}

pub struct Server {
    pub source_map: SourceMap,
    workspace_roots: Vec<std::path::PathBuf>,
}

impl Server {
    pub fn new(source_map: SourceMap, workspace_roots: Vec<std::path::PathBuf>) -> Self {
        Self {
            source_map,
            workspace_roots,
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

        self.source_map.set(&mount_name, (&relative_key).as_ref(), text);

        debug!(path=?uri_to_path(&uri).unwrap(), bytes=text.len(), mount=%mount_name, key=%relative_key, "did_open");

        self.recompile(connection);
    }

    pub fn send_diagnostics(&self, severity: DiagnosticSeverity, errors: &[Error], connection: &dyn SendConnection) {
        let mut diagnostics = Vec::new();
        for err in errors {
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
        let uri = Uri::from_str("").unwrap();
        let params = PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        };

        connection.send_publish_diagnostics(params);
    }

    pub fn recompile(&mut self, connection: &dyn SendConnection) {
        let result = swamp::compile_and_analyze(&["crate".to_string(), "main".to_string()], &mut self.source_map);
        match result {
            Ok(program) => {
                self.send_diagnostics(DiagnosticSeverity::ERROR, &program.state.errors, connection);
            }
            Err(err) => {
                error!(?err, "internal error");
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

        self.source_map.set(&mount_name, (&relative_key).as_ref(), &new_text);
        debug!(path=?uri_to_path(&uri).unwrap(), bytes=new_text.len(), mount=%mount_name, key=%relative_key, "did_change");
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
