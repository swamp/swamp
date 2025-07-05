/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::conn::SendConnection;
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams, Hover,
    HoverContents, HoverParams, MarkupContent, MarkupKind, Position, PublishDiagnosticsParams,
    Range, Uri,
};
use seq_map::SeqMap;
use source_map_cache::SourceMap;

pub struct Server {
    pub source_map: SourceMap,
}

impl Server {
    pub fn new() -> Self {
        let mounts = SeqMap::new();
        Self {
            source_map: SourceMap::new(&mounts).unwrap(),
        }
    }

    pub(crate) fn on_did_open(
        &mut self,
        params: &DidOpenTextDocumentParams,
        connection: &dyn SendConnection,
    ) {
        let uri = params.text_document.uri.clone();
        let text = &params.text_document.text;

        self.source_map
            .add_manual_no_id("crate", uri.to_string().as_ref(), text);

        connection.log("did_open happened");
    }

    pub(crate) fn on_did_change(
        &mut self,
        params: &DidChangeTextDocumentParams,
        connection: &dyn SendConnection,
    ) {
        let uri = params.text_document.uri.clone();
        let new_text = params.content_changes[0].text.clone();
        connection.log("did_change happened");
    }

    pub(crate) fn on_hover(&self, params: &HoverParams, connection: &dyn SendConnection) -> Hover {
        connection.log("on_over happened");
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
