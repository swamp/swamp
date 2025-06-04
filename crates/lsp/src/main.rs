use lsp_server::{Connection, ErrorCode, Message, Notification, Response};
use lsp_types::Uri;
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidOpenTextDocumentParams, InitializeParams, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncKind,
    TextDocumentSyncOptions,
};
use lsp_types::{DidChangeTextDocumentParams, InitializeResult};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

fn main() {
    let (connection, io_threads) = Connection::stdio();

    let (init_id, params_value) = connection.initialize_start().unwrap();
    let _params: InitializeParams = serde_json::from_value(params_value).unwrap();

    let caps = ServerCapabilities {
        position_encoding: None,
        text_document_sync: Some(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL), // FULL is probably the easiest to implement for now
                save: None,
                will_save: None, // The user should always be able to save when they want
                will_save_wait_until: None, // The user should always be able to save when they want
            }
            .into(),
        ),
        notebook_document_sync: None, // how to synchronize notebook documents (e.g. Jupyter-style .ipynb or VS Code’s own notebook formats). when user updates cell it gets reported.
        selection_range_provider: None, // Enables “Selection Range” (`textDocument/selectionRange`). What are those?
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)), // Respond with a Markdown text that can contain Code Blocks.
        completion_provider: None, // IntelliSense” completion (`textDocument/completion`)
        signature_help_provider: None, // “Signature Help” (parameter hints when user types) (`textDocument/signatureHelp`)
        definition_provider: Some(lsp_types::OneOf::Left(true)), // Go to Definition
        type_definition_provider: None, // Enables “Go to Type Definition” `textDocument/typeDefinition`
        implementation_provider: None, // “Go to Implementation” `textDocument/implementation`.  interfaces or abstract methods, probably not useful for Swamp.
        references_provider: None,     // “Find References” `textDocument/references`. Very useful.
        document_highlight_provider: None, // “Document Symbol Highlights” (what is this?) `textDocument/documentHighlight`.
        document_symbol_provider: None, // “Outline view” for the current file only. (textDocument/documentSymbol). Makes sense to implement. Can be Package,Module,Struct,Enum, etc
        workspace_symbol_provider: None, // “Workspace Symbols” `workspace/symbol` (same as Outline View but for the whole workspace). `Ctrl+T`.
        code_action_provider: None, // Quick fixes, optimize imports, sort members, etc. Probably not that useful for Swamp.
        code_lens_provider: None,   // Enables `textDocument/codeLens`
        document_formatting_provider: None, // “Format Document” (`textDocument/formatting`)
        document_range_formatting_provider: None, // “Format Selection” (`textDocument/rangeFormatting`).
        document_on_type_formatting_provider: None, // auto-format when the user type.
        rename_provider: None,                    // Renaming, could be super useful
        document_link_provider: None, // For "hyperlinks", probably mostly for documentation? `textDocument/documentLink`
        color_provider: None, // "Document Color” support (`textDocument/documentColor`, `textDocument/colorPresentation`)
        folding_range_provider: None, // Also cool to implement.
        declaration_provider: None, // “Go to Declaration” (`textDocument/declaration`). what is difference with definition_provider?
        execute_command_provider: None, // “Command” requests (`workspace/executeCommand`). Can provide a list of commands.
        workspace: None, // combination of workspace_folders, file_operations, execute_command_provider
        call_hierarchy_provider: None, // The workspace can show cross‐file call graphs. What is that?
        semantic_tokens_provider: None, // Enables Semantic Highlighting (reporting back what is variables, function names, types, constants, ...)
        moniker_provider: None,         // cross‐jumping between files. No idea how that works?
        linked_editing_range_provider: None, // Show simultaneous renames
        inline_value_provider: None, // Enables “Inline Values” (`textDocument/inlineValue`). Debugger to show values for variables, etc.
        inlay_hint_provider: None, //  “Inlay Hints” (parameter name hints, type hints) (`textDocument/inlayHint`)
        diagnostic_provider: None, // interFileDependencies, workspaceDiagnostics
        experimental: None,
    };

    let init_result = InitializeResult {
        capabilities: caps,
        server_info: None,
    };
    connection
        .initialize_finish(init_id, serde_json::to_value(init_result).unwrap())
        .unwrap();

    let open_docs: Arc<Mutex<HashMap<Uri, String>>> = Arc::new(Mutex::new(HashMap::new()));

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req).unwrap() {
                    break;
                }
                let resp = Response::new_err(
                    req.id,
                    ErrorCode::MethodNotFound as i32,
                    "Method not implemented".to_string(),
                );
                connection.sender.send(Message::Response(resp)).unwrap();
            }
            Message::Notification(notif) => {
                handle_notification(notif, &Arc::clone(&open_docs), &connection);
            }
            Message::Response(_) => {
                // no custom requests, so ignore.
            }
        }
    }

    io_threads.join().unwrap();
}

fn handle_notification(
    notif: Notification,
    open_docs: &Arc<Mutex<HashMap<Uri, String>>>,
    connection: &Connection,
) {
    match notif.method.as_str() {
        "textDocument/didOpen" => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(notif.params).unwrap();
            let uri = params.text_document.uri.clone();
            let text = params.text_document.text;
            open_docs.lock().unwrap().insert(uri.clone(), text.clone());
            publish_diagnostics(uri, &text, connection);
        }
        "textDocument/didChange" => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(notif.params).unwrap();
            let uri = params.text_document.uri.clone();
            let new_text = params.content_changes[0].text.clone();
            open_docs
                .lock()
                .unwrap()
                .insert(uri.clone(), new_text.clone());
            publish_diagnostics(uri, &new_text, connection);
        }
        _ => {}
    }
}

fn publish_diagnostics(uri: Uri, text: &str, connection: &Connection) {
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
                message: "Found “error” in code".into(),
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
    let notif = Notification::new("textDocument/publishDiagnostics".into(), params);
    connection
        .sender
        .send(Message::Notification(notif))
        .unwrap();
}
