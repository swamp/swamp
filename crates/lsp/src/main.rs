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

        // how to synchronize notebook documents (e.g. Jupyter-style .ipynb or VS Code’s own notebook formats). when user updates cell it gets reported.
        notebook_document_sync: None,

        // Enables “Selection Range” (`textDocument/selectionRange`). What are those?
        // Default VS Code key bindings:
        //   • Expand selection: Shift + Alt + Right Arrow
        //   • Shrink selection: Shift + Alt + Left Arrow
        selection_range_provider: None,

        // Respond with a Markdown text that can contain Code Blocks.
        // Default VS Code key binding: Ctrl + K Ctrl + I
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),

        // "IntelliSense" completion (`textDocument/completion`)
        // Default VS Code key binding: Ctrl + Space
        completion_provider: None,

        // “Signature Help” (parameter hints when user types) (`textDocument/signatureHelp`)
        // Default VS Code key binding: Ctrl + Shift + Space
        signature_help_provider: None,

        // Go to Definition (`textDocument/definition`)
        // Default VS Code key bindings:
        //   • F12 (Go to Definition)
        //   • Alt + F12 (Peek Definition)
        //   • Ctrl + Click on symbol
        definition_provider: Some(lsp_types::OneOf::Left(true)),

        // “Go to Type Definition” `textDocument/typeDefinition`
        // Default VS Code key binding: Ctrl + Shift + F12
        type_definition_provider: None,

        // “Go to Implementation” `textDocument/implementation`. Interfaces or abstract methods, probably not useful for Swamp.
        // Default VS Code key binding: Ctrl + F12
        implementation_provider: None,

        // “Find References” `textDocument/references`. Very useful.
        // Default VS Code key binding: Shift + F12
        references_provider: None,

        // “Document Symbol Highlights” (what is this?) `textDocument/documentHighlight`.
        document_highlight_provider: None,

        // “Outline view” for the current file only. (textDocument/documentSymbol). Makes sense to implement. Can be Package,Module,Struct,Enum, etc.
        // Default VS Code key binding to reveal document symbols: Ctrl + Shift + O
        document_symbol_provider: None,

        // “Workspace Symbols” `workspace/symbol` (same as Outline View but for the whole workspace). `Ctrl+T`.
        // Default VS Code key binding: Ctrl + T (or Ctrl + P then type “@”)
        workspace_symbol_provider: None,

        // Quick fixes, optimize imports, sort members, etc. Probably not that useful for Swamp.
        // Default VS Code key binding: Ctrl + .  (Show Code Actions)
        code_action_provider: None,

        // Enables `textDocument/codeLens`
        code_lens_provider: None,

        // “Format Document” (`textDocument/formatting`)
        // Default VS Code key binding: Shift + Alt + F
        document_formatting_provider: None,

        // “Format Selection” (`textDocument/rangeFormatting`).
        // Default VS Code key binding: Ctrl + K Ctrl + F
        document_range_formatting_provider: None,

        // auto-format when the user type.
        document_on_type_formatting_provider: None,

        // Renaming, could be super useful. `textDocument/rename`
        // Default VS Code key binding: F2
        rename_provider: None,

        // For "hyperlinks", probably mostly for documentation? `textDocument/documentLink`
        document_link_provider: None,

        // "Document Color” support (`textDocument/documentColor`, `textDocument/colorPresentation`)
        color_provider: None,

        // Folding ranges. Also cool to implement.
        // Default VS Code key bindings:
        //   • Fold: Ctrl + Shift + [
        //   • Unfold: Ctrl + Shift + ]
        folding_range_provider: None,

        // “Go to Declaration” (`textDocument/declaration`). what is difference with definition_provider?
        // Default VS Code key binding: Ctrl + F12
        declaration_provider: None,

        // “Command” requests (`workspace/executeCommand`). Can provide a list of commands.
        execute_command_provider: None,

        // combination of workspace_folders, file_operations, execute_command_provider
        workspace: None,

        // The workspace can show cross‐file call graphs.
        // who calls a given function, and who that function calls
        // `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls`
        call_hierarchy_provider: None, 

        // Enables Semantic Highlighting (reporting back what is variables, function names, types, constants, ...)
        semantic_tokens_provider: None,

        // cross‐jumping between files. No idea how that works? it is only for package registry, where it can jump to external files?
        moniker_provider: None,

        // Show simultaneous renames
        linked_editing_range_provider: None,

        // Enables “Inline Values” (`textDocument/inlineValue`). Debugger to show values for variables, etc.
        inline_value_provider: None,

        //  “Inlay Hints” (parameter name hints, type hints) (`textDocument/inlayHint`)
        inlay_hint_provider: None,

        // interFileDependencies, workspaceDiagnostics
        // Problems panel key binding: Ctrl + Shift + M
        diagnostic_provider: None,
        
        // We shouldn't implement experimental stuff.
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
