/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod conn;
mod srv;
mod log;

use crate::conn::SendConnection;
use crate::conn::SendConnectionImpl;
use crate::srv::Server;
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::{DidChangeTextDocumentParams, HoverParams, InitializeResult, Uri, WorkspaceFolder};
use lsp_types::{
    DidOpenTextDocumentParams, InitializeParams, ServerCapabilities, TextDocumentSyncKind,
    TextDocumentSyncOptions,
};
use pico_args;
use seq_map::SeqMap;
use source_map_cache::SourceMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tracing::{debug, info};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, Layer};
use walkdir::WalkDir;


fn main() {
    let (lsp_layer, lsp_connection) = log::LspLayer::new();

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(std::io::stderr)
        )
        .with(lsp_layer)
        .init();

    let mut pargs = pico_args::Arguments::from_env();

    let (connection, io_threads) = if let Ok(port) = pargs.value_from_str::<_, u16>("--listen") {
        info!("Starting LSP server on port {}", port);
        Connection::listen(("127.0.0.1", port)).unwrap()
    } else {
        info!("Starting LSP server on stdio");
        Connection::stdio()
    };

    let (init_id, params_value) = connection.initialize_start().unwrap();
    let params: InitializeParams = serde_json::from_value(params_value).unwrap();

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

        // how to synchronize notebook documents (e.g. Jupyter-style .ipynb or VS Code's own notebook formats). when user updates cell it gets reported.
        notebook_document_sync: None,

        // Enables "Selection Range" (`textDocument/selectionRange`). What are those?
        // Default VS Code key bindings:
        //   * Expand selection: Shift + Alt + Right Arrow
        //   * Shrink selection: Shift + Alt + Left Arrow
        selection_range_provider: None,

        // Respond with a Markdown text that can contain Code Blocks.
        // Default VS Code key binding: Ctrl + K Ctrl + I
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),

        // "IntelliSense" completion (`textDocument/completion`)
        // Default VS Code key binding: Ctrl + Space
        completion_provider: None,

        // "Signature Help" (parameter hints when user types) (`textDocument/signatureHelp`)
        // Default VS Code key binding: Ctrl + Shift + Space
        signature_help_provider: None,

        // Go to Definition (`textDocument/definition`)
        // Default VS Code key bindings:
        //   * F12 (Go to Definition)
        //   * Alt + F12 (Peek Definition)
        //   * Ctrl + Click on symbol
        definition_provider: Some(lsp_types::OneOf::Left(true)),

        // "Go to Type Definition" `textDocument/typeDefinition`
        // Default VS Code key binding: Ctrl + Shift + F12
        type_definition_provider: None,

        // "Go to Implementation" `textDocument/implementation`. Interfaces or abstract methods, probably not useful for Swamp.
        // Default VS Code key binding: Ctrl + F12
        implementation_provider: None,

        // "Find References" `textDocument/references`. Very useful.
        // Default VS Code key binding: Shift + F12
        references_provider: None,

        // "Document Symbol Highlights" (what is this?) `textDocument/documentHighlight`.
        document_highlight_provider: None,

        // "Outline view" for the current file only. (textDocument/documentSymbol). Makes sense to implement. Can be Package,Module,Struct,Enum, etc.
        // Default VS Code key binding to reveal document symbols: Ctrl + Shift + O
        document_symbol_provider: None,

        // "Workspace Symbols" `workspace/symbol` (same as Outline View but for the whole workspace). `Ctrl+T`.
        // Default VS Code key binding: Ctrl + T (or Ctrl + P then type "@")
        workspace_symbol_provider: None,

        // Quick fixes, optimize imports, sort members, etc. Probably not that useful for Swamp.
        // Default VS Code key binding: Ctrl + .  (Show Code Actions)
        code_action_provider: None,

        // Enables `textDocument/codeLens`
        code_lens_provider: None,

        // "Format Document" (`textDocument/formatting`)
        // Default VS Code key binding: Shift + Alt + F
        document_formatting_provider: None,

        // "Format Selection" (`textDocument/rangeFormatting`).
        // Default VS Code key binding: Ctrl + K Ctrl + F
        document_range_formatting_provider: None,

        // auto-format when the user type.
        document_on_type_formatting_provider: None,

        // Renaming, could be super useful. `textDocument/rename`
        // Default VS Code key binding: F2
        rename_provider: None,

        // For "hyperlinks", probably mostly for documentation? `textDocument/documentLink`
        document_link_provider: None,

        // "Document Color" support (`textDocument/documentColor`, `textDocument/colorPresentation`)
        color_provider: None,

        // Folding ranges. Also cool to implement.
        // Default VS Code key bindings:
        //   * Fold: Ctrl + Shift + [
        //   * Unfold: Ctrl + Shift + ]
        folding_range_provider: None,

        // "Go to Declaration" (`textDocument/declaration`). what is difference with definition_provider?
        // Default VS Code key binding: Ctrl + F12
        declaration_provider: None,

        // "Command" requests (`workspace/executeCommand`). Can provide a list of commands.
        execute_command_provider: None,

        // combination of workspace_folders, file_operations, execute_command_provider
        workspace: None,

        // The workspace can show cross-file call graphs.
        // who calls a given function, and who that function calls
        // `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls`
        call_hierarchy_provider: None,

        // Enables Semantic Highlighting (reporting back what is variables, function names, types, constants, ...)
        semantic_tokens_provider: None,

        // cross-jumping between files. No idea how that works? it is only for package registry, where it can jump to external files?
        moniker_provider: None,

        // Show simultaneous renames
        linked_editing_range_provider: None,

        // Enables "Inline Values" (`textDocument/inlineValue`). Debugger to show values for variables, etc.
        inline_value_provider: None,

        //  "Inlay Hints" (parameter name hints, type hints) (`textDocument/inlayHint`)
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

    let send_connection = SendConnectionImpl { connection };

    let send_connection_arc = Arc::new(send_connection);

    if let Ok(mut conn_opt) = lsp_connection.lock() {
        *conn_opt = Some(send_connection_arc.clone());
    }

    info!("LSP server initialized and ready to start fetching swamp files");
    let source_map_cache = first_init(&params);

    let mut server = Server::new(source_map_cache);

    for msg in &send_connection_arc.connection.receiver {
        match msg {
            Message::Request(req) => {
                if send_connection_arc.connection.handle_shutdown(&req).unwrap() {
                    break;
                }
                handle_request(req, &send_connection_arc, &mut server);
            }
            Message::Notification(notif) => {
                handle_notification(notif, &send_connection_arc, &mut server);
            }
            Message::Response(_) => {
                // no custom requests, so ignore.
            }
        }
    }

    io_threads.join().unwrap();
}

fn handle_request(req: Request, send_connection: &SendConnectionImpl, server: &mut Server) {
    let result = if req.method.as_str() == "textDocument/hover" {
        let params: HoverParams = serde_json::from_value(req.params.clone()).unwrap();
        server.on_hover(&params, send_connection)
    } else {
        let resp = Response::new_err(
            req.id,
            ErrorCode::MethodNotFound as i32,
            "Method not implemented".to_string(),
        );
        send_connection
            .connection
            .sender
            .send(Message::Response(resp))
            .unwrap();
        return;
    };

    let resp = Response::new_ok(req.id, result);
    send_connection
        .connection
        .sender
        .send(Message::Response(resp))
        .unwrap();
}

fn handle_notification(notif: Notification, connection: &SendConnectionImpl, server: &mut Server) {
    match notif.method.as_str() {
        "textDocument/didOpen" => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(notif.params).unwrap();
            server.on_did_open(&params, connection);
        }
        "textDocument/didChange" => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(notif.params).unwrap();
            server.on_did_change(&params, connection);
        }
        _ => {}
    }
}

fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    if uri.scheme().map(|s| s.as_str()) == Some("file") {
        // Get the path component and convert to string
        let path_str = uri.path().as_str();
        Some(PathBuf::from(path_str))
    } else {
        None
    }
}

fn to_relative_key(workspace_root: &Path, path: &Path) -> Result<String, ()> {
    let rel = path.strip_prefix(workspace_root).map_err(|_| ())?;

    let s = rel
        .to_string_lossy()
        .replace(std::path::MAIN_SEPARATOR, "/");
    Ok(s)
}

fn first_init(params: &InitializeParams) -> SourceMap {
    if let Some(folder_list) = &params.workspace_folders {
        let roots = extract_workspace_roots(folder_list);

        // TODO: For now, just support the first root:
        let main_root = &roots[0];
        let scripts = main_root.join("scripts");
        let packages = main_root.join("packages");
        let mut mounts = SeqMap::new();

        let _ = mounts.insert("crate".to_string(), scripts.clone());
        let _ = mounts.insert("packages".to_string(), packages.clone());

        let mut source_map_cache = SourceMap::new(&mounts).unwrap();

        let script_files = collect_swamp_files(&scripts);
        debug!(count=%script_files.len(), "found script files");
        for file in script_files {
            source_map_cache.read_file(&file, "crate").unwrap();
        }

        let package_files = collect_swamp_files(&packages);
        debug!(count=%package_files.len(), "found package files");
        for file in package_files {
            source_map_cache.read_file(&file, "packages").unwrap();
        }
        source_map_cache
    } else {
        panic!("no workspace folders");
    }
}

fn extract_workspace_roots(folder_list: &[WorkspaceFolder]) -> Vec<PathBuf> {
    let mut roots = Vec::new();

    for WorkspaceFolder { uri, .. } in folder_list {
        if let Some(path) = uri_to_path(uri) {
            roots.push(path);
        }
    }

    roots
}

fn collect_swamp_files(root: &PathBuf) -> Vec<PathBuf> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| {
            entry.file_type().is_file()
                && entry.path().extension().is_some_and(|ext| ext == "swamp")
        })
        .map(walkdir::DirEntry::into_path)
        .collect()
}
