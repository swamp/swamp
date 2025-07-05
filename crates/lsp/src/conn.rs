/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use lsp_server::{Connection, Message, Notification};
use lsp_types::{LogMessageParams, MessageType, PublishDiagnosticsParams, ShowMessageParams};

pub trait SendConnection {
    fn send_publish_diagnostics(&self, params: PublishDiagnosticsParams);
    fn send_window_show_message(&self, params: ShowMessageParams);
    fn send_log_message(&self, params: LogMessageParams);
    fn log(&self, s: &str);
}

pub struct SendConnectionImpl {
    pub(crate) connection: Connection,
}

impl SendConnection for SendConnectionImpl {
    fn send_publish_diagnostics(&self, params: PublishDiagnosticsParams) {
        let notif = Notification::new("textDocument/publishDiagnostics".into(), params);
        self.connection
            .sender
            .send(Message::Notification(notif))
            .unwrap();
    }

    // User visible show message
    fn send_window_show_message(&self, params: ShowMessageParams) {
        let alert = Notification::new("window/showMessage".into(), params);
        self.connection
            .sender
            .send(Message::Notification(alert))
            .unwrap();
    }

    fn send_log_message(&self, params: LogMessageParams) {
        let notif = Notification::new("window/logMessage".into(), params);
        self.connection
            .sender
            .send(Message::Notification(notif))
            .unwrap();
    }

    fn log(&self, s: &str) {
        self.send_log_message(LogMessageParams {
            typ: MessageType::LOG,
            message: s.to_string(),
        });
    }
}
