use crate::conn::{SendConnection, SendConnectionImpl};
use std::sync::{Arc, Mutex};
use tracing_subscriber::Layer;

// Custom layer that sends filtered tracing logs to LSP connection
pub struct LspLayer {
    connection: Arc<Mutex<Option<Arc<SendConnectionImpl>>>>,
}

impl LspLayer {
    pub(crate) fn new() -> (Self, Arc<Mutex<Option<Arc<SendConnectionImpl>>>>) {
        let connection = Arc::new(Mutex::new(None));
        (Self { connection: connection.clone() }, connection)
    }
}

impl<S> Layer<S> for LspLayer
where
    S: tracing::Subscriber,
{
    fn on_event(&self, event: &tracing::Event<'_>, _ctx: tracing_subscriber::layer::Context<'_, S>) {
        // Only send logs from `swamp_lsp` crate to LSP connection
        if event.metadata().target().starts_with("swamp_lsp") {
            if let Ok(conn_opt) = self.connection.try_lock() {
                if let Some(conn) = conn_opt.as_ref() {
                    let mut visitor = LogVisitor::new();
                    event.record(&mut visitor);
                    let formatted_message = visitor.format_message();
                    if !formatted_message.is_empty() {
                        conn.log(&formatted_message);
                    }
                }
            }
        }
    }
}

struct LogVisitor {
    message: String,
    fields: Vec<String>,
}

impl LogVisitor {
    fn new() -> Self {
        Self {
            message: String::new(),
            fields: Vec::new(),
        }
    }

    fn format_message(&self) -> String {
        if self.fields.is_empty() {
            self.message.clone()
        } else {
            format!("{} {}", self.message, self.fields.join(" "))
        }
    }
}

impl tracing::field::Visit for LogVisitor {
    fn record_i64(&mut self, field: &tracing::field::Field, value: i64) {
        self.fields.push(format!("{}={}", field.name(), value));
    }

    fn record_u64(&mut self, field: &tracing::field::Field, value: u64) {
        self.fields.push(format!("{}={}", field.name(), value));
    }

    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        if field.name() == "message" {
            self.message = value.to_string();
        } else {
            self.fields.push(format!("{}={}", field.name(), value));
        }
    }

    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        if field.name() == "message" {
            self.message = format!("{:?}", value);
        } else {
            self.fields.push(format!("{}={:?}", field.name(), value));
        }
    }
}
