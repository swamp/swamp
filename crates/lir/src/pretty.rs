use crate::{Block, Inst};

pub fn print_inst(inst: &Inst) -> String {
    let core = format!("{:?}", inst.op);
    #[cfg(feature = "debug-info")]
    {
        let mut tail = String::new();
        if let Some(c) = inst.dbg.comment.as_deref() {
            tail.push_str("  ; ");
            tail.push_str(c);
        }
        if let Some(o) = inst.dbg.origin.as_deref() {
            if tail.is_empty() {
                tail.push_str("  ; ");
            } else {
                tail.push_str(" | ");
            }
            tail.push_str(o);
        }
        if let Some(src) = &inst.dbg.src {
            let sep = if tail.is_empty() { "  ; " } else { " | " };
            tail.push_str(sep);
            use std::fmt::Write;
            let _ = write!(tail, "{}:{}:{}", src.file, src.line, src.col);
        }
        return format!("{core}{tail}");
    }
    core
}

pub fn print_block_header(b: &Block) -> String {
    #[cfg(feature = "debug-info")]
    {
        if let Some(label) = b.dbg.label.as_deref() {
            if let Some(comment) = b.dbg.comment.as_deref() {
                return format!("{label}:    ; {comment}");
            }
            return format!("{label}:");
        }
    }
    format!("B{}:", b.id.0)
}
