use crate::cache::TypeCache;
use std::fmt;
use std::fmt::Write;

pub fn print_types(f: &mut dyn Write, cache: &TypeCache) -> fmt::Result {
    for (id, ty) in &cache.type_id_to_type {
        writeln!(f, "{ty}")?;
    }

    Ok(())
}
