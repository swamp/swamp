/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::cache::TypeCache;
use std::fmt;
use std::fmt::Write;

pub fn print_types(f: &mut dyn Write, cache: &TypeCache) -> fmt::Result {
    for (_id, ty) in &cache.type_id_to_type {
        writeln!(f, "{ty}")?;
    }

    Ok(())
}
