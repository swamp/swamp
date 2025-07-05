/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub struct StderrWriter;

use std::fmt::{self, Write as FmtWrite};
use std::io::{self, Write as IoWrite};

impl FmtWrite for StderrWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let mut stderr = io::stderr();

        stderr.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}
