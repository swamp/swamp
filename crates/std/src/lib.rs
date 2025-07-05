/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod print;

#[must_use]
pub fn std_text() -> String {
    "external 1 fn print(output: String)".to_string()
}
