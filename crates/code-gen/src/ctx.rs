/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

#[derive(Clone, Default)]
pub struct Context {
    pub comment: String,
}

impl Context {
    pub fn new() -> Self {
        Self {
            comment: String::new(),
        }
    }

    pub fn with_comment(self, comment: &str) -> Self {
        Self {
            comment: comment.to_string(),
        }
    }
    pub(crate) fn comment(&self) -> &str {
        &self.comment
    }
}
