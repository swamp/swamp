/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_types::{FrameMemoryAddress, FrameMemoryRegion, MemoryOffset, MemorySize};

pub struct Context {
    target_info: FrameMemoryRegion,
    comment: String,
}

impl Context {
    pub(crate) fn comment(&self) -> &str {
        &self.comment
    }
}

impl Context {
    pub(crate) const fn target(&self) -> FrameMemoryRegion {
        self.target_info
    }
}

impl Context {
    pub(crate) fn with_offset(&self, offset: MemoryOffset, memory_size: MemorySize) -> Self {
        Self {
            target_info: FrameMemoryRegion {
                addr: self.addr().add_offset(offset),
                size: memory_size,
            },
            comment: "".to_string(),
        }
    }
}

impl Context {
    #[must_use]
    pub fn new(target_info: FrameMemoryRegion) -> Self {
        Self {
            target_info,
            comment: String::new(),
        }
    }

    pub const fn addr(&self) -> FrameMemoryAddress {
        self.target_info.addr
    }
    pub const fn target_size(&self) -> MemorySize {
        self.target_info.size
    }

    #[must_use]
    pub fn with_target(&self, target_info: FrameMemoryRegion, comment: &str) -> Self {
        Self {
            target_info,
            comment: comment.to_string(),
        }
    }

    #[must_use]
    pub fn create_scope(&self) -> Self {
        Self {
            target_info: self.target_info,
            comment: self.comment.clone(),
        }
    }

    #[must_use]
    pub fn create_function_scope(&self, return_target: FrameMemoryRegion, comment: &str) -> Self {
        Self {
            target_info: self.target_info,
            comment: comment.to_string(),
        }
    }
}
