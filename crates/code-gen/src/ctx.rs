/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_types::types::{BasicType, FramePlacedType};
use swamp_vm_types::{FrameMemoryAddress, MemoryOffset, MemorySize};

pub struct Context {
    placed_type: FramePlacedType,
    comment: String,
}

impl Context {}

impl Context {}

impl Context {
    pub fn ty(&self) -> &BasicType {
        &self.placed_type.ty()
    }
}

impl Context {}

impl Context {
    pub(crate) fn comment(&self) -> &str {
        &self.comment
    }
}

impl Context {
    pub(crate) const fn target(&self) -> &FramePlacedType {
        &self.placed_type
    }
}

impl Context {
    pub(crate) fn with_offset(&self, offset: MemoryOffset, ty: BasicType) -> Self {
        Self {
            placed_type: self.placed_type.move_with_offset(offset, ty),
            comment: "".to_string(),
        }
    }

    pub(crate) fn move_to_field_index(&self, index: usize) -> Self {
        let offset_item = self.placed_type.ty().get_field_offset(index).unwrap();
        Self {
            placed_type: FramePlacedType::new(
                self.placed_type.addr() + offset_item.offset,
                offset_item.ty.clone(),
            ),
            comment: "".to_string(),
        }
    }
    pub fn move_to_optional_tag(&self) -> Self {
        let new_placed_type = self.placed_type.move_to_optional_tag();

        Self {
            placed_type: new_placed_type,
            comment: "".to_string(),
        }
    }

    pub fn move_to_optional_some_payload(&self) -> Self {
        let new_placed_type = self.placed_type.move_to_optional_some_payload();

        Self {
            placed_type: new_placed_type,
            comment: "".to_string(),
        }
    }
}

impl Context {
    #[must_use]
    pub fn new(placed_type: FramePlacedType) -> Self {
        Self {
            comment: String::new(),
            placed_type,
        }
    }

    pub const fn addr(&self) -> FrameMemoryAddress {
        self.placed_type.addr()
    }
    pub const fn target_size(&self) -> MemorySize {
        self.placed_type.size()
    }

    #[must_use]
    pub fn with_target(&self, placed_type: FramePlacedType, comment: &str) -> Self {
        Self {
            comment: comment.to_string(),
            placed_type,
        }
    }
}
