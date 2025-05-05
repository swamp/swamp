/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_types::types::{BasicType, FramePlacedType, TypedRegister};
use swamp_vm_types::{FrameMemoryAddress, MemorySize};

#[derive(Clone)]
pub struct Context {
    target_register: TypedRegister,
    protected_registers: Vec<u8>,
    comment: String,
}

impl Context {
    pub(crate) fn add_protected_registers(&mut self, registers: &[TypedRegister]) {
        for x in registers {
            self.protected_registers.push(x.index);
        }
    }
}

impl Context {
    pub(crate) fn register_is_protected(&self, register: &TypedRegister) -> bool {
        self.protected_registers
            .iter()
            .any(|reg_index| register.index == *reg_index)
    }

    pub fn add_protected_register(&mut self, register: &TypedRegister) {
        self.protected_registers.push(register.index);
    }
}

impl Context {
    #[must_use]
    pub fn ty(&self) -> &BasicType {
        &self.target_register.ty()
    }
}

impl Context {
    pub(crate) const fn target_register(&self) -> &TypedRegister {
        &self.target_register
    }
    #[must_use]
    pub fn comment(&self) -> &str {
        &self.comment
    }

    /*
    pub(crate) fn move_to_field_index(&self, index: usize) -> Self {
        let offset_item = self
            .target_register
            .final_type()
            .get_field_offset(index)
            .unwrap();
        Self {
            target_register: FramePlacedType::new(
                self.target_register.addr() + offset_item.offset,
                offset_item.ty.clone(),
            ),
            comment: String::new(),
        }
    }
    #[must_use]
    pub fn move_to_optional_tag(&self) -> Self {
        let new_placed_type = self.target_register.move_to_optional_tag();

        Self {
            target_register: new_placed_type,
            comment: String::new(),
        }
    }

    #[must_use]
    pub fn move_to_optional_some_payload(&self) -> Self {
        let new_placed_type = self.target_register.move_to_optional_some_payload();

        Self {
            target_register: new_placed_type,
            comment: String::new(),
        }
    }

     */
}

impl Context {
    #[must_use]
    pub fn new(placed_type: TypedRegister) -> Self {
        Self {
            comment: String::new(),
            target_register: placed_type,
            protected_registers: vec![],
        }
    }

    pub fn addr(&self) -> FrameMemoryAddress {
        self.target_register.addr()
    }
    pub fn target_size(&self) -> MemorySize {
        self.target_register.size()
    }

    #[must_use]
    pub fn final_target_size(&self) -> MemorySize {
        self.target_register.underlying().total_size
    }

    #[must_use]
    pub fn with_target(&self, target_register: TypedRegister, comment: &str) -> Self {
        Self {
            comment: comment.to_string(),
            target_register,
            protected_registers: vec![],
        }
    }
}
