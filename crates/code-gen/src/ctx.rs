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
    pub(crate) const fn register(&self) -> &TypedRegister {
        &self.target_register
    }
    #[must_use]
    pub fn comment(&self) -> &str {
        &self.comment
    }
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
