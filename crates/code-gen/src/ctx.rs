/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_types::types::TypedRegister;

#[derive(Clone, Default)]
pub struct Context {
    protected_registers: Vec<u8>,
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
    pub(crate) fn add_protected_registers(&mut self, registers: &[TypedRegister]) {
        for x in registers {
            self.protected_registers.push(x.index);
        }
    }
}
