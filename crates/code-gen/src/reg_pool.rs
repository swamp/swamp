use std::backtrace::Backtrace;
use std::hint::assert_unchecked;
use swamp_vm_types::types::{TypedRegister, VmType};
use tracing::info;

#[derive(Debug)]
pub struct TempRegister {
    pub(crate) register: TypedRegister,
}

impl TempRegister {
    pub(crate) fn register(&self) -> &TypedRegister {
        &self.register
    }
    pub fn addressing(&self) -> u8 {
        self.register.addressing()
    }
}

#[derive(Debug)]
pub struct RegisterInfo {
    pub index: u8,
}

pub struct TempRegisterPool {
    free_registers: Vec<RegisterInfo>,
}

impl TempRegisterPool {
    pub fn new(start: u8, count: usize) -> Self {
        let mut registers = Vec::new();
        for index in start..(start + count as u8) {
            registers.push(RegisterInfo { index });
        }

        Self {
            free_registers: registers,
        }
    }
    pub fn allocate(&mut self, ty: VmType) -> TempRegister {
        assert!(!self.free_registers.is_empty(), "out of temp registers");
        let free_reg_info = self.free_registers.pop().unwrap();

        info!(?free_reg_info, "lending out temp reg");
        let backtrace = Backtrace::capture();
        println!("Current call stack:\n{}", backtrace);

        TempRegister {
            register: TypedRegister {
                index: free_reg_info.index,
                ty,
            },
        }
    }

    pub fn free(&mut self, reg: TempRegister) {
        assert!(
            !self
                .free_registers
                .iter()
                .any(|info| info.index == reg.register.index)
        );

        let kind = reg.register().underlying().kind;
        info!(?kind, "free temp reg");

        self.free_registers.push(RegisterInfo {
            index: reg.register.index,
        });
    }

    pub(crate) fn free_multiple(&mut self, registers: Vec<TempRegister>) {
        for temp in registers {
            self.free(temp)
        }
    }
}

pub struct RegisterPool {
    pub start_index: u8,
    pub end_index: u8,
    pub current_index: u8,
}

impl RegisterPool {
    pub fn new(start: u8, count: u8) -> Self {
        Self {
            start_index: start,
            end_index: start + count,
            current_index: start,
        }
    }
    pub fn alloc_register(&mut self, ty: VmType) -> TypedRegister {
        assert!(
            self.current_index + 1 < self.end_index,
            "out of registers {} {}",
            self.current_index,
            self.end_index
        );
        let allocated_register = self.current_index;
        self.current_index += 1;
        TypedRegister {
            index: allocated_register,
            ty,
        }
    }
}
