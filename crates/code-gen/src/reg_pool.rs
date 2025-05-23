use swamp_vm_types::types::{TypedRegister, VmType};

#[derive(Debug)]
pub struct TempRegister {
    pub(crate) register: TypedRegister,
}

impl TempRegister {
    pub(crate) const fn register(&self) -> &TypedRegister {
        &self.register
    }
    #[must_use]
    pub const fn addressing(&self) -> u8 {
        self.register.addressing()
    }
}

#[derive(Debug)]
pub struct RegisterInfo {
    pub index: u8,
}

#[derive(Debug)]
pub struct HwmTempRegisterPool {
    start_index: u8,
    capacity: u8,
    num_allocated: u8,
    /// Track registers that are 'pinned' and shouldn't be reused
    /// Pinned registers might be used for specific, ongoing operations.
    pinned_registers: Vec<u8>,
    next_pinned_index: u8,
}

impl HwmTempRegisterPool {
    /// # Panics
    ///
    #[must_use]
    pub fn new(start: u8, count: usize) -> Self {
        if count == 0 {
            return Self {
                start_index: start,
                capacity: 0,
                num_allocated: 0,
                pinned_registers: Vec::new(),
                next_pinned_index: 200,
            };
        }
        assert!(
            count <= u8::MAX as usize + 1,
            "Register count too large for u8 capacity"
        );
        assert!(
            start.checked_add((count - 1) as u8).is_some(),
            "Register index range would overflow u8"
        );

        Self {
            start_index: start,
            capacity: count as u8,
            num_allocated: 0, // Initially, no registers are allocated
            pinned_registers: Vec::new(),
            next_pinned_index: 200,
        }
    }

    /// # Panics
    /// if out of registers
    pub fn allocate(&mut self, ty: VmType, comment: &str) -> TempRegister {
        assert!(
            (self.num_allocated < self.capacity),
            "HwmTempRegisterPool: Out of temporary registers. Requested for: '{comment}'",
        );

        let register_index = self.start_index + self.num_allocated;
        self.num_allocated += 1;

        TempRegister {
            register: TypedRegister {
                index: register_index,
                ty,
                comment: comment.to_string(),
            },
        }
    }

    /// Pin a register so it won't be reused even after `restore_to_mark`
    pub fn pin_register(&mut self, reg: &TypedRegister) {
        if !self.pinned_registers.contains(&reg.index) {
            self.pinned_registers.push(reg.index);
        }
    }

    /// Unpin a register when it's safe to reuse
    pub fn unpin_register(&mut self, reg: &TypedRegister) {
        if let Some(pos) = self.pinned_registers.iter().position(|&i| i == reg.index) {
            self.pinned_registers.remove(pos);
        }
    }

    /// Allocate a register specifically for long-lived registers
    ///
    /// This is primarily needed for call logic when registers hold
    /// copies for registers that are used for argument registers (r1-r5).
    /// An alternative approach, such as keeping the high watermark, was found to risk
    /// register exhaustion.
    pub fn allocate_pinned_register(&mut self, ty: VmType, comment: &str) -> TempRegister {
        let register_index = self.next_pinned_index;
        self.next_pinned_index += 1;

        let reg = TempRegister {
            register: TypedRegister {
                index: register_index,
                ty,
                comment: comment.to_string(),
            },
        };

        self.pin_register(reg.register());

        reg
    }

    #[must_use]
    pub const fn save_mark(&self) -> u8 {
        self.num_allocated
    }

    /// # Panics
    ///
    pub fn restore_to_mark(&mut self, mark: u8) {
        // TODO: add a check that we aren't using marks in the pinned range
        assert!(
            (mark <= self.num_allocated),
            "HwmTempRegisterPool: Invalid mark {} provided. Current allocation count is {}.",
            mark,
            self.num_allocated
        );

        self.num_allocated = mark;
    }
}

pub struct RegisterPool {
    pub start_index: u8,
    pub end_index: u8,
    pub current_index: u8,
}

impl RegisterPool {
    #[must_use]
    pub const fn new(start: u8, count: u8) -> Self {
        Self {
            start_index: start,
            end_index: start + count,
            current_index: start,
        }
    }

    /// # Panics
    ///
    pub fn alloc_register(&mut self, ty: VmType, comment: &str) -> TypedRegister {
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
            comment: comment.to_string(),
        }
    }
}
