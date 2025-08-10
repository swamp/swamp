use swamp_vm::{Vm, VmSetup};

#[test]
fn create() {
    let setup = VmSetup {
        stack_memory_size: 32 * 1024,
        heap_memory_size: 128 * 1024,
        constant_memory: vec![],
        debug_stats_enabled: false,
        debug_opcodes_enabled: false,
        debug_operations_enabled: false,
    };
    let vm = Vm::new(vec![], setup);
}
