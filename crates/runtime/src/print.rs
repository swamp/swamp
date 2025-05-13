use crate::err_wrt::StderrWriter;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_vm::Vm;
use swamp_vm::host::HostArgs;

#[derive(Debug, Default, Clone, Copy)]
struct TestContext {}

fn print_fn(ctx: &mut TestContext, mut args: HostArgs) {
    let output = args.get_str();
    eprintln!("PRINT! {output}");
    let mut stderr_adapter = StderrWriter;
    /*
       let frame = args.get_heap_raw;
       let heap = args.heap_memory();

       swamp_vm_pretty_print::print_value(
           &mut stderr_adapter,
           frame,
           &heap,
           StackMemoryAddress(0),
           &ctx.simulation_layout,
           "Simulation",
       )
           .unwrap();

    */
}

pub fn register_print(mut vm: &mut Vm, context: TestContext) {
    {
        swamp_vm_host::register_context_aware::<TestContext, _>(
            &mut vm,
            1,
            &Rc::new(RefCell::new(context)),
            print_fn,
        );
    }
}
