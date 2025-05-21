use std::cell::RefCell;
use std::rc::Rc;
use swamp_modules::prelude::Modules;
use swamp_vm::Vm;
use swamp_vm::host::HostArgs;

fn print_fn<C>(_ctx: &mut C, mut args: HostArgs) {
    let output = args.get_str();
    eprintln!("print: {output}");
}

pub fn register_print<C: 'static>(vm: &mut Vm, modules: &Modules, context: C) {
    let maybe_std = modules.get(&["std".to_string()]);
    if let Some(std_module) = maybe_std {
        let print_fn_id = std_module
            .symbol_table
            .get_external_function_declaration("print")
            .unwrap()
            .id;

        swamp_vm_host::register_context_aware::<C, _>(
            vm,
            print_fn_id as u16,
            &Rc::new(RefCell::new(context)),
            print_fn,
        );
    } else {
        eprintln!("print() isn't used, this is very unusual");
    }
}
