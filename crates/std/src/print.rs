use swamp_vm::host::HostArgs;

pub fn print_fn(mut args: HostArgs) {
    let output = args.get_str();
    eprintln!("print: {output}");
}
