use fentext_ui::{Input, Tui};
use std::env::current_dir;
use std::io::Write;
use std::io::stderr;
use std::path::Path;
use std::thread;
use std::time::Duration;
use swamp::prelude::{
    CodeGenAndVmResult, GenFunctionInfo, HostArgs, HostFunctionCallback, RunConstantsOptions,
    RunOptions, SourceMapWrapper, StackMemoryAddress, compile_codegen_and_create_vm, layout_type,
    print_value, run_first_time, run_function_with_debug,
};
use swamp_std::print::print_fn;

pub struct Application {
    pub canvas: Tui,
    tick_count: usize,
}

impl Application {
    pub fn debug(&self) {
        self.canvas.move_to(0, 0);
        // self.canvas.write(&format!("tick: {}", self.tick_count));
    }

    pub fn external_move_cursor(&mut self, mut host_args: HostArgs) {
        let x = host_args.get_i32();
        let y = host_args.get_i32();
        self.canvas.move_to(x as u16, y as u16);
    }

    pub fn external_write(&mut self, mut host_args: HostArgs) {
        let str = host_args.get_str();
        self.canvas.write(str);
    }
}

pub struct FenTextSwamp {
    pub runtime_result: CodeGenAndVmResult,
    pub tick_fn: GenFunctionInfo,
    pub simulation_value_addr: u32,
    pub safe_stack_start_addr: u32,
}

impl FenTextSwamp {
    pub fn new(application: &mut Application) -> Self {
        let mut runtime_result = Self::compile();
        Self::run_once(&mut runtime_result, application);
        let (tick_fn, simulation_value_addr, safe_stack_start_addr) =
            Self::boot(&mut runtime_result, application);

        Self {
            runtime_result,
            tick_fn,
            simulation_value_addr,
            safe_stack_start_addr,
        }
    }

    pub fn tick(&mut self, application: &mut Application) {
        let run_options = RunOptions {
            debug_stats_enabled: false,
            debug_opcodes_enabled: false,
            debug_info: &self.runtime_result.code_gen_result.debug_info,
            source_map_wrapper: SourceMapWrapper {
                source_map: &self.runtime_result.source_map,
                current_dir: current_dir().unwrap(),
            },
        };

        let vm = &mut self.runtime_result.vm;

        //    let slice_before = &vm.heap_memory()[self.simulation_value_addr as usize..self.simulation_value_addr as usize + 32];
        //        eprintln!("before tick: frame: {:04X} {:04X},  raw: {:?}", self.simulation_value_addr, vm.frame_offset(), slice_before);

        vm.set_register_pointer_addr_for_parameter(1, self.simulation_value_addr);
        vm.set_stack_start(self.safe_stack_start_addr as usize);

        run_function_with_debug(vm, &self.tick_fn, application, run_options);

        //      let slice_after = &vm.heap_memory()[self.simulation_value_addr as usize..self.simulation_value_addr as usize + 32];
        //        eprintln!("after tick: frame: {:04X} {:04X} raw: {:?}", self.simulation_value_addr, vm.frame_offset(), slice_after);
    }

    #[must_use]
    pub fn compile() -> CodeGenAndVmResult {
        compile_codegen_and_create_vm(
            Path::new("assets/crawler"),
            &["crate".to_string(), "main".to_string()],
        )
        .unwrap()
    }

    pub fn boot(
        runtime_result: &mut CodeGenAndVmResult,
        application: &mut Application,
    ) -> (GenFunctionInfo, u32, u32) {
        let run_options = RunOptions {
            debug_stats_enabled: false,
            debug_opcodes_enabled: false,
            debug_info: &runtime_result.code_gen_result.debug_info,
            source_map_wrapper: SourceMapWrapper {
                source_map: &runtime_result.source_map,
                current_dir: current_dir().unwrap(),
            },
        };

        let main_fn = runtime_result
            .code_gen_result
            .find_function("main")
            .unwrap();

        let simulation_type = main_fn.return_type();
        let gen_simulation_type = layout_type(simulation_type);

        let mut s = String::new();
        let early_frame = runtime_result.vm.memory().frame_offset() as u32;

        let safe_stack_start = early_frame + gen_simulation_type.total_size.0 as u32;

        eprintln!(
            "before: frame: {:04X} safe_start: {:04X} raw: {:?}",
            early_frame,
            safe_stack_start,
            &runtime_result.vm.frame_memory()[..32]
        );
        runtime_result.vm.set_return_register_address(early_frame);
        run_function_with_debug(&mut runtime_result.vm, main_fn, application, run_options);
        eprintln!(
            "frame: {:04X} raw: {:?}",
            runtime_result.vm.memory().frame_offset(),
            &runtime_result.vm.frame_memory()[..32]
        );

        //print_value(&mut s, runtime_result.vm.frame_memory(), runtime_result.vm.memory(), StackMemoryAddress(0), &gen_simulation_type, "main() return").unwrap();
        writeln!(stderr(), "{}", s).unwrap();

        (
            runtime_result
                .get_gen_internal_member_function(simulation_type, "tick")
                .unwrap()
                .clone(),
            early_frame,
            safe_stack_start,
        )
    }

    pub fn run_once(runtime_result: &mut CodeGenAndVmResult, application: &mut Application) {
        let run_first_options = RunConstantsOptions {
            stderr_adapter: None,
        };

        run_first_time(
            &mut runtime_result.vm,
            &runtime_result.code_gen_result.constants_in_order,
            application,
            run_first_options,
        );
    }
}

pub struct FenText {
    pub application: Application,
    pub swamp: FenTextSwamp,
}

impl Default for FenText {
    fn default() -> Self {
        Self::new()
    }
}

impl HostFunctionCallback for Application {
    fn dispatch_host_call(&mut self, args: HostArgs) {
        match args.function_id {
            1 => print_fn(args),
            23 => self.external_move_cursor(args),
            24 => self.external_write(args),
            _ => panic!("unknown external"),
        }
    }
}

impl FenText {
    /// # Panics
    ///
    #[must_use]
    pub fn new() -> Self {
        let mut app = Application {
            canvas: Tui::new().unwrap(),
            tick_count: 0,
        };

        let swamp = FenTextSwamp::new(&mut app);

        Self {
            application: app,
            swamp,
        }
    }

    #[must_use]
    pub fn tick(&mut self) -> bool {
        let frame_time = Duration::from_millis(16);

        if let Some(input) = self.application.canvas.poll() {
            if input == Input::Esc {
                return false;
            }
        }

        self.swamp.tick(&mut self.application);

        self.application.tick_count += 1;

        self.application.debug();

        thread::sleep(frame_time);

        true
    }
}

fn main() {
    let mut engine = FenText::new();
    while engine.tick() {}
}
