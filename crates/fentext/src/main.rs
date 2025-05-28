use fentext_ui::{Input, Tui};
use std::env::current_dir;
use std::path::Path;
use std::thread;
use std::time::Duration;
use swamp::prelude::{
    CodeGenAndVmResult, HostArgs, HostFunctionCallback, RunConstantsOptions, RunOptions,
    SourceMapWrapper, compile_codegen_and_create_vm, run_first_time, run_function_with_debug,
};
use swamp_std::print::print_fn;

pub struct Application {
    pub canvas: Tui,
    tick_count: usize,
}

impl Application {
    pub fn debug(&self) {
        self.canvas.move_to(0, 0);
        self.canvas.write(&format!("tick: {}", self.tick_count));
    }

    pub fn external_move_cursor(&mut self, mut host_args: HostArgs) {
        let x = host_args.get_i32();
        let y = host_args.get_i32();
        self.canvas.move_to(x as u16, y as u16);
    }
}

pub struct FenText {
    pub application: Application,
    pub runtime_result: CodeGenAndVmResult,
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
            _ => panic!("unknown external"),
        }
    }
}

impl FenText {
    /// # Panics
    ///
    #[must_use]
    pub fn new() -> Self {
        let runtime_result = Self::compile();
        let app = Application {
            canvas: Tui::new().unwrap(),
            tick_count: 0,
        };

        let mut s = Self {
            application: app,
            runtime_result,
        };

        s.run_once();

        s
    }

    #[must_use]
    pub fn compile() -> CodeGenAndVmResult {
        compile_codegen_and_create_vm(
            Path::new("assets/crawler"),
            &["crate".to_string(), "main".to_string()],
        )
        .unwrap()
    }

    pub fn run_once(&mut self) {
        let run_first_options = RunConstantsOptions {
            stderr_adapter: None,
        };

        run_first_time(
            &mut self.runtime_result.vm,
            &self.runtime_result.code_gen_result.constants_in_order,
            &mut self.application,
            run_first_options,
        );
    }

    #[must_use]
    pub fn tick(&mut self) -> bool {
        let frame_time = Duration::from_millis(16);

        if let Some(input) = self.application.canvas.poll() {
            if input == Input::Esc {
                return false;
            }
        }

        let run_options = RunOptions {
            debug_stats_enabled: false,
            debug_opcodes_enabled: false,
            debug_info: &self.runtime_result.code_gen_result.debug_info,
            source_map_wrapper: SourceMapWrapper {
                source_map: &self.runtime_result.source_map,
                current_dir: current_dir().unwrap(),
            },
        };

        let main_fn = self
            .runtime_result
            .code_gen_result
            .find_function("main")
            .unwrap();

        run_function_with_debug(
            &mut self.runtime_result.vm,
            main_fn,
            &mut self.application,
            run_options,
        );

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
