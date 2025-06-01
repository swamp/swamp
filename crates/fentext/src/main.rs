use fentext_ui::{Input, Tui};
use fs_change_detector::FileWatcher;
use pico_args::Arguments;
use std::env::current_dir;
use std::path::{Path, PathBuf};
use std::time::Duration;
use std::{process, thread};
use swamp::prelude::{
    CodeGenAndVmResult, GenFunctionInfo, HostArgs, HostFunctionCallback, RunConstantsOptions,
    RunOptions, SAFE_ALIGNMENT, SourceMapWrapper, VmState, align, compile_codegen_and_create_vm,
    layout_type, run_first_time, run_function, run_function_with_debug,
};
use swamp_std::print::print_fn;
use tracing::{error, warn};

#[must_use]
pub fn compile() -> Option<CodeGenAndVmResult> {
    compile_codegen_and_create_vm(
        Path::new("assets/crawler"),
        &["crate".to_string(), "main".to_string()],
    )
}

pub type FfiInput = SwampEnumWithoutPayload;

impl FfiInput {
    pub const LEFT: Self = Self { discriminant: 0 };
    pub const RIGHT: Self = Self { discriminant: 1 };
    pub const UP: Self = Self { discriminant: 2 };
    pub const DOWN: Self = Self { discriminant: 3 };

    pub const ACTION_1: Self = Self { discriminant: 4 };
    pub const ACTION_2: Self = Self { discriminant: 5 };
}

pub struct Application {
    pub canvas: Tui,
    pub last_keypress: SwampOption<FfiInput>,
    tick_count: usize,
}

#[repr(C)]
pub struct SwampOption<T: Default> {
    pub is_some: u8, // this is needed because Rust pick a single fixed size for the T payload slot within EnumName that works for all instantiations of T that it encounters. Sigh.
    pub value: T,
}

impl<T: Default> SwampOption<T> {
    pub const fn some(t: T) -> Self {
        Self {
            is_some: 1,
            value: t,
        }
    }

    #[must_use]
    pub fn none() -> Self {
        Self {
            is_some: 0,
            value: Default::default(),
        }
    }
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
pub struct SwampEnumWithoutPayload {
    pub discriminant: u8, // Just the tag
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
        let str = { host_args.get_str().to_string() };
        let enum_without_payload = host_args.read_from_register::<SwampEnumWithoutPayload>(2);
        let discriminant = unsafe { (*enum_without_payload).discriminant };

        let colors: Vec<crossterm::style::Color> = vec![
            crossterm::style::Color::Black,
            crossterm::style::Color::DarkRed,
            crossterm::style::Color::DarkGreen,
            crossterm::style::Color::DarkYellow,
            crossterm::style::Color::DarkBlue,
            crossterm::style::Color::DarkMagenta,
            crossterm::style::Color::DarkCyan,
            crossterm::style::Color::Grey,
            crossterm::style::Color::DarkGrey,
            crossterm::style::Color::Red,
            crossterm::style::Color::Green,
            crossterm::style::Color::Yellow,
            crossterm::style::Color::Blue,
            crossterm::style::Color::Magenta,
            crossterm::style::Color::Cyan,
            crossterm::style::Color::White,
        ];

        let color = &colors[discriminant as usize];
        self.canvas.write(&str, *color);
    }

    pub(crate) fn external_last_keypress(&self, mut host_args: HostArgs) {
        host_args.write_to_register(0, &self.last_keypress);
    }
}

pub struct FenTextEngine {
    pub detector: FileWatcher,
    pub fen_text: Option<FenText>,
}

impl FenTextEngine {
    pub(crate) fn update(&mut self) -> bool {
        if self.detector.has_changed() {
            self.something_changed();
        }
        self.fen_text.as_mut().map_or_else(
            || {
                thread::sleep(Duration::from_millis(32));
                true
            },
            FenText::tick,
        )
    }
}

impl FenTextEngine {
    /// # Panics
    ///
    #[must_use]
    pub fn new(path: &Path) -> Self {
        let mut engine = Self {
            detector: FileWatcher::new(path).unwrap(),
            fen_text: None,
        };

        engine.something_changed();

        engine
    }

    pub fn something_changed(&mut self) {
        if let Some(_previous_fen_text) = &self.fen_text {
            // Intentionally drop FenText and restore the screen and everything.
            // this is needed so we can see the actual compile
            self.fen_text = None;
        }
        let compile = compile();
        if let Some(runtime_result) = compile {
            let new_fen_text = FenText::new(runtime_result);
            self.fen_text = Some(new_fen_text);
        }
    }
}

pub struct FenTextSwamp {
    pub runtime_result: CodeGenAndVmResult,
    pub tick_fn: GenFunctionInfo,
    pub simulation_value_addr: u32,
    pub safe_stack_start_addr: u32,
}

impl FenTextSwamp {
    pub fn new(
        mut runtime_result: CodeGenAndVmResult,
        application: &mut dyn HostFunctionCallback,
    ) -> Self {
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

    pub fn tick(&mut self, application: &mut dyn HostFunctionCallback) -> bool {
        let run_options = RunOptions {
            debug_stats_enabled: false,
            debug_opcodes_enabled: false,
            debug_operations_enabled: false,
            use_color: true,
            max_count: 0,
            debug_info: &self.runtime_result.code_gen_result.debug_info,
            source_map_wrapper: SourceMapWrapper {
                source_map: &self.runtime_result.source_map,
                current_dir: current_dir().unwrap(),
            },
        };

        let vm = &mut self.runtime_result.vm;

        assert!(self.simulation_value_addr >= vm.constant_size() as u32);
        vm.set_register_pointer_addr_for_parameter(1, self.simulation_value_addr);
        vm.set_stack_start(self.safe_stack_start_addr as usize);

        let run_fast = true;

        if run_fast {
            run_function(vm, &self.tick_fn, application, run_options);
        } else {
            run_function_with_debug(vm, &self.tick_fn, application, run_options);
        }

        vm.state == VmState::Normal
    }

    #[must_use]
    pub fn boot(
        runtime_result: &mut CodeGenAndVmResult,
        application: &mut dyn HostFunctionCallback,
    ) -> (GenFunctionInfo, u32, u32) {
        let run_options = RunOptions {
            debug_stats_enabled: false,
            debug_opcodes_enabled: false,
            debug_operations_enabled: false,
            use_color: true,
            max_count: 0,
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

        let s = String::new();
        let early_frame = runtime_result.vm.memory().constant_memory_size as u32;

        let safe_stack_start = align(
            (early_frame + u32::from(gen_simulation_type.total_size.0)) as usize,
            SAFE_ALIGNMENT,
        );

        runtime_result.vm.set_return_register_address(early_frame);
        run_function_with_debug(&mut runtime_result.vm, main_fn, application, run_options);

        //print_value(&mut s, runtime_result.vm.frame_memory(), runtime_result.vm.memory(), StackMemoryAddress(0), &gen_simulation_type, "main() return").unwrap();
        eprintln!("{s}");

        (
            runtime_result
                .get_gen_internal_member_function(simulation_type, "tick")
                .unwrap()
                .clone(),
            early_frame,
            safe_stack_start as u32,
        )
    }

    pub fn run_once(
        runtime_result: &mut CodeGenAndVmResult,
        application: &mut dyn HostFunctionCallback,
    ) {
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
    pub ui: Application,
    pub swamp: FenTextSwamp,
}

impl HostFunctionCallback for Application {
    fn dispatch_host_call(&mut self, args: HostArgs) {
        match args.function_id {
            1 => print_fn(args),
            22 => self.external_last_keypress(args),
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
    pub fn new(runtime_result: CodeGenAndVmResult) -> Self {
        let mut ui = Application {
            canvas: Tui::new().unwrap(),
            tick_count: 0,
            last_keypress: SwampOption::none(),
        };

        let swamp = FenTextSwamp::new(runtime_result, &mut ui);

        Self { ui, swamp }
    }

    #[must_use]
    pub fn tick(&mut self) -> bool {
        let frame_time = Duration::from_millis(0);

        let ui = &mut self.ui;
        if let Some(input) = ui.canvas.poll() {
            if input == Input::Esc {
                return false;
            }

            let converted = match input {
                Input::Left => FfiInput::LEFT,
                Input::Right => FfiInput::RIGHT,
                Input::Up => FfiInput::UP,
                Input::Down => FfiInput::DOWN,
                Input::Action1 => FfiInput::ACTION_1,
                Input::Action2 => FfiInput::ACTION_2,
                _ => panic!("unknown input"),
            };
            ui.last_keypress = SwampOption::some(converted);
        } else {
            ui.last_keypress = SwampOption::none();
        }

        ui.tick_count += 1;

        ui.debug();

        if !self.swamp.tick(ui) {
            return false;
        }

        thread::sleep(frame_time);

        true
    }
}

pub fn init_logger() {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .init();
}

fn main() {
    init_logger();

    let mut args = Arguments::from_env();

    // Explicitly specify the type parameters: <SuccessType, ErrorType>
    let script_dir: PathBuf =
        match args.free_from_os_str::<PathBuf, String>(|s| Ok(PathBuf::from(s))) {
            Ok(path) => path,
            Err(e) => {
                error!(?e, "Error parsing script directory:");
                error!("Usage: fentext <script_directory>");
                process::exit(1);
            }
        };

    let remaining_args = args.finish();
    if !remaining_args.is_empty() {
        warn!(?remaining_args, "Unrecognized arguments");
    }

    let mut engine = FenTextEngine::new(Path::new(&script_dir));

    while engine.update() {}
}
