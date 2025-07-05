/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::time::{Duration, Instant};
use swamp_vm::Vm;
use swamp_vm::host::HostFunctionCallback;
use swamp_vm_types::InstructionPosition;
pub struct TestExternals {}

impl HostFunctionCallback for TestExternals {
    fn dispatch_host_call(&mut self, _args: swamp_vm::host::HostArgs) {}
}

fn setup_vm() -> Vm {
    todo!()
}

fn format_duration(nanos: f64) -> String {
    if nanos < 1_000.0 {
        format!("{nanos:.2} ns")
    } else if nanos < 100_000.0 {
        format!("{:.2} us", nanos / 1_000.0)
    } else if nanos < 1_000_000_000.0 {
        format!("{:.2} ms", nanos / 1_000_000.0)
    } else {
        format!("{:.2} s", nanos / 1_000_000_000.0)
    }
}
fn main() {
    println!("Starting VM performance benchmark");

    println!("\nWarm-up phase...");
    {
        let warmup_iterations = 5_000;
        let mut vm = setup_vm();

        for i in 0..warmup_iterations {
            if i % 1_000 == 0 {
                println!("  Warm-up iteration {i}");
            }
            vm.execute_from_ip(&InstructionPosition(0), &mut TestExternals {});
            vm.reset();
        }

        println!("  Warm-up complete (CPU cache and branch prediction should be primed)");
        println!("  Waiting for system to stabilize...");
        std::thread::sleep(std::time::Duration::from_millis(700));
    }

    println!("-- Single VM, multiple executions, should be the fastest");

    let execution_count = 10_000;

    let mut vm = setup_vm();

    let start = Instant::now();
    for _ in 0..execution_count {
        vm.execute_from_ip(&InstructionPosition(0), &mut TestExternals {});
        vm.reset();
    }
    let duration = start.elapsed();

    summary(duration, execution_count, None);

    // ---------------
    println!("-- Fresh VM executions (compile not timed), should be similar in performance");
    let iterations = 1_000;

    let mut total_execution_time = std::time::Duration::new(0, 0);

    let start_test_time = Instant::now();
    for _ in 0..iterations {
        let mut vm = setup_vm();

        let start = Instant::now();
        vm.execute_from_ip(&InstructionPosition(0), &mut TestExternals {});
        let execution_time = start.elapsed();
        total_execution_time += execution_time;
    }
    let test_time_duration = start_test_time.elapsed();
    summary(total_execution_time, iterations, Some(test_time_duration));

    println!("-- Multiple VMs executed sequentially, includes setup_vm (compile) time");
    let vm_count = 1_000;

    let start = Instant::now();
    for _ in 0..vm_count {
        let mut vm = setup_vm();
        vm.execute_from_ip(&InstructionPosition(0), &mut TestExternals {});
    }
    let duration = start.elapsed();
    summary(duration, vm_count, None);

    println!("\nAll benchmarks completed successfully!");
}

pub fn summary(total_execution_time: Duration, iteration_count: i32, test_time: Option<Duration>) {
    let duration = total_execution_time;
    let total_nanos = duration.as_nanos() as f64;
    let per_op_nanos = total_nanos / f64::from(iteration_count);
    print!(
        "  Completed {} executions in {} ({}/execution)",
        iteration_count,
        format_duration(total_nanos),
        format_duration(per_op_nanos)
    );

    if let Some(found_test_time) = test_time {
        print!(
            " (test_time {})",
            format_duration(found_test_time.as_nanos() as f64)
        );
    }
    println!();
}
