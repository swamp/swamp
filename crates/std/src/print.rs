/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm::host::HostArgs;

pub fn print_fn(args: HostArgs) {
    let output = args.string(1);
    eprintln!("{output}");
}
