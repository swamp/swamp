/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_vm::host::HostArgs;

pub fn print_fn(mut args: HostArgs) {
    let output = args.get_str();
    eprintln!("print: {output}");
}
