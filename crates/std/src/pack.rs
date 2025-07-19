/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm::host::HostArgs;
use swamp_vm_types::AnyHeader;

pub fn pack(args: HostArgs) {
    let output = args.get::<AnyHeader>(1);
    eprintln!("OUTPUT: {output:?}");
}
