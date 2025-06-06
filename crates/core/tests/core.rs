/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_core::create_module;
use tiny_ver::TinyVersion;
use tracing::info;

#[test_log::test]
fn test_core() {
    let version: TinyVersion = "1.2.3".parse().unwrap();

    let module = create_module(&version);

    let x = format!("output:{module:?}");

    info!(x, "output");
}
