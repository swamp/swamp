/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use source_map_cache::SourceMap;
use source_map_cache::SourceMapWrapper;
use std::path::{Path, PathBuf};
use swamp::prelude::SeqMap;
use swamp_code_gen_program::code_gen_program;
use swamp_dep_loader::swamp_registry_path;

#[test_log::test]
fn compile_and_run() {
    let mut mounts = SeqMap::new();
    let path_buf = Path::new("/Users/peter/external/swamp_autobattler/scripts").to_path_buf();
    mounts.insert("crate".to_string(), path_buf).unwrap();

    let registry_path = swamp_registry_path().unwrap();
    mounts
        .insert("registry".to_string(), registry_path)
        .unwrap();

    let mut source_map = SourceMap::new(&mounts).expect("source map failed");

    let crate_main_path = &["crate".to_string(), "main".to_string()];

    let program = swamp_compile::bootstrap_and_compile(&mut source_map, crate_main_path)
        .expect("TODO: panic message");

    let source_map_wrapper = SourceMapWrapper {
        source_map: &source_map,
        current_dir: PathBuf::from(Path::new("")),
    };
    code_gen_program(&program, &source_map_wrapper);
    //info!(?program, "program");
}
