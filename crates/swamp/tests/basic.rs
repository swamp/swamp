use std::path::PathBuf;
use swamp_runtime::{RunConstantsOptions, RunOptions};
use swamp_std::print::register_print;
use time_dilation::ScopedTimer;

#[must_use]
pub fn get_fixture_dir(sub_dirs: &[&str]) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("tests");
    path.push("fixtures");
    for sub_dir in sub_dirs {
        path.push(sub_dir);
    }

    path
}

#[test_log::test]
fn very_basic() {}
