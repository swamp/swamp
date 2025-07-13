# sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
SWAMP_HOME=~/.swamp-dev cargo flamegraph --release --example very_basic --package swamp --
SWAMP_HOME=~/.swamp-dev xctrace record --template "Time Profiler" --output swamp_basic.release.trace --launch /Users/peter/cargo-target/release/examples/very_basic

# ~/.cargo/config.toml:
#[build]
# rustflags = ["-C", "force-frame-pointers=yes"]
# rustflags = ["-C", "symbol-mangling-version=v0"]