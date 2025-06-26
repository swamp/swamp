# SWAMP_HOME=~/swamp-dev cargo run --package swamp-build --bin swamp-build -- --path /Users/peter/external/catnipped/meteorite/scripts --module simulation --show-assembly > output.ansi
SWAMP_HOME=~/swamp-dev cargo run --package swamp-test --bin swamp-test -- --path /Users/peter/external/catnipped/meteorite/scripts --module test -i > output.ansi
