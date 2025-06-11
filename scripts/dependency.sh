# brew install graphviz
# cargo install cargo-tree
# cargo install cargo-depgraph

# cargo tree --workspace --all-features --no-dev-dependencies

# --workspace-only
cargo depgraph > depgraph.dot
dot -Tsvg depgraph.dot -o depgraph.svg
