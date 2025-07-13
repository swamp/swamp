# Swamp Parser

Parses [Swamp programming language](https://github.com/swamp/swamp) source code text into an Abstract Syntax Tree (AST).

## Overview

This crate takes a string containing Swamp source code and attempts to parse it according to the grammar defined in `src/grammar.pest`. If successful, it produces an AST representation using the data structures defined in the `swamp-ast` crate. If the source code contains syntax errors, it returns a `ParseError`.

The parser is built using the [pest](https://pest.rs/) parser generator library.

## Key Features

*   **Input:** Swamp source code as a `&str`.
*   **Output:** A `Result<swamp_ast::Module, ParseError>`.
*   **AST:** The success variant contains the root of the Abstract Syntax Tree (`swamp_ast::Module`).
*   **Error Handling:** The error variant (`ParseError`) provides information about the location and nature of the syntax error.
*   **Span Information:** Attaches source location information (`swamp_ast::Node` containing `SpanWithoutFileId`) to the generated AST nodes.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
swamp-parser = "0.2.11"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
