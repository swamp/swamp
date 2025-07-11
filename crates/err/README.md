# Swamp Error Reporter

Provides user-friendly formatting and display for errors generated during the compilation of [Swamp programming language](https://github.com/swamp/swamp) code.

## Overview

This crate takes structured error information, primarily from the `swamp-analyzer` crate, and transforms it into human-readable diagnostic messages. It leverages `swamp-source-map` to pinpoint the error location within the original source code and uses the `eira` library to build and format the final report, often including the relevant source line and highlighting.

## Key Features

* **Formats Compiler Errors:** Translates `swamp_analyzer::Error` (and potentially other error types) into structured reports.
* **Source Code Context:** Uses `swamp_source_map::SourceMap` to fetch and display the source code line where the error occurred.
* **Location Highlighting:** Points out the specific span within the source line related to the error.
* **User-Friendly Output:** Aims to present compiler errors clearly to the developer.

## Installation

This crate is typically used by the main Swamp compiler executable or related tooling.

```toml
[dependencies]
swamp-error-report = "0.2.8"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
