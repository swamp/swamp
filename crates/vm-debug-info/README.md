# swamp-vm-debug-info

Internal helper for swamp VM — maps Swamp VM instructions to source code info.

## What’s inside?

* **FileOffsetTable**: map PC ranges -> file/line
* **FunctionTable**: find which function covers a PC
* **Meta & FunctionInfo**: per-instruction comments and function metadata

## License

MIT — see [LICENSE](LICENSE) for details.

_Copyright Peter Bjorklund. All rights reserved._
