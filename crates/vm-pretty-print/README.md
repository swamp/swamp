# swamp-vm-pretty-print

**Swamp VM's Memory, in glorious colors!** 🎨

Are you tired of staring into the soul-crushing abyss of undecipherable bytes in memory?

Fear not, this crate will help you out!

**So, what does this thing actually do?**

You point it at a chunk of memory from the Swamp VM's memory, give it an address,
and tell it what kind of data you think is there (like a struct, a number, or a TaggedUnion).

It then dives head-first into that raw memory, converts the bytes, and spits out a beautiful,
colorful, and indented string that shows you exactly what's inside.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
