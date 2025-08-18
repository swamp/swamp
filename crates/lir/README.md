# swamp-lir

A tiny, machine-shaped low-level, 32-bit IR, mainly for the Swamp toolchain.
This is not trying to compete with other IRs.
It’s a pragmatic layer that makes it easy to target
multiple CPUs (older ARM CPUs, Swamp VM, etc.)
without giving up sane performance or writing a giant
backend.

Swamp has two IRs:

- `swamp-mir` – friendly mid-level IR (intrinsics, effects on calls, symbolic stack slots).

- `swamp-lir` – Boring on purpose:
    - no intrinsics
    - no Select
    - no effects metadata
    - explicit control flow
    - concrete stack addressing
