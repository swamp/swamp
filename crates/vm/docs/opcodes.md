# Swamp VM Opcode Documentation

The opcodes for the Swamp Virtual Machine. The VM features a minimized 32 bit
ARM (RISC)-inspired architecture with 256 general-purpose registers. Includes
complex instructions as "Runtime Functions".

## Swamp VM Instruction Set

A 32-bit RISC-style VM with fixed-width instructions. Each instruction is 9
bytes:

- 1 byte opcode
- 8 bytes operands

## Instruction Format

```text
 Byte   |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |
 Field  | Op  |            Operands (8 bytes)                 |
```

## Data Types

- Fixed-point: 32-bit (16.16 format)
- Integer: 32-bit signed/unsigned
- Byte: 8-bit
- Half: 16-bit

## Register Conventions

Note: These ranges might be reduced in future Swamp VM versions.

- R0: Return register
- R1-R6. Argument registers. Caller-saved.
- R7-R127: Callee-saved registers. E.g. local function variables.
- R128-R255: Caller-saved registers. Scratch / Temporary registers.

## Instruction Categories and Encoding

**134** unique opcodes.

| Mnemonic                   | Operands                       | Description                                                                                                                             |
| -------------------------- | ------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------- |
| **Special**                |                                |                                                                                                                                         |
| `nop`                      |                                | No operation.                                                                                                                           |
| `hlt`                      |                                | Halts VM execution and relinquishes control back to the host. Used as a "return" for functions called by the host.                      |
| `user_halt`                |                                | Halts VM execution initiated by the user.                                                                                               |
| `step`                     |                                | Stop executing and return to host. User has a step-point here.                                                                          |
| `panic`                    | `Rn`                           | Initiates a panic state in the VM. $Rn$ points to a string header with the explanation for the panic.                                   |
| `trap`                     | `#reason_code`                 | Provide a reason code for trap.                                                                                                         |
| `brk`                      |                                | Breakpoint. Pause execution, keep all relevant state.                                                                                   |
| **Control Flow**           |                                |                                                                                                                                         |
| `b.true`                   | `Rd, #branch_delta`            | Branch if register $Rd$ is non-zero. Branch target is `PC += #branch_delta`.                                                            |
| `b.false`                  | `Rd, #branch_delta`            | Branch if register $Rd$ is zero. Branch target is `PC += #branch_delta`.                                                                |
| `b`                        | `#branch_delta`                | Unconditional branch. Branch target is `PC += #branch_delta`.                                                                           |
| `call`                     | `#branch_absolute`             | Call a subroutine at address #branch_absolute (Sets the `PC=#branch_absolute`), but first pushes current `PC` and Frame Pointer (`FP`). |
| `enter`                    | `#frame_size`                  | Sets up a new stack frame of size `#frame_size`. `FP = SP`, `SP += #frame_size`.                                                        |
| `ret`                      |                                | Returns from a subroutine, restoring `PC` and frame pointer (`FP`).                                                                     |
| **Integer Arithmetic**     |                                |                                                                                                                                         |
| `add`                      | `Rd, Rn, Rm`                   | Adds unsigned 32-bit (and indirectly signed 32-bite) integers. $Rd = Rn + Rm$.                                                          |
| `add`                      | `Rd, Rn, #imm32`               | Adds immediate value to register. $Rd = Rn +  \#imm32$.                                                                                 |
| `mul`                      | `Rd, Rn, Rm`                   | Multiplies unsigned 32-bit integers. $Rd = Rn * Rm$.                                                                                    |
| `sub`                      | `Rd, Rn, Rm`                   | Subtracts unsigned 32-bit integers. $Rd = Rn - Rm$.                                                                                     |
| `s.neg`                    | `Rd, Rm`                       | Negates a signed 32-bit integer. $Rd = -Rm$.                                                                                            |
| `s.mod`                    | `Rd, Rn, Rm`                   | Signed 32-bit integer modulo. $Rd = Rn \% Rm$.                                                                                          |
| `s.div`                    | `Rd, Rn, Rm`                   | Signed 32-bit integer division. $Rd$ = $Rn$ / $Rm$.                                                                                     |
| **Fixed Point Arithmetic** |                                |                                                                                                                                         |
| `f.mul`                    | `Rd, Rn, Rm`                   | Multiplies 32-bit fixed point numbers. $Rd = Rn * Rm$.                                                                                  |
| `f.div`                    | `Rd, Rn, Rm`                   | Divides 32-bit fixed point numbers. $Rd = Rn / Rm$.                                                                                     |
| **Comparisons**            |                                |                                                                                                                                         |
| `lt`                       | `Rd, Rn, Rm`                   | Set $Rd$ to 1 if signed 32-bit $Rn < Rm$, otherwise set $Rd$ to 0.                                                                      |
| `le`                       | `Rd, Rn, Rm`                   | Set $Rd$ to 1 if signed 32-bit $Rn \le Rm$, otherwise set $Rd$ to 0.                                                                    |
| `gt`                       | `Rd, Rn, Rm`                   | Set $Rd$ to 1 if signed 32-bit $Rn > Rm$, otherwise set $Rd$ to 0.                                                                      |
| `ge`                       | `Rd, Rn, Rm`                   | Set $Rd$ to 1 if signed 32-bit $Rn \ge Rm$, otherwise set $Rd$ to 0.                                                                    |
| `uge`                      | `Rd, Rn, Rm`                   | Set $Rd$ to 1 if unsigned $Rn \ge Rm$, otherwise set $Rd$ to 0.                                                                         |
| `ult`                      | `Rd, Rn, Rm`                   | Set $Rd$ to 1 if unsigned 32-bit $Rn < Rm$, otherwise set $Rd$ to 0.                                                                    |
| `cmp`                      | `Rd, Rn, Rm`                   | Compare registers $Rn$ and $Rm$ and set register $Rd$ to 1 if equal, otherwise set $Rd$ to 0.                                           |
| `cmp`                      | `Rd, Rn, #imm8`                | Set register $Rd$ to 1 if the whole $Rn$ equal immediate value `#imm8`, otherwise set $Rd$ to 0.                                        |
| `cmp.blk`                  | `Rd, Rn, Rm, #len`             | Compares block of memory pointed to by $Rn$ and $Rm$ with length `#len`. Set register $Rd$ to 1 if equal, otherwise set $Rd$ to 0.      |
| `trap.lt`                  | `Rn, Rm`                       | Trap if $Rn < Rm$.                                                                                                                      |
| **Memory Operations**      |                                |                                                                                                                                         |
| `blk.cpy`                  | `Rd, Rn, #len`                 | Copy block of memory of length `#len` from address $Rn$ to address $Rd$.                                                                |
| `clr.blk.f`                | `#fp_offset, #size`            | Clear block of memory on the current frame offset of byte size `#size`.                                                                 |
| **Load Operations**        |                                |                                                                                                                                         |
| `lea`                      | `Rd, #fp_offset`               | Load effective address calculated from `FP` + `#fp_offset` into register $Rd$.                                                          |
| `ld.b`                     | `Rd, [Rn, #offset]`            | Load an 8-bit value from memory at address $Rn$ + `#offset` into register $Rd$.                                                         |
| `ld.b`                     | `Rd, #absolute_addr`           | Load an 8-bit value from absolute memory address `#absolute_addr` into register $Rd$.                                                   |
| `ld.h`                     | `Rd, [Rn, #offset]`            | Load a 16-bit value from memory at address $Rn$ + `#offset` into register $Rd$.                                                         |
| `ld`                       | `Rd, [Rn, #offset]`            | Load a 32-bit value from memory at address $Rn$ + `#offset` into register $Rd$.                                                         |
| `ld`                       | `Rd, #absolute_addr`           | Load a 32-bit value from absolute memory address `#absolute_addr` into register $Rd$.                                                   |
| `ldmf`                     | `Rd, #fp_offset, #len`         | Load values from the frame at FP + `#fp_offset` into register $Rd$ to `$Rd+#len-1`.                                                     |
| `ldmf`                     | `#mask, #fp_offset`            | Load values from frame using mask at `FP + #fp_offset` into registers R0-R7 depending on mask.                                          |
| **Store Operations**       |                                |                                                                                                                                         |
| `st.b`                     | `[Rd, #offset], Rn`            | Store an 8-bit value from register $Rn$ to memory at address $Rd$ + `#offset`.                                                          |
| `st.h`                     | `[Rd, #offset], Rn`            | Store a 16-bit value from register $Rn$ to memory at address $Rd$ + `#offset`.                                                          |
| `st`                       | `[Rd, #offset], Rn`            | Store a 32-bit value from register $Rn$ to memory at address $Rd$ + `#offset`.                                                          |
| `stmf`                     | `#fp_offset, Rn, #len`         | Store values from register $Rn-Rn+\#len-1$ to the current stack frame starting at `#fp_offset`.                                         |
| `stmf`                     | `#fp_offset, #mask`            | Store registers specified by mask (R0-R7) to frame starting at `FP + #fp_offset`.                                                       |
| **Move Operations**        |                                |                                                                                                                                         |
| `mov`                      | `Rd, Rm`                       | Move the value from $Rm$ to $Rd$. $Rd = Rm$.                                                                                            |
| `mov.b`                    | `Rd, #imm8`                    | Move an 8-bit immediate value `#imm8` into the lower 8 bits of register $Rd$.                                                           |
| `mov.h`                    | `Rd, #imm16`                   | Move a 16-bit immediate value `#imm16` into register $Rd$.                                                                              |
| `mov`                      | `Rd, #imm32`                   | Move a 32-bit immediate value `#imm32` into register $Rd$.                                                                              |
| `meqz`                     | `Rd, Rm`                       | Set register $Rd$ to 1 if $Rm$ equals zero, otherwise sets $Rd$ to 0.                                                                   |
| **Float Functions**        |                                |                                                                                                                                         |
| `f.round`                  | `Rd, Rm`                       | Round the 32-bit fixed point in $Rm$ to the nearest integer fixed point, store in $Rd$.                                                 |
| `f.floor`                  | `Rd, Rm`                       | Compute the floor of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                    |
| `f.sqrt`                   | `Rd, Rm`                       | Compute the square root of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                              |
| `f.sign`                   | `Rd, Rm`                       | Copy the sign of the 32-bit fixed point in $Rm$ to $Rd$ with a magnitude of 1.0.                                                        |
| `f.abs`                    | `Rd, Rm`                       | Compute the absolute value of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                           |
| `f.prnd`                   | `Rd, Rm`                       | Generate a pseudo-random 32-bit fixed point in $Rd$ based on $Rm$.                                                                      |
| `f.sin`                    | `Rd, Rm`                       | Compute the sine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                     |
| `f.cos`                    | `Rd, Rm`                       | Compute the cosine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                   |
| `f.acos`                   | `Rd, Rm`                       | Compute the arccosine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                |
| `f.asin`                   | `Rd, Rm`                       | Compute the arcsine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                  |
| `f.atan2`                  | `Rd, Rm`                       | Compute the arctangent of $Rm$, stores in $Rd$.                                                                                         |
| `f.min`                    | `Rd, Rn, Rm`                   | Compute the minimum of the 32-bit fixed points in $Rn$ and $Rm$, stores in $Rd$.                                                        |
| `f.max`                    | `Rd, Rn, Rm`                   | Compute the maximum of the 32-bit fixed points in $Rn$ and $Rm$, stores in $Rd$.                                                        |
| `f.clamp`                  | `Rd, Rn, Rm, Rp`               | Clamp the 32-bit fixed point in $Rn$ between the values in $Rm$ and $Rp$, stores in $Rd$. $Rd = \max(Rm, \min(Rn, Rp))$.                |
| `f.to.str`                 | `Rd, Rm`                       | Convert the 32-bit fixed point in $Rm$ to a string (pointer), store pointer in $Rd$.                                                    |
| **Integer Functions**      |                                |                                                                                                                                         |
| `i.rnd`                    | `Rd, Rm`                       | Generate a pseudo random integer based on the value in $Rm$, stores an unsigned 32 bit value in $Rd$.                                   |
| `i.tof`                    | `Rd, Rm`                       | Convert the integer in $Rm$ to a 32-bit fixed point, store in $Rd$.                                                                     |
| `i.abs`                    | `Rd, Rm`                       | Compute the absolute value of the integer in $Rm$, store in $Rd$.                                                                       |
| `i.min`                    | `Rd, Rn, Rm`                   | Compute the minimum of the integers in $Rn$ and $Rm$, store in $Rd$.                                                                    |
| `i.max`                    | `Rd, Rn, Rm`                   | Compute the maximum of the integers in $Rn$ and $Rm$, store in $Rd$.                                                                    |
| `i.clamp`                  | `Rd, Rn, Rm, Rp`               | Clamps the integer in $Rn$ between the values in $Rm$ and $Rp$, store in $Rd$. $Rd = \max(Rm, \min(Rn, Rp))$.                           |
| `i.tos`                    | `Rd, Rm`                       | Converts the integer in $Rm$ to a string (pointer), store pointer in $Rd$.                                                              |
| **Codepoint Functions**    |                                |                                                                                                                                         |
| `codepoint.to.str`         | `Rd, Rm`                       | Converts unicode codepoint in $Rm$ to string representation, store string pointer in $Rd$.                                              |
| **Boolean Functions**      |                                |                                                                                                                                         |
| `bool.to.str`              | `Rd, Rm`                       | Converts boolean in $Rm$ to string representation, store pointer in $Rd$.                                                               |
| **Byte Functions**         |                                |                                                                                                                                         |
| `byte.to.str`              | `Rd, Rm`                       | Converts byte in $Rm$ to string representation, store pointer in $Rd$. the string will be prefixed with 0x, e.g. "0xF4"                 |
| **Range Operations**       |                                |                                                                                                                                         |
| `range.init`               | `Rd, Rn, Rm, Rp`               | Initialize range from $Rn$ to $Rm$ with inclusive flag $Rp$, store in $Rd$.                                                             |
| `range.iter`               | `Rd, Rm`                       | Initialize range iterator for range $Rm$, store iterator state in $Rd$.                                                                 |
| `range.iter.next`          | `Rd, Rm, #branch_delta`        | Advance the range iterator in $Rm$, store the next value in $Rd$. Branches to PC + `#branch_delta` if iteration is complete.            |
| **Array Operations**       |                                |                                                                                                                                         |
| `array.init`               | `Rd, Rm, #capacity`            | Initialize fixed capacity array with capacity `#capacity` and element address in $Rm$, store in $Rd$.                                   |
| **Vector Operations**      |                                |                                                                                                                                         |
| `vec.init`                 | `Rd, #capacity, #element_size` | Initialize vector with capacity and element size, store in $Rd$.                                                                        |
| `vec.cmp`                  | `Rd, Rm, Rp`                   | Compare vectors $Rm$ and $Rp$, store boolean result in $Rd$.                                                                            |
| `vec.copy`                 | `Rd, Rm`                       | Copy the $Rm$ over the existing vector at $Rd$.                                                                                         |
| `vec.push`                 | `Rd, Rm`                       | Append space for an element to vector $Rm$ and return the address in $Rd$.                                                              |
| `vec.rem`                  | `Rd, Rm`                       | Remove the element at index $Rm$ from vector $Rd$.                                                                                      |
| `vec.pop`                  | `Rd, Rm`                       | Remove the last element from vector $Rm$ and store its address in $Rd$.                                                                 |
| `vec.rem.v`                | `Rd, Rm, Rp`                   | Remove the element at index $Rp$ from vector $Rm$ and store its value in $Rd$.                                                          |
| `vec.get`                  | `Rd, Rm, Rp`                   | Get the element at index $Rp$ from vector $Rm$ and store its address in $Rd$.                                                           |
| `vec.get.range`            | `Rd, Rm, Rp`                   | Create a new slice from vector $Rm$ using range $Rp$, store slice pointer in $Rd$.                                                      |
| `vec.swap`                 | `Rd, Rm, Rp`                   | Swap the elements at indices $Rm$ and $Rp$ in vector $Rd$.                                                                              |
| `vec.iter`                 | `Rd, Rm`                       | Initialize a vector iterator for vector $Rm$, store iterator state in $Rd$.                                                             |
| `vec.iter.next`            | `Rd, Rm, #branch_delta`        | Advance vector iterator in $Rm$, store the next element in $Rd$. Branches to PC + `#branch_delta` if iteration is complete.             |
| `vec.iter.next.pair`       | `Rd, Rm, Rp, #branch_delta`    | Advance vector iterator in $Rm$, store index in $Rd$ and value in $Rp$. Branches to PC + `#branch_delta` if iteration is complete.      |
| **Map Operations**         |                                |                                                                                                                                         |
| `map.init`                 | `Rd, params`                   | `params: #capacity, #key_size, #key_align, #value_size, #value_align`. Init map with capacity and parameters, store pointer in $Rd$.    |
| `map.entry`                | `Rd, Rm, Rp`                   | Get entry location for key $Rp$ in map $Rm$, store location in $Rd$.                                                                    |
| `map.entry.or_create`      | `Rd, Rm, Rp`                   | Get or add entry location for key $Rp$ in map $Rm$, store location in $Rd$.                                                             |
| `map.rem`                  | `Rd, Rm`                       | Remove the entry with key $Rm$ from map $Rd$.                                                                                           |
| `map.has`                  | `Rd, Rm, Rp`                   | Check if map $Rm$ contains key $Rp$, store boolean result in $Rd$.                                                                      |
| `map.overwrite`            | `Rd, Rm`                       | Overwrite map $Rd$ with contents of map $Rm$.                                                                                           |
| `map.iter.init`            | `Rd, Rm`                       | Initialize a map iterator for map $Rm$, store iterator state in $Rd$.                                                                   |
| `map.iter.next`            | `Rd, Rm, #branch_delta`        | Advance the map iterator in $Rm$, store the next value in $Rd$. Branches to PC + `#branch_delta` if iteration is complete.              |
| `map.iter.next.pair`       | `Rd, Rm, Rp, #branch_delta`    | Advance map iterator in $Rm$, store key in $Rd$ and value in $Rp$. Branches to PC + `#branch_delta` if iteration is complete.           |
| **String Operations**      |                                |                                                                                                                                         |
| `str.app`                  | `Rd, Rm, Rp`                   | Create a new string with $Rm$ appended with $Rp$ and store the new string at $Rd$.                                                      |
| `str.cmp`                  | `Rd, Rm, Rp`                   | Compare strings pointed to by $Rm$ and $Rp$, sets register $Rd$ to 1 if equal, otherwise sets $Rd$ to 0.                                |
| `str.tos`                  | `Rd, Rm`                       | Convert string in $Rm$ to string representation, store pointer in $Rd$.                                                                 |
| `str.iter`                 | `Rd, Rm`                       | Initialize a string iterator for string $Rm$, store iterator state in $Rd$.                                                             |
| `str.iter.next`            | `Rd, Rm, #branch_delta`        | Advance string iterator in $Rm$, store the next codepoint in $Rd$. Branches to PC + `#branch_delta` if iteration is complete.           |
| `str.iter.next.pair`       | `Rd, Rm, Rp, #branch_delta`    | Advance string iterator in $Rm$, store index in $Rd$ and codepoint in $Rp$. Branches to PC + `#branch_delta` if iteration is complete.  |
| **Host Interaction**       |                                |                                                                                                                                         |
| `host`                     | `#host_call_id, #args_count`   | Initiate call to a host function specified by `#host_call_id` with `#args_count` arguments.                                             |
| **Sparse Collection**      |                                |                                                                                                                                         |
| `sparse.init`              | `Rd, #element_size, #capacity` | Initialize a sparse collection with element size and capacity, store in $Rd$.                                                           |
| `sparse.add_entry_addr`    | `Rd, Rm, Rp, #element_size`    | Add a new entry to sparse collection $Rp$, store handle in $Rm$ and entry address in $Rd$.                                              |
| `sparse.remove`            | `Rd, Rm`                       | Remove the entry with handle $Rm$ from sparse collection $Rd$.                                                                          |
| `sparse.entry_addr`        | `Rd, Rm, Rp, #element_size`    | Get the entry address for handle $Rp$ in sparse collection $Rm$, store in $Rd$.                                                         |
| `sparse.is_alive`          | `Rd, Rm, Rp`                   | Check if handle $Rp$ is valid in sparse collection $Rm$, store boolean result in $Rd$.                                                  |
| `sparse.iter.init`         | `Rd, Rm`                       | Initialize an iterator for sparse collection $Rm$, store iterator state in $Rd$.                                                        |
| `sparse.iter.next`         | `Rd, Rm, #branch_delta`        | Advance sparse iterator in $Rm$, store next handle in $Rd$. Branches to PC + `#branch_delta` if iteration is complete.                  |
| `sparse.iter.next.pair`    | `Rd, Rm, Rp, #branch_delta`    | Advance sparse iterator in $Rm$, store handle in $Rd$ and value in $Rp$. Branches to PC + `#branch_delta` if iteration is complete.     |
| **Grid Operations**        |                                |                                                                                                                                         |
| `grid.init`                | `Rd, Rm, #width, #height`      | Initialize a grid with element size in $Rm$, dimensions `#width` x `#height`, store in $Rd$.                                            |
| `grid.entry_addr`          | `Rd, Rm, Rp, Rq`               | Get the entry address at coordinates ($Rp$,$Rq$) in grid $Rm$, store in $Rd$.                                                           |

**Operand Key:**

- `Rd`: Destination Register (8-bit register index)
- `Rn`: Source Register n (8-bit register index)
- `Rm`: Source Register m (8-bit register index)
- `Rp`: Source Register p (8-bit register index)
- `Rq`: Source Register q (8-bit register index)
- `#imm8`: 8-bit immediate value
- `#imm16`: 16-bit immediate value
- `#imm32`: 32-bit immediate value
- `#offset`: 32-bit Offset value (can be positive or negative)
- `#fp_offset`: 32-bit Frame Pointer unsigned Offset value
- `#branch_delta`: 16-bit signed offset for the PC
- `#branch_absolute`: Absolute u32 instruction offset to call into
- `#absolute_addr`: Absolute unsigned u32-bit memory address
- `#size`: 32-bit Size value
- `#len`: 32-bit length value
- `#element_size`: 32-bit Size of collection elements in bytes
- `#capacity`: 16-bit Initial capacity of a collection
- `#key_size`: 32-bit Size of map keys in bytes
- `#key_align`: 8-bit Alignment requirement for map keys
- `#value_size`: 32-bit Size of map values in bytes
- `#value_align`: 8-bit Alignment requirement for map values
- `#width`, `#height`: 16-bit Dimensions for grid initialization
- `#host_call_id`: 16-bit ID of host function to call
- `#args_count`: 8-bit Number of arguments for host call
- `[...]`: Memory address calculated from base register and 32-bit offset

**Internal registers:**

- SP. Stack Pointer. Modified by `enter` and `ret` opcode. Stack grows upwards.
- FP. Frame pointer. Points to the "start" of the allocated stack frame.
- PC. Program Counter, holds the offset to the instruction.
- R0-R255. General purpose 32-bit registers.
