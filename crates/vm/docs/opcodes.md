# Swamp VM Opcode Documentation

The opcodes for the Swamp Virtual Machine. The VM features a minimized 32 bit
ARM (RISC)-inspired architecture with 256 general-purpose registers. Includes complex instructions as "Runtime Functions".

## Swamp VM Instruc#tion Set

A 32-bit RISC-style VM with fixed-width instructions. Each instruction is 9 bytes:

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

- R0: Return register
- R1-R5. Argument registers
- R6-R127: Callee-saved registers. E.g. local variables.
- R128-R255: Caller-saved registers. Scratch / Temporary registers.

## Instruction Categories and Encoding

**81** unique opcodes.

| Mnemonic                          | U8 Value | Operands                    | Description                                                                                                                                     |
| --------------------------------- | -------- | --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| **Control Flow**                  |          |                             |                                                                                                                                                 |
| `nop`                             | 0        |                             | No operation.                                                                                                                                   |
| `hlt`                             | 1        |                             | Halts VM execution and relinquishes control back to the host. Used as a "return" for functions called by the host.                              |
| `panic`                           | 2        | `Rn`                        | Initiates a panic state in the VM. $Rn$ points to a string header with the explanation for the panic.                                           |
| `brk`                             | 3        |                             | Software breakpoint. _not implemented yet_.                                                                                                     |
| `b.true`                          | 26       | `#branch_delta`             | Branches if predicate flag is set (1). Branch target is `PC += #branch_delta`.                                                                  |
| `b.false`                         | 25       | `#branch_delta`             | Branches if predicate flag is not set (0). `PC += #branch_delta`.                                                                               |
| `b`                               | 27       | `#branch_absolute`          | Unconditional branch. Branch target is `PC += #branch_delta`.                                                                                   |
| `call`                            | 28       | `#branch_delta`             | Calls a subroutine at address #branch_absolute (Sets the `PC=#branch_absolute`), but first it is pushing current `PC` and Frame Pointer (`FP`). |
| `ret`                             | 30       |                             | Returns from a subroutine, restoring `PC` and frame pointer (`FP`).                                                                             |
| **Frame and Function Management** |          |                             |                                                                                                                                                 |
| `enter`                           | 29       | `#frame_size`               | Sets up a new stack frame of size `#frame_size`. `FP = SP`, `SP += #frame_size`.                                                                |
| **Integer Arithmetic**            |          |                             |                                                                                                                                                 |
| `add.u`                           | 4        | `Rd, Rn, Rm`                | Adds unsigned 32-bit integers. $Rd = Rn + Rm$.                                                                                                  |
| `add.u`                           | ?        | `Rd, Rn, #imm`              | Adds immediate value to register. $Rd = Rn + #imm$.                                                                                             |
| `mul`                             | 5        | `Rd, Rn, Rm`                | Multiplies unsigned 32-bit integers. $Rd = Rn * Rm$.                                                                                            |
| `sub`                             | 6        | `Rd, Rn, Rm`                | Subtracts unsigned 32-bit integers. $Rd = Rn - Rm$.                                                                                             |
| `neg`                             | 7        | `Rd, Rm`                    | Negates a signed 32-bit integer. $Rd = -Rm$.                                                                                                    |
| `mod`                             | 8        | `Rd, Rn, Rm`                | Signed 32-bit integer modulo. $Rd = Rn \% Rm$.                                                                                                  |
| `div`                             | 9        | `Rd, Rn, Rm`                | Signed 32-bit integer division. $Rd = Rn / Rm$.                                                                                                 |
| **Fixed Point Arithmetic**        |          |                             |                                                                                                                                                 |
| `mul.f`                           | 10       | `Rd, Rn, Rm`                | Multiplies 32-bit fixed point numbers. $Rd = Rn * Rm$.                                                                                          |
| `div.f`                           | 12       | `Rd, Rn, Rm`                | Divides 32-bit fixed point numbers. $Rd = Rn / Rm$.                                                                                             |
| **Comparisons and Flags**         |          |                             |                                                                                                                                                 |
| `lt`                              | 13       | `Rn, Rm`                    | Sets predicate flag if signed 32-bit $Rn < Rm$.                                                                                                 |
| `lt.u`                            | ?        | `Rn, Rm`                    | Sets predicate flag if unsigned 32-bit $Rn < Rm$.                                                                                               |
| `le`                              | 14       | `Rn, Rm`                    | Sets predicate flag if signed 32-bit $Rn \le Rm$.                                                                                               |
| `gt`                              | 15       | `Rn, Rm`                    | Sets predicate flag if signed 32-bit $Rn > Rm$.                                                                                                 |
| `ge`                              | 16       | `Rn, Rm`                    | Sets predicate flag if signed 32-bit $Rn \ge Rm$.                                                                                               |
| `ge.u`                            | ?        | `Rn, Rm`                    | Sets predicate flag if unsigned $Rn \ge Rm$.                                                                                                    |
| `cmp.b`                           | 17       | `Rn, #imm8`                 | Sets predicate flag if the lowest 8 bits of $Rn$ equal immediate value `#imm8`.                                                                 |
| `cmp`                             | 18       | `Rn, Rm`                    | Compares two registers $Rn$ and $Rm$ and sets predicate flag depending on equality.                                                             |
| `cmp.blk`                         | 19       | `Rn, Rm, #len`              | Compares blocks of memory pointed to by $Rn$ and $Rm$ with length `#len`. Sets flags (i.e. Z).                                                  |
| `tst`                             | 20       | `Rn`                        | Tests the value of register $Rn$ and updates the predicate flag. Predicate flag = $Rn$ != 0.                                                    |
| **Predicate Operations**          |          |                             |                                                                                                                                                 |
| `notp`                            | 22       |                             | Inverts the state of the predicate flag. **TODO: should be removed?**.                                                                          |
| `mvp`                             | 23       | `Rd`                        | Stores the state of the predicate flag into register $Rd$.                                                                                      |
| `mvnp`                            | 24       | `Rd`                        | Stores the inverse state of the predicate flag into register $Rd$. **TODO: should be removed?**.                                                |
| **Move and Copy**                 |          |                             |                                                                                                                                                 |
| `mov` (alias `mov.w`)             | 31       | `Rd, Rm`                    | Moves the value from register $Rm$ to register $Rd$. $Rd = Rm$.                                                                                 |
| `mov` (alias `mov.w`)             | 35       | `Rd, #imm32`                | Moves a 32-bit immediate value `#imm32` into register $Rd$.                                                                                     |
| `mov.b`                           | 34       | `Rd, #imm8`                 | Moves an 8-bit immediate value `#imm8` into the lower 8 bits of register $Rd$.                                                                  |
| `mov.h`                           | ?        | `Rd, #imm16`                | Moves a 16-bit immediate value into register $Rd$.                                                                                              |
| `blkcpy`                          | 32       | `Rd, Rn, #len`              | Copies a block of memory of length `#len` from address $Rn$ to address $Rd$. TODO: Maybe should be called `memmov`?.                            |
| **Load Operations**               |          |                             |                                                                                                                                                 |
| `lea`                             | 33       | `Rd, #offset`               | Loads the effective address calculated from `FP` + `#offset` into register $Rd$.                                                                |
| `lea.si`                          | ?        | `Rd, Rbase, Rindex, #scale` | Loads effective address with scaled index. $Rd = Rbase + (Rindex * #scale)$.                                                                    |
| `ld.b`                            | 36       | `Rd, [Rn, #offset]`         | Loads a 8-bit value from memory at address $Rn$ + `#offset` into register $Rd$.                                                                 |
| `ld.b`                            | ?        | `Rd, #absolute_addr`        | Loads a 8-bit value from the absolute memory address `#absolute_addr` into register $Rd$. (e.g. for loading constants).                         |
| `ld.h`                            | ?        | `Rd, [Rn, #offset]`         | Loads an unsigned 16-bit value from memory at address $Rn$ + `#offset` into register $Rd$.                                                      |
| `ld.h`                            | ?        | `Rd, #absolute_addr`        | Loads an unsigned 16-bit from the absolute memory address `#absolute_addr` into register $Rd$. (e.g. for loading constants).                    |
| `ld.w` (alias `ld`)               | 37       | `Rd, [Rn, #offset]`         | Loads a 32-bit value from memory at address $Rn$ + `#offset` into register $Rd$.                                                                |
| `ld.w` (alias `ld`)               | ?        | `Rd, #absolute_addr`        | Loads a 32-bit value from the absolute memory address `#absolute_addr` into register $Rd$. (e.g. for loading constants).                        |
| `ldmf`                            | ?        | `#mask, #fp_offset`         | Loads value from frame using mask at `FP + #fp_offset` into $Rd$ 0-7 depending on mask.                                                         |
| `ldmf`                            | 38       | `Rd, #fp_offset, #len`      | Loads a values from the pointer at FP + `#fp_offset` into register $Rd$ to `$Rd+#len-1`.                                                        |
| **Store Operations**              |          |                             |                                                                                                                                                 |
| `st.b`                            | 40       | `Rn, [Rd, #offset]`         | Stores a 8-bit value from register $Rn$ to memory at address $Rd$ + `#offset`.                                                                  |
| `st.h`                            | ?        | `Rn, [Rd, #offset]`         | Stores a 16-bit value from register $Rn$ to memory at address $Rd$ + `#offset`.                                                                 |
| `st.w` (alias `st`)               | 39       | `Rn, [Rd, #offset]`         | Stores a 32-bit value from register $Rn$ to memory at address $Rd$ + `#offset`.                                                                 |
| `stmf`                            | ?        | `#fp_offset, #mask`         | Stores multiple registers depending on mask (r0-r7) to frame starting at at `FP + #fp_offset`.                                                  |
| `stmf`                            | 41       | `Rn, #fp_offset, #len`      | Stores the value from register `$Rn$-$Rn$+#len` to the current stack frame starting at `#fp_offset`.                                            |
| **Memory Management**             |          |                             |                                                                                                                                                 |
| `memclrf`                         | 21       | `#offset, #size`            | Clears a block of memory on the current frame offset of byte size `#size`. Used mainly for literals to ensure that paddings are zeroed.         |
| **Float Functions**               |          |                             |                                                                                                                                                 |
| `fround`                          | 43       | `Rd, Rm`                    | Rounds the 32-bit fixed point in $Rm$ to the nearest integer fixed point, stores in $Rd$.                                                       |
| `ffloor`                          | 44       | `Rd, Rm`                    | Computes the floor of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                           |
| `fsqrt`                           | 45       | `Rd, Rm`                    | Computes the square root of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                     |
| `fsign`                           | 46       | `Rd, Rm`                    | Copies the sign of the 32-bit fixed point in $Rm$ to $Rd$ with a magnitude of 1.0.                                                              |
| `fabs`                            | 47       | `Rd, Rm`                    | Computes the absolute value of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                  |
| `fprnd`                           | 48       | `Rd`                        | Generates a pseudo-random 32-bit fixed point in $Rd$.                                                                                           |
| `fsin`                            | 49       | `Rd, Rm`                    | Computes the sine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                            |
| `fcos`                            | 50       | `Rd, Rm`                    | Computes the cosine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                          |
| `facos`                           | 51       | `Rd, Rm`                    | Computes the arccosine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                       |
| `fasin`                           | 52       | `Rd, Rm`                    | Computes the arcsine of the 32-bit fixed point in $Rm$, stores in $Rd$.                                                                         |
| `fatan2`                          | 53       | `Rd, Rn, Rm`                | Computes the arctangent of $Rn/Rm$ using their signs to determine the quadrant, stores in $Rd$.                                                 |
| `fmin`                            | 54       | `Rd, Rn, Rm`                | Computes the minimum of the 32-bit fixed points in $Rn$ and $Rm$, stores in $Rd$.                                                               |
| `fmax`                            | 55       | `Rd, Rn, Rm`                | Computes the maximum of the 32-bit fixed points in $Rn$ and $Rm$, stores in $Rd$.                                                               |
| `fclamp`                          | 56       | `Rd, Rn, Rm, Rp`            | Clamps the 32-bit fixed point in $Rn$ between the values in $Rm$ and $Rp$, stores in $Rd$. $Rd = \max(Rm, \min(Rn, Rp))$.                       |
| **Integer Functions**             |          |                             |                                                                                                                                                 |
| `irnd`                            | 58       | `Rd, Rm`                    | Generates a pseudo random integer based on the value in $Rm$, stores an unsigned 32 bit value in $Rd$.                                          |
| `itof`                            | 59       | `Rd, Rm`                    | Converts the integer in $Rm$ to a 32-bit fixed point, stores in $Rd$.                                                                           |
| `iabs`                            | 60       | `Rd, Rm`                    | Computes the absolute value of the integer in $Rm$, stores in $Rd$.                                                                             |
| `imin`                            | 61       | `Rd, Rn, Rm`                | Computes the minimum of the integers in $Rn$ and $Rm$, stores in $Rd$.                                                                          |
| `imax`                            | 62       | `Rd, Rn, Rm`                | Computes the maximum of the integers in $Rn$ and $Rm$, stores in $Rd$.                                                                          |
| `iclamp`                          | 63       | `Rd, Rn, Rm, Rp`            | Clamps the integer in $Rn$ between the values in $Rm$ and $Rp$, stores in $Rd$. $Rd = \max(Rm, \min(Rn, Rp))$.                                  |
| **Host Interaction**              |          |                             |                                                                                                                                                 |
| `host`                            | 65       | `#host_call_id, ...`        | Initiates a call to a host function specified by `#host_call_id`. Operands depend on the specific host call.                                    |
| **Runtime Functions**             |          |                             |                                                                                                                                                 |
| **String Operations**             |          |                             |                                                                                                                                                 |
| `btos`                            | ?        | `Rd, Rm`                    | Converts boolean in $Rm$ to string representation, stores pointer in $Rd$.                                                                      |
| `ftos`                            | 57       | `Rd, Rm`                    | Converts the 32-bit fixed point in $Rm$ to a string (pointer), stores pointer in $Rd$.                                                          |
| `itos`                            | 64       | `Rd, Rm`                    | Converts the integer in $Rm$ to a string (pointer), stores pointer in $Rd$.                                                                     |
| `str.cmp`                         | ?        | `Rn, Rm`                    | Compares strings pointed to by $Rn$ and $Rm$, sets predicate flag accordingly.                                                                  |
| `str.append`                      | 92       | `Rn, Rn, Rm`                | Creates a new string with the (constant) string $Rn$ appended with the (constant) string $Rm$ and stores the new constant string at $Rd$.       |
| **Range Operations**              |          |                             |                                                                                                                                                 |
| `range.init`                      | ?        | `Rd, Rn, Rm`                | Initializes a range from $Rn$ to $Rm$, stores in $Rd$.                                                                                          |
| `range.iter.init`                 | 68       | `Rd, Rn, Rm, Rp`            | Initializes a range iterator with start $Rn$, end $Rm$, inclusive $Rp$, stores iterator state in $Rd$.                                          |
| `range.iter.next`                 | 69       | `#branch_delta, Rd, Rn`     | Advances the range iterator in $Rn$, stores the next value in $Rd$, and updates $Rn$. Branches to PC + `#offset` if iteration is complete.      |
| **Vector Operations**             |          |                             |                                                                                                                                                 |
| `vec.init`                        | ?        | `Rd, #len, #capacity`       | Initializes a vector with length and capacity, stores pointer in $Rd$.                                                                          |
| `vec.push`                        | 74       | `Rn, Rm`                    | Appends the value in $Rm$ to the vector pointed to by $Rn$.                                                                                     |
| `vec.set`                         | 75       | `Rn, Rm, Rp`                | Sets the element at index $Rm$ in vector $Rn$ to the value in $Rp$.                                                                             |
| `vec.removeindex`                 | 76       | `Rn, Rm`                    | Removes the element at index $Rm$ from vector $Rn$.                                                                                             |
| `vec.pop`                         | 77       | `Rd, Rn`                    | Removes the last element from vector $Rn$ and stores its value in $Rd$.                                                                         |
| `vec.removeindexgetvalue`         | 78       | `Rd, Rn, Rm`                | Removes the element at index $Rm$ from vector $Rn$ and stores its value in $Rd$.                                                                |
| `vec.clear`                       | 79       | `Rn`                        | Removes all elements from vector $Rn$.                                                                                                          |
| `vec.create`                      | 80       | `Rd, #initial_capacity`     | Creates a new empty vector with initial capacity `#initial_capacity`, stores pointer in $Rd$.                                                   |
| `vec.get`                         | 81       | `Rd, Rn, Rm`                | Gets the element at index $Rm$ from vector $Rn$ and stores its value in $Rd$.                                                                   |
| `vec.getrange`                    | 82       | `Rd, Rn, Rm, Rp`            | Creates a new slice from vector $Rn$ from start index $Rm$ to end index $Rp$, stores slice pointer in $Rd$.                                     |
| `vec.swap`                        | 83       | `Rn, Rm, Rp`                | Swaps the elements at indices $Rm$ and $Rp$ in vector $Rn$.                                                                                     |
| `vec.iter.init`                   | 70       | `Rd, Rn`                    | Initializes a vector iterator for vector $Rn$, stores iterator state in $Rd$.                                                                   |
| `vec.iter.next`                   | 71       | `#branch_delta, Rd, Rn`     | Advance vector iterator in $Rn$, store the next element value in $Rd$, and updates $Rn$. Branch to PC + `#offset` if iteration is complete.     |
| `vec.iter.next.pair`              | 72       | `#branch_delta, Rd, Rn`     | Advance vector iterator in $Rn$, store index-value pair in $Rd$ and $Rd+1$ and updats $Rn$. Branch to PC + `#offset` if iteration is complete.  |
| **Map Operations**                |          |                             |                                                                                                                                                 |
| `map.init`                        | ?        | `Rd, #capacity, #key_size`  | Initializes a new map with given capacity and key size, stores pointer in $Rd$.                                                                 |
| `map.entry`                       | ?        | `Rd, Rn, Rm`                | Gets entry location for key $Rm$ in map $Rn$, stores location in $Rd$.                                                                          |
| `map.entry.must`                  | ?        | `Rd, Rn, Rm`                | Gets or reserves entry location for key $Rm$ in map $Rn$, stores location in $Rd$.                                                              |
| `map.remove`                      | 88       | `Rn, Rm`                    | Removes the entry with key $Rm$ from map $Rn$.                                                                                                  |
| `map.has`                         | 89       | `Rd, Rn, Rm`                | Checks if map $Rn$ contains key $Rm$, stores boolean result (e.g., 1 for true, 0 for false) in $Rd$.                                            |
| `map.fetch`                       | 90       | `Rd, Rn, Rm`                | Fetches the value associated with key $Rm$ from map $Rn$, stores value in $Rd$. May set a flag or return a specific value if key not found.     |
| `map.set`                         | 91       | `Rn, Rm, Rp`                | Sets or inserts the entry with key $Rm$ and value $Rp$ into map $Rn$.                                                                           |
| `map.iter.init`                   | 84       | `Rd, Rn`                    | Initializes a map iterator for map $Rn$, stores iterator state in $Rd$.                                                                         |
| `map.iter.next`                   | 85       | `#branch_delta, Rd, Rn`     | Advances the map iterator in $Rn$, stores the next value in $Rd$, and updates $Rn$. Branches to PC + `#offset` if iteration is complete.        |
| `map.iter.next.pair`              | 86       | `#branch_delta, Rd, Rn`     | Advance map iterator in $Rn$, store next key-value pair in $Rd$ and $Rd+1$ and updats $Rn$. Branc to `#branch_delta` if iteration is complete.  |

**Operand Key:**

- `Rd`: Destination Register (8-bit register index)
- `Rn`: Source Register n (8-bit register index)
- `Rm`: Source Register m (8-bit register index)
- `Rp`: Source Register p (8-bit register index)
- `#imm8`: 8-bit immediate value
- `#imm32`: 32-bit immediate value
- `#offset`: Offset value (can be positive or negative, size dependent on
  instruction encoding)
- `#fp_offset`: Frame Pointer unsigned Offset value.
- `#branch_delta`. Offset for the PC.
- `#branch_absolute`. Absolute u32 instruction offset to call into.
- `#absolute_addr`: Absolute unsigned u32 memory address.
- `#size`: Size value (size dependent on instruction encoding)
- `#len`: Length value (size dependent on instruction encoding)
- `[...]`: Memory address calculated from base register and offset.

**Internal registers:**

- SP. Stack Pointer. Modified by `enter` and `ret` opcode. Stack grows upwards.
- FP. Frame pointer. Points to the "start" of the allocated stack frame.
- PC. Program Counter, holds the offset to the instruction
- Flags. Only predicate flag is supported.
- R0-R255. General purpose 32-bit registers.

**Fixed point:**

It is a signed 32 bit value with 16 bits for integer part and 16 bits for the floating part. (16.16).

**Instruction execution:**

- Instruction is fetched from PC.
- PC is incremented (PC += 1).
- The instruction is executed.
