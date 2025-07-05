# Swamp VM Application Binary Interface (ABI)

This document describes the Application Binary Interface (ABI) for the Swamp
Virtual Machine.

The ABI defines the low-level conventions for how compiled code interacts with
the VM runtime, including function calls, data representation, and register
usage.

Adhering to this ABI is essential for compilers and runtime libraries targeting
the Swamp VM to ensure correct interoperation.

## 1. General Principles

- **Endianness:** The Swamp VM uses **Little Endian** byte ordering for all data
  in memory. When interpreting multi-byte values (like 32-bit integers or
  pointers), the least significant byte is stored at the lowest memory address.

- **Data Type Sizes:**

  - Bool. 0 is false, everything else is true. stored as u8.

  - Integers are always 32-bit signed or unsigned (`i32`/`u32`).

  - Floating-point values are represented as fixed-point numbers stored within a
    32-bit integer format (specifically 16 bits for the integer part and 16 bits
    for the fractional part, denoted as 16.16 fixed-point).

## 2. Register Allocation and Usage Conventions

The Swamp VM has 256 general-purpose registers ($R0$ - $R255$).
This ABI defines specific roles and save
conventions for a portion of these registers, particularly governing function
calls.

- **R0 (Return Value Register):**

  - Used **exclusively** for the return value of a function.

  - If a function returns a value, the result is placed in $R0$ by the callee
    before returning, or if it is a indirect (aggregate type) value, the pointer stored in the
    $R0$ is used.

  - If a function does not return a value (i.e., is 'void'), the contents of
    $R0$ upon return are undefined (as a return value). The callee has no
    obligation to set $R0$ for return purposes, and the caller must not use the
    value in $R0$ as if it were a meaningful result.

- **$R1$ - $R6$ (Argument Registers):**

  - Used to pass the first six (6) arguments to a function.

  - **$R1$** is conventionally used for the `self` pointer in method calls or
    the first argument in regular function calls.

  - **$R2$** through **$R6$** are used for arguments 2 through 6 (or 1 through 5
    following the `self` pointer).

  - These are **Caller-Save** registers. The caller must preserve these
    registers (e.g., by saving them to the stack) if their values are needed
    after the function call, as the callee is free to modify them. Usually the
    caller wants to keep the values, since later part of the function interacts
    with their own parameters.

- **$R7$ - $126$ (Callee-Save / Frame Registers):**

  - This range of is designated as
    **Callee-Save**. In the future this range might go down to $R7$ - $R48$ or similar.

  - If a callee function modifies any of these registers, it must save their
    original values before modifying them and restore them before returning.

  - These registers are primarily used by compilers to hold "frame variables" –
    local variables or values that need to persist throughout the execution of a
    function, across calls made by that function. This large pool allows
    compilers to keep many local variables in registers, reducing memory access.
    This is a standard and idiomatic convention for registers holding long-lived
    local state.

- **$128$ - $R255$ (Caller-Save / Temporary Registers - Primary Range):**

  - This range of 32 registers is designated as **Caller-Save**.

  - Callee are free to modify these registers
    without saving their original values.

  - These registers are typically used for temporary values that are short-lived
    within a function or between function calls. Callers must save them if their
    values are needed across a call.

## 3. Function Calling Convention

To call a function adhering to the Swamp VM ABI:

1. **Argument & Hidden Pointer Passing:**

   - **Direct Values. functions returning 32-bit or smaller values:**

     - Place the `self` pointer (if applicable) or the first argument in R1.

     - Place the second argument in $R2$, ..., up to the sixth argument in R6.

   - **Indirect Values. For functions returning values larger than 32 bits
     (e.g., structs):**

     - The caller must allocate memory space for the return value (e.g., on its
       stack frame or the heap).

     - Place the **pointer** to this allocated space in $R0$ (serving as the
       hidden pointer).

     - Place the `self` pointer (if applicable) or the first explicit argument
       in $R1$.

     - Place the second explicit argument in $R2$, ..., up to the sixth explicit
       argument in $R6$.

2. **Function Call:** Execute the `call #addr` instruction, where `#addr` is the
   absolute instruction offset to the target function's entry point. The `call`
   instruction implicitly saves the return address onto the stack.

3. **Return Value Retrieval:**

   - **Direct Values. For functions returning 32-bit or smaller values
     (primitives):** Retrieve the return value from register $R0$.

   - **Indirect Values. For functions returning values larger than 32 bits:**
     The return value has been written directly to the memory location pointed
     to by the hidden pointer passed to the callee in $R0$. The caller can then
     access the return data at the address originally placed in $R0$ before the
     call.

To implement a function adhering to the Swamp VM ABI:

1. **Entry (`enter`):** The function begins execution. The `enter #frame_size`
   instruction sets up the function's stack frame. It increases the SP and sets
   the FP.

2. **Argument & Hidden Pointer Access:** Access arguments passed in registers
   $R1$-$R6$ (understanding $R0$'s potential role as a hidden pointer for large
   returns). If the function returns a large type, treat the value in $R0$ upon
   entry as a destination pointer.

3. **Callee-Save Registers:** If the function needs to use any registers in the
   **$R7$-$127$** range, it must save their current values (typically onto the
   stack, relative to the FP) immediately after the `enter` instruction and
   before modifying them.

4. **Local Variables:** Allocate space for local variables within the stack
   frame (relative to the FP). Local variables that need to persist across calls
   made by this function are good candidates for $R1$-$R127$ registers.

5. **Body Execution:** Execute the function's logic.

   - **For functions returning Direct Values (scalar), 32-bit or smaller values:**
     Compute the result and place it into $R0$ before returning.

   - **For functions returning values Indirect (aggregate) Values, larger than 32 bits:**
     Compute the result and write it to the memory location pointed to by the
     "hidden pointer" received upon entry (in $R0$).

6. **Restore Callee-Save Registers:** Before returning, restore the original
   values of any **R7-R127** registers that were saved upon entry.

7. **Return (`ret`):** Execute the `ret` instruction. This restores the Stack
   Pointer (SP) and Frame Pointer (FP) to their state before the `enter`
   instruction and pops the saved return address into the Program Counter (PC).

## 4. Stack Frame Layout

The `enter` and `ret` instructions manage the stack frame by internal registers
in the VM (FP and SP):

## 5. Data Representation and Memory Layout

- **Primitives:** `i32` integers and 16.16 fixed-point floats occupy 4 bytes in
  memory, stored in Little Endian format.

- **Pointers:** Pointers are unsigned 32-bit values, stored in Little Endian
  format.

  - Pointers to Indirect Values point directly to the memory location of the
    struct's data. The layout of fields within a struct (order, padding) is
    determined by the compiler, subject to alignment requirements.

  - Pointers to **Vec** or **String** point to a **header** structure in memory.
    This header contains metadata about the collection, such as a pointer to the
    actual data buffer on the heap, the number of elements/characters, and the
    capacity.

- **Alignment:** Data types must be aligned to their size. The compiler is
  responsible for ensuring appropriate alignment in data structures.

## 6. Runtime Function Calls

Runtime function calls are handled as normal opcodes.

## 7. Host Calls

Calls to the host environment using the `host` instruction follow a convention
similar to standard function calls, but without the VM's typical stack frame
management by the `host` instruction itself:

- Arguments are passed in registers $R1$-$R6$, similar to regular function
  calls. $R1$ can be used for a context pointer (self) or the first argument.

- The host function's identifier is passed as an immediate operand
  (`#host_call_id`).

- The host function returns its result in $R0$ (or use the pointer inside $R0$
  to write the return value).

- The `host` instruction transfers control to the VM's host interface layer,
  which then invokes the appropriate native host function. The VM's stack frame
  (`enter`/`ret`) is not automatically managed by the `host` instruction.
