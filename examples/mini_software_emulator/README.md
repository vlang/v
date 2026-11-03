# ARM64 styled CPU emulator in V

This example implements a small 64-bit CPU with an ARM inspired instruction set.
It uses its own four-byte encoding and does not execute ARM machine code.

The CPU has 31 general purpose registers (`x0` through `x30`). Register 31 is
`XZR`: reads return zero and writes are discarded. Register `x30` also serves as
the link register for `BL` and `RET`. Arithmetic wraps at 64 bits. Division and
remainder use unsigned operands; arithmetic right shifts interpret the operand
as signed. Shift counts are masked to six bits.

## Run

From the V repository root, run the demonstrations and timing comparisons:

```sh
./v run examples/mini_software_emulator/
./v -prod run examples/mini_software_emulator/
```

Run the regression tests:

```sh
./v test examples/mini_software_emulator/
```

The demonstration compares native V calculations with emulated summation,
square summation, Fibonacci and Pell sequences. These small timings illustrate
execution overhead and are not a controlled performance benchmark. Summation
helpers require a positive `max`; repeated benchmarks also require positive
`times`. Sequence helpers accept zero.

## Encoding

Each instruction occupies four bytes in little endian order. The opcode is the
first byte (the low eight bits of the decoded word). Register fields are full
bytes but must contain values from 0 through 31. Unused bytes are ignored.

Formats below list bytes from low to high. `R` is `[op, rd, rn, rm]`, `I` is
`[op, rd, rn, imm8]`, and `T` is `[op, target_lo, target_mid, target_hi]`.
Branch targets are absolute byte addresses. Immediate values are unsigned.

| Opcode | Instruction | Format | Operation |
| --- | --- | --- | --- |
| `00` | HALT | `[op, 0, 0, 0]` | Stop execution |
| `01` | MOV | `[op, rd, imm_lo, imm_hi]` | Load a 16-bit immediate |
| `02` | ADD | R | `rd = rn + rm` |
| `03` | SUB | R | `rd = rn - rm` |
| `04` | MUL | R | `rd = rn * rm` |
| `05` | B | T | Always branch |
| `06` | B.EQ | T | Branch if Z |
| `07` | LDR | I | Load 64 bits at `rn + imm8` |
| `08` | STR | I | Store 64 bits from rd at `rn + imm8` |
| `09` | CMP | `[op, rn, rm, 0]` | Set flags from `rn - rm` |
| `0A` | B.NE | T | Branch if not Z |
| `0B` | B.LT | T | Branch if N differs from V |
| `0C` | B.LE | T | Branch if Z or N differs from V |
| `0D` | B.GT | T | Branch if not Z and N equals V |
| `0E` | B.GE | T | Branch if N equals V |
| `0F` | ADDI | I | `rd = rn + imm8` |
| `10` | SUBI | I | `rd = rn - imm8` |
| `11` | MULI | I | `rd = rn * imm8` |
| `12` | DIV | R | Unsigned division |
| `13` | DIVI | I | Unsigned immediate division |
| `14` | REM | R | Unsigned remainder |
| `15` | REMI | I | Unsigned immediate remainder |
| `16`, `17` | AND, ANDI | R, I | Bitwise AND |
| `18`, `19` | ORR, ORRI | R, I | Bitwise OR |
| `1A`, `1B` | EOR, EORI | R, I | Bitwise XOR |
| `1C`, `1D` | LSL, LSLI | R, I | Logical left shift |
| `1E`, `1F` | LSR, LSRI | R, I | Logical right shift |
| `20`, `21` | ASR, ASRI | R, I | Arithmetic right shift |
| `22` | BL | T | Save PC + 4 in x30, then branch |
| `23` | RET | `[op, 0, 0, 0]` | Branch to x30 |
| `24` | LDRB | I | Load a byte at `rn + imm8` |
| `25` | STRB | I | Store the low byte of rd at `rn + imm8` |
| `26` | MOVR | `[op, rd, rn, 0]` | Copy rn to rd |
| `27` | CMPI | `[op, rn, imm_lo, imm_hi]` | Set flags from `rn - imm16` |
| `28`, `29` | ADDS, SUBS | R | Add or subtract and set flags |
| `2A`, `2B` | ADDIS, SUBIS | I | Immediate add or subtract and set flags |

`CMP`, `CMPI` and flag-setting arithmetic update all four NZCV flags:

- N is the most significant result bit; Z indicates a zero result.
- C means unsigned carry on addition, or no borrow on subtraction.
- V means that the signed result overflowed.

Loads and stores use little endian byte order. Memory is shared by instructions
and data, so stores can change later instructions.

## Execution and validation

`CPU.new(size)` creates zeroed memory. `execute_program` copies its input to
address zero and starts there, preserving existing registers and other memory.
Use `reset` to clear memory, registers, flags and execution state first.
`execute_instruction` executes one complete step, including PC advancement.
Taken branches preserve their target even when it equals the current PC.

A program must fit in memory and contain complete four-byte instructions.
Execution stops on HALT or when PC reaches exactly the program length, and
`state.is_running` then becomes false. Taken branch targets must be aligned
and within the program or exactly at its end. Intentional infinite loops
continue running until the caller stops the process.

Invalid register indexes, instruction fetches, memory addresses and branch
targets panic. Unknown opcodes and division or remainder by zero also panic.
Bounds checks occur before host indexing or unsigned address addition.

The following fragment can be used alongside this example's CPU source files:

```v ignore
mut cpu := CPU.new(1024)
program := [
    u8(0x01), 0x01, 0x15, 0x00, // MOV x1, #21
    0x01, 0x02, 0x07, 0x00, // MOV x2, #7
    0x04, 0x03, 0x01, 0x02, // MUL x3, x1, x2
]
cpu.execute_program(program)
assert cpu.registers.read(3) == 147
```
