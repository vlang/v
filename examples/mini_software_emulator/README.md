# ⚡ ARM64-Styled CPU Emulator in V

A lightweight, fast, and feature-complete 64-bit ARM-inspired Virtual Machine and Instruction Set Architecture (ISA) emulator written entirely in **V** (`vlang`).

This project serves as a clear, high-performance reference implementation of low-level CPU concepts—including **registers, memory indexing, flag manipulation, conditional branching, call stacks, and binary instruction decoding**—all implemented with zero dependencies.

---

## 📸 Features

- **64-Bit General-Purpose Registers:** Implements 31 general-purpose registers (`x0`–`x30`) and a dedicated zero-register `XZR` (`x31`) that discards writes and yields zero.
- **Custom 32-Bit Instruction Encoding:** Little-endian opcode-based binary format featuring arithmetic, logic, immediate values, bit-shifts, memory I/O, and function calls.
- **Condition Flags (`NZCV`):** Full support for Negative (`N`), Zero (`Z`), Carry (`C`), and Overflow (`V`) flags updated by setting operations (e.g., `ADDS`, `SUBS`, `CMP`).
- **Control Flow & Branching:** Support for absolute branching (`B`), link register branching (`BL` / `RET`), and condition code jumps (`B.EQ`, `B.NE`, `B.LT`, `B.LE`, `B.GT`, `B.GE`).
- **Memory Operations:** Endian-aware 64-bit load/store (`LDR`, `STR`) and 8-bit byte load/store (`LDRB`, `STRB`).
- **Benchmarking Tools:** Built-in benchmarking suite to compare native V execution speed against the virtual machine's fetch-decode-execute cycle.

---

## 🏗️ System Architecture

```
                    ┌──────────────────────────────────┐
                    │       CPU State / Control        │
                    │   Program Counter (PC) & State   │
                    └────────────────┬─────────────────┘
                                     │
           ┌─────────────────────────┼─────────────────────────┐
           ▼                         ▼                         ▼
┌────────────────────┐    ┌────────────────────┐    ┌────────────────────┐
│    Registers       │    │   Flags (NZCV)     │    │  Memory Space      │
│  x0 – x30 (u64)    │    │  Zero      (Z)     │    │  RAM ([]u8)        │
│  x31 (XZR = 0)     │    │  Negative  (N)     │    │  Little-Endian     │
└────────────────────┘    │  Carry     (C)     │    └────────────────────┘
                          │  Overflow  (V)     │
                          └────────────────────┘
```

---

## 📑 Supported Instruction Set (Opcodes)

| Opcode | Mnemonic | Description | Format / Bit Layout |
| :--- | :--- | :--- | :--- |
| `0x00` | `HALT` | Halts CPU execution | `[Opcode]` |
| `0x01` | `MOV` | Load immediate 16-bit value into register | `[Opcode:8] [Rd:8] [Imm:16]` |
| `0x02` | `ADD` | Register addition (`Rd = Rn + Rm`) | `[Opcode:8] [Rd:8] [Rn:8] [Rm:8]` |
| `0x03` | `SUB` | Register subtraction (`Rd = Rn - Rm`) | `[Opcode:8] [Rd:8] [Rn:8] [Rm:8]` |
| `0x04` | `MUL` | Register multiplication (`Rd = Rn * Rm`) | `[Opcode:8] [Rd:8] [Rn:8] [Rm:8]` |
| `0x05` | `B` | Unconditional Branch | `[Opcode:8] [Target Address:24]` |
| `0x06` | `B.EQ` | Branch if Equal (`Z == 1`) | `[Opcode:8] [Target Address:24]` |
| `0x07` | `LDR` | Load 64-bit doubleword from memory | `[Opcode:8] [Rd:8] [Rn:8] [Offset:8]` |
| `0x08` | `STR` | Store 64-bit doubleword to memory | `[Opcode:8] [Rt:8] [Rn:8] [Offset:8]` |
| `0x09` | `CMP` | Compare registers (updates `NZCV`) | `[Opcode:8] [Rn:8] [Rm:8]` |
| `0x0A` | `B.NE` | Branch if Not Equal (`Z == 0`) | `[Opcode:8] [Target Address:24]` |
| `0x0B` | `B.LT` | Branch if Less Than (`N != V`) | `[Opcode:8] [Target Address:24]` |
| `0x0C` | `B.LE` | Branch if Less or Equal (`Z == 1` or `N != V`) | `[Opcode:8] [Target Address:24]` |
| `0x0D` | `B.GT` | Branch if Greater Than (`Z == 0` and `N == V`)| `[Opcode:8] [Target Address:24]` |
| `0x0E` | `B.GE` | Branch if Greater or Equal (`N == V`) | `[Opcode:8] [Target Address:24]` |
| `0x0F` | `ADDI` | Add immediate byte (`Rd = Rn + Imm`) | `[Opcode:8] [Rd:8] [Rn:8] [Imm:8]` |
| `0x10` | `SUBI` | Subtract immediate byte (`Rd = Rn - Imm`) | `[Opcode:8] [Rd:8] [Rn:8] [Imm:8]` |
| `0x11` | `MULI` | Multiply immediate byte (`Rd = Rn * Imm`) | `[Opcode:8] [Rd:8] [Rn:8] [Imm:8]` |
| `0x12` | `DIV` | Unsigned integer division | `[Opcode:8] [Rd:8] [Rn:8] [Rm:8]` |
| `0x13` | `DIVI` | Immediate integer division | `[Opcode:8] [Rd:8] [Rn:8] [Imm:8]` |
| `0x14` | `REM` | Remainder / Modulo | `[Opcode:8] [Rd:8] [Rn:8] [Rm:8]` |
| `0x15` | `REMI` | Immediate Remainder / Modulo | `[Opcode:8] [Rd:8] [Rn:8] [Imm:8]` |
| `0x16`–`0x1B` | `AND`/`ORR`/`EOR` | Bitwise Logical Operations (Register & Immediate) | `[Opcode:8] [Rd:8] [Rn:8] [Rm/Imm:8]` |
| `0x1C`–`0x21` | `LSL`/`LSR`/`ASR` | Bitwise Shift Operations (Logical/Arithmetic) | `[Opcode:8] [Rd:8] [Rn:8] [Shift:8]` |
| `0x22` | `BL` | Branch with Link (`x30 = PC + 4`, `PC = Target`) | `[Opcode:8] [Target Address:24]` |
| `0x23` | `RET` | Return from Subroutine (`PC = x30`) | `[Opcode:8]` |
| `0x24`–`0x25` | `LDRB`/`STRB` | Load/Store Single Byte | `[Opcode:8] [Rd/Rt:8] [Rn:8] [Offset:8]` |
| `0x26` | `MOVR` | Move Register to Register (`Rd = Rn`) | `[Opcode:8] [Rd:8] [Rn:8]` |
| `0x27` | `CMPI` | Compare Register with Immediate | `[Opcode:8] [Rn:8] [Imm:16]` |
| `0x28`–`0x2B` | `ADDS`/`SUBS`/..| Flag-Setting Arithmetic (`ADDS`, `SUBS`, `ADDIS`, `SUBIS`) | `[Opcode:8] [Rd:8] [Rn:8] [Rm/Imm:8]` |

---

## 🚀 Quick Start

### Prerequisites

Make sure you have [V installed](https://vlang.io/#install).

```bash
# Verify V installation
v version
```

### Running the Example & Tests

1. Clone or copy the codebase into a directory:
   ```bash
   git clone https://github.com/your-username/v-arm64-emulator.git
   cd v-arm64-emulator
   ```

2. Run the project directly:
   ```bash
   v run .
   ```

3. (Optional) Run with optimized release flags:
   ```bash
   v -prod run .
   ```

---

## 💡 Example Usage

Here is how to create a CPU instance, assemble raw instruction bytes, and execute a loop program programmatically in V:

```v
module main

fn main() {
    // 1. Initialize a CPU with 1KB of memory
    mut cpu := CPU.new(1024)

    // 2. Write a program calculating 21 * 7 using an ARM64-styled loop:
    //    MOV x1, #21
    //    MOV x2, #7
    //    MOV x3, #0
    //    MOV x4, #1
    // loop:
    //    ADD x3, x3, x1
    //    SUB x2, x2, x4
    //    CMP x2, x0
    //    B.EQ exit
    //    B loop
    program := [
        u8(0x01), 0x01, 0x15, 0x00, // MOV x1, #21
        0x01, 0x02, 0x07, 0x00,     // MOV x2, #7
        0x01, 0x03, 0x00, 0x00,     // MOV x3, #0
        0x01, 0x04, 0x01, 0x00,     // MOV x4, #1
        // --- Loop Start (Address 0x10) ---
        0x02, 0x03, 0x03, 0x01,     // ADD x3, x3, x1
        0x03, 0x02, 0x02, 0x04,     // SUB x2, x2, x4
        0x09, 0x02, 0x00, 0x00,     // CMP x2, x0 (XZR)
        0x06, 0x24, 0x00, 0x00,     // B.EQ -> exit (0x24)
        0x05, 0x10, 0x00, 0x00,     // B -> loop (0x10)
        // --- Exit (Address 0x24) ---
    ]

    // 3. Load & Run
    cpu.execute_program(program)

    // 4. Retrieve results
    result := cpu.registers.read(3)
    println('Result of 21 * 7 = ${result}') // Outputs: 147
}
```

---

## 🧪 Included Test Benchmarks

The project includes test suites verifying core CPU operations:

- **Rough Verification:** Memory round-trips (`STR` / `LDR`), branch correctness, dynamic bounds checks.
- **Mathematical Sequences:** Fibonacci calculation ($Fib(71)$) and Pell numbers ($Pell(21)$).
- **Square Summation Benchmark:** Computes $\sum_{i=1}^{N} i^2$ both natively in V and inside the VM to benchmark cycle overhead.

---

## 🤝 Contributing

Contributions are welcome! If you want to expand the ISA (e.g., floating-point registers `v0`–`v31`, stack pointer `SP` abstractions, or standard ARM64 system calls), feel free to open a Pull Request.

---

## 📜 License

This project is licensed under the **MIT License**.