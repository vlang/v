module main

// -- HALT instruction -- \\
// -- opcode: 0x00 -- \\
pub fn halt(mut cpu CPU, instruction u32) {
    cpu.state.is_running = false
}

// -- MOV instruction -- \\
// -- opcode: 0x01 -- \\
pub fn mov(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    imm := u64((instruction >> 16) & 0xFFFF)
    cpu.registers.write(rd, imm)
}

// -- ADD instruction -- \\
// -- opcode: 0x02 -- \\
pub fn add(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    sum := cpu.registers.read(rn) + cpu.registers.read(rm)
    cpu.registers.write(rd, sum)
}

// -- SUB instruction -- \\
// -- opcode: 0x03 -- \\
pub fn sub(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    diff := cpu.registers.read(rn) - cpu.registers.read(rm)
    cpu.registers.write(rd, diff)
}

// -- MUL instruction -- \\
// -- opcode: 0x04 -- \\
pub fn mul(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    prod := cpu.registers.read(rn) * cpu.registers.read(rm)
    cpu.registers.write(rd, prod)
}

// -- B instruction -- \\
// -- opcode: 0x05 -- \\
pub fn b(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    cpu.pc = target_address
}

// -- B.EQ instruction -- \\
// -- opcode: 0x06 -- \\
pub fn beq(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    if cpu.flags.zero_flag {
        cpu.pc = target_address
    }
}

// -- LDR instruction -- \\
// -- opcode: 0x07 -- \\
pub fn ldr(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)

    base_address := cpu.registers.read(rn)
    target_address := int(base_address + imm)

    if target_address + 8 > cpu.memory.len {
        panic('Out of bounds error caught at `ldr`.')
    }

    value := u64(cpu.memory[target_address]) |
             (u64(cpu.memory[target_address + 1]) << 8) |
             (u64(cpu.memory[target_address + 2]) << 16) |
             (u64(cpu.memory[target_address + 3]) << 24) |
             (u64(cpu.memory[target_address + 4]) << 32) |
             (u64(cpu.memory[target_address + 5]) << 40) |
             (u64(cpu.memory[target_address + 6]) << 48) |
             (u64(cpu.memory[target_address + 7]) << 56)

    cpu.registers.write(rd, value)
}

// -- STR instruction -- \\
// -- opcode: 0x08 -- \\
pub fn str(mut cpu CPU, instruction u32) {
    rt := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)

    base := cpu.registers.read(rn)
    addr := int(base + imm)
    if addr + 8 > cpu.memory.len {
        panic('STR: address overflow or out of bounds')
    }

    val := cpu.registers.read(rt)
    cpu.memory[addr]     = u8(val)
    cpu.memory[addr + 1] = u8(val >> 8)
    cpu.memory[addr + 2] = u8(val >> 16)
    cpu.memory[addr + 3] = u8(val >> 24)
    cpu.memory[addr + 4] = u8(val >> 32)
    cpu.memory[addr + 5] = u8(val >> 40)
    cpu.memory[addr + 6] = u8(val >> 48)
    cpu.memory[addr + 7] = u8(val >> 56)
}

// -- CMP instruction -- \\
// -- opcode: 0x09 -- \\
pub fn cmp(mut cpu CPU, instruction u32) {
    rn := int((instruction >> 8) & 0xFF)
    rm := int((instruction >> 16) & 0xFF)

    val1 := cpu.registers.read(rn)
    val2 := cpu.registers.read(rm)

    diff := val1 - val2
    did_borrow := val1 < val2

    cpu.flags.zero_flag = diff == 0
    cpu.flags.negative_flag = i64(diff) < 0
    cpu.flags.carry_flag = !did_borrow

    val1_i := i64(val1)
    val2_i := i64(val2)
    diff_i := i64(diff)
    cpu.flags.overflow_flag = (val1_i ^ val2_i) >= 0 && (val1_i ^ diff_i) < 0
}

// -- B.NE instruction -- \\
// -- opcode: 0x0A -- \\
pub fn bne(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    if !cpu.flags.zero_flag {
        cpu.pc = target_address
    }
}

// -- B.LT instruction -- \\
// -- opcode: 0x0B -- \\
pub fn blt(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    if cpu.flags.negative_flag != cpu.flags.overflow_flag {
        cpu.pc = target_address
    }
}

// -- B.LE instruction -- \\
// -- opcode: 0x0C -- \\
pub fn ble(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    if cpu.flags.zero_flag || cpu.flags.negative_flag != cpu.flags.overflow_flag {
        cpu.pc = target_address
    }
}

// -- B.GT instruction -- \\
// -- opcode: 0x0D -- \\
pub fn bgt(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    if !cpu.flags.zero_flag && cpu.flags.negative_flag == cpu.flags.overflow_flag {
        cpu.pc = target_address
    }
}

// -- B.GE instruction -- \\
// -- opcode: 0x0E -- \\
pub fn bge(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    if cpu.flags.negative_flag == cpu.flags.overflow_flag {
        cpu.pc = target_address
    }
}

// -- ADDI instruction -- \\
// -- opcode: 0x0F -- \\
pub fn addi(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    sum := cpu.registers.read(rn) + imm
    cpu.registers.write(rd, sum)
}

// -- SUBI instruction -- \\
// -- opcode: 0x10 -- \\
pub fn subi(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    diff := cpu.registers.read(rn) - imm
    cpu.registers.write(rd, diff)
}

// -- MULI instruction -- \\
// -- opcode: 0x11 -- \\
pub fn muli(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    prod := cpu.registers.read(rn) * imm
    cpu.registers.write(rd, prod)
}

// -- DIV instruction -- \\
// -- opcode: 0x12 -- \\
pub fn div(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    divisor := cpu.registers.read(rm)
    if divisor == 0 {
        panic('ZeroDivisionError: caught at div.')
    }
    quo := cpu.registers.read(rn) / divisor
    cpu.registers.write(rd, quo)
}

// -- DIVI instruction -- \\
// -- opcode: 0x13 -- \\
pub fn divi(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    if imm == 0 {
        panic('ZeroDivisionError: caught at divi.')
    }
    quo := cpu.registers.read(rn) / imm
    cpu.registers.write(rd, quo)
}

// -- REM instruction -- \\
// -- opcode: 0x14 -- \\
pub fn rem(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    divisor := cpu.registers.read(rm)
    if divisor == 0 {
        panic('ZeroDivisionError: caught at rem.')
    }
    r := cpu.registers.read(rn) % divisor
    cpu.registers.write(rd, r)
}

// -- REMI instruction -- \\
// -- opcode: 0x15 -- \\
pub fn remi(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    if imm == 0 {
        panic('ZeroDivisionError: caught at remi.')
    }
    r := cpu.registers.read(rn) % imm
    cpu.registers.write(rd, r)
}

// -- AND instruction -- \\
// -- opcode: 0x16 -- \\
pub fn and(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    val := cpu.registers.read(rn) & cpu.registers.read(rm)
    cpu.registers.write(rd, val)
}

// -- ANDI instruction -- \\
// -- opcode: 0x17 -- \\
pub fn andi(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    val := cpu.registers.read(rn) & imm
    cpu.registers.write(rd, val)
}

// -- ORR instruction -- \\
// -- opcode: 0x18 -- \\
pub fn orr(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    val := cpu.registers.read(rn) | cpu.registers.read(rm)
    cpu.registers.write(rd, val)
}

// -- ORRI instruction -- \\
// -- opcode: 0x19 -- \\
pub fn orri(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    val := cpu.registers.read(rn) | imm
    cpu.registers.write(rd, val)
}

// -- EOR instruction -- \\
// -- opcode: 0x1A -- \\
pub fn eor(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    val := cpu.registers.read(rn) ^ cpu.registers.read(rm)
    cpu.registers.write(rd, val)
}

// -- EORI instruction -- \\
// -- opcode: 0x1B -- \\
pub fn eori(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    val := cpu.registers.read(rn) ^ imm
    cpu.registers.write(rd, val)
}

// -- LSL instruction -- \\
// -- opcode: 0x1C -- \\
pub fn lsl(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    shift := u32(cpu.registers.read(rm) & 0x3F)
    val := cpu.registers.read(rn) << shift
    cpu.registers.write(rd, val)
}

// -- LSLI instruction -- \\
// -- opcode: 0x1D -- \\
pub fn lsli(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm_shift := u32((instruction >> 24) & 0x3F)
    val := cpu.registers.read(rn) << imm_shift
    cpu.registers.write(rd, val)
}

// -- LSR instruction -- \\
// -- opcode: 0x1E -- \\
pub fn lsr(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    shift := u32(cpu.registers.read(rm) & 0x3F)
    val := cpu.registers.read(rn) >> shift
    cpu.registers.write(rd, val)
}

// -- LSRI instruction -- \\
// -- opcode: 0x1F -- \\
pub fn lsri(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm_shift := u32((instruction >> 24) & 0x3F)
    val := cpu.registers.read(rn) >> imm_shift
    cpu.registers.write(rd, val)
}

// -- ASR instruction -- \\
// -- opcode: 0x20 -- \\
pub fn asr(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    shift := u32(cpu.registers.read(rm) & 0x3F)
    val := u64(i64(cpu.registers.read(rn)) >> shift)
    cpu.registers.write(rd, val)
}

// -- ASRI instruction -- \\
// -- opcode: 0x21 -- \\
pub fn asri(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm_shift := u32((instruction >> 24) & 0x3F)
    val := u64(i64(cpu.registers.read(rn)) >> imm_shift)
    cpu.registers.write(rd, val)
}

// -- BL instruction -- \\
// -- opcode: 0x22 -- \\
pub fn bl(mut cpu CPU, instruction u32) {
    target_address := u64((instruction >> 8) & 0xFFFFFF)
    cpu.registers.write(30, cpu.pc + 4)
    cpu.pc = target_address
}

// -- RET instruction -- \\
// -- opcode: 0x23 -- \\
pub fn ret(mut cpu CPU, instruction u32) {
    cpu.pc = cpu.registers.read(30)
}

// -- LDRB instruction -- \\
// -- opcode: 0x24 -- \\
pub fn ldrb(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    target_address := int(cpu.registers.read(rn) + imm)
    if target_address >= cpu.memory.len {
        panic('Out of bounds error caught at `ldrb`.')
    }
    value := u64(cpu.memory[target_address])
    cpu.registers.write(rd, value)
}

// -- STRB instruction -- \\
// -- opcode: 0x25 -- \\
pub fn strb(mut cpu CPU, instruction u32) {
    rt := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    value := u8(cpu.registers.read(rt))
    target_address := int(cpu.registers.read(rn) + imm)
    if target_address >= cpu.memory.len {
        panic('STRB: out of bounds')
    }
    cpu.memory[target_address] = value
}

// -- MOVR instruction -- \\
// -- opcode: 0x26 -- \\
pub fn movr(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    cpu.registers.write(rd, cpu.registers.read(rn))
}

// -- CMPI instruction -- \\
// -- opcode: 0x27 -- \\
pub fn cmpi(mut cpu CPU, instruction u32) {
    rn := int((instruction >> 8) & 0xFF)
    imm := u64((instruction >> 16) & 0xFFFF)
    val1 := cpu.registers.read(rn)
    diff := val1 - imm
    did_borrow := val1 < imm

    cpu.flags.zero_flag = diff == 0
    cpu.flags.negative_flag = i64(diff) < 0
    cpu.flags.carry_flag = !did_borrow

    val1_i := i64(val1)
    imm_i := i64(imm)
    diff_i := i64(diff)
    cpu.flags.overflow_flag = (val1_i ^ imm_i) >= 0 && (val1_i ^ diff_i) < 0
}

// -- ADDS instruction -- \\
// -- opcode: 0x28 -- \\
pub fn adds(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    val1 := cpu.registers.read(rn)
    val2 := cpu.registers.read(rm)
    sum := val1 + val2
    did_carry := sum < val1

    cpu.flags.zero_flag = sum == 0
    cpu.flags.negative_flag = i64(sum) < 0
    cpu.flags.carry_flag = did_carry

    val1_i := i64(val1)
    val2_i := i64(val2)
    sum_i := i64(sum)
    cpu.flags.overflow_flag = (val1_i ^ val2_i) < 0 && (val1_i ^ sum_i) >= 0
    cpu.registers.write(rd, sum)
}

// -- SUBS instruction -- \\
// -- opcode: 0x29 -- \\
pub fn subs(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    rm := int((instruction >> 24) & 0xFF)
    val1 := cpu.registers.read(rn)
    val2 := cpu.registers.read(rm)
    diff := val1 - val2
    did_borrow := val1 < val2

    cpu.flags.zero_flag = diff == 0
    cpu.flags.negative_flag = i64(diff) < 0
    cpu.flags.carry_flag = !did_borrow

    val1_i := i64(val1)
    val2_i := i64(val2)
    diff_i := i64(diff)
    cpu.flags.overflow_flag = (val1_i ^ val2_i) >= 0 && (val1_i ^ diff_i) < 0
    cpu.registers.write(rd, diff)
}

// -- ADDIS instruction -- \\
// -- opcode: 0x2A -- \\
pub fn addis(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    val1 := cpu.registers.read(rn)
    sum := val1 + imm
    did_carry := sum < val1

    cpu.flags.zero_flag = sum == 0
    cpu.flags.negative_flag = i64(sum) < 0
    cpu.flags.carry_flag = did_carry

    val1_i := i64(val1)
    imm_i := i64(imm)
    sum_i := i64(sum)
    cpu.flags.overflow_flag = (val1_i ^ imm_i) < 0 && (val1_i ^ sum_i) >= 0
    cpu.registers.write(rd, sum)
}

// -- SUBIS instruction -- \\
// -- opcode: 0x2B -- \\
pub fn subis(mut cpu CPU, instruction u32) {
    rd := int((instruction >> 8) & 0xFF)
    rn := int((instruction >> 16) & 0xFF)
    imm := u64((instruction >> 24) & 0xFF)
    val1 := cpu.registers.read(rn)
    diff := val1 - imm
    did_borrow := val1 < imm

    cpu.flags.zero_flag = diff == 0
    cpu.flags.negative_flag = i64(diff) < 0
    cpu.flags.carry_flag = !did_borrow

    val1_i := i64(val1)
    imm_i := i64(imm)
    diff_i := i64(diff)
    cpu.flags.overflow_flag = (val1_i ^ imm_i) >= 0 && (val1_i ^ diff_i) < 0
    cpu.registers.write(rd, diff)
}

pub type InstructionFn = fn (mut cpu CPU, instruction u32)

pub const instruction_handlers = []InstructionFn[
    halt,
    mov,
    add,
    sub,
    mul,
    b,
    beq,
    ldr,
    str,
    cmp,
    bne,
    blt,
    ble,
    bgt,
    bge,
    addi,
    subi,
    muli,
    div,
    divi,
    rem,
    remi,
    and,
    andi,
    orr,
    orri,
    eor,
    eori,
    lsl,
    lsli,
    lsr,
    lsri,
    asr,
    asri,
    bl,
    ret,
    ldrb,
    strb,
    movr,
    cmpi,
    adds,
    subs,
    addis,
    subis,
]
