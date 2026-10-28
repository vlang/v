module main

// halt executes HALT (opcode 0x00).
pub fn halt(mut cpu CPU, instruction u32) {
	cpu.state.is_running = false
}

// mov executes MOV (opcode 0x01).
pub fn mov(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	imm := u64((instruction >> 16) & 0xFFFF)
	cpu.registers.write(rd, imm)
}

// add executes ADD (opcode 0x02).
pub fn add(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	sum := cpu.registers.read(rn) + cpu.registers.read(rm)
	cpu.registers.write(rd, sum)
}

// sub executes SUB (opcode 0x03).
pub fn sub(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	diff := cpu.registers.read(rn) - cpu.registers.read(rm)
	cpu.registers.write(rd, diff)
}

// mul executes MUL (opcode 0x04).
pub fn mul(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	prod := cpu.registers.read(rn) * cpu.registers.read(rm)
	cpu.registers.write(rd, prod)
}

// b executes B (opcode 0x05).
pub fn b(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	cpu.jump(target_address)
}

// beq executes B.EQ (opcode 0x06).
pub fn beq(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	if cpu.flags.zero_flag {
		cpu.jump(target_address)
	}
}

// ldr executes LDR (opcode 0x07).
pub fn ldr(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)

	base_address := cpu.registers.read(rn)
	target_address := cpu.memory_address(base_address, imm, 8)

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

// str executes STR (opcode 0x08).
pub fn str(mut cpu CPU, instruction u32) {
	rt := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)

	base := cpu.registers.read(rn)
	addr := cpu.memory_address(base, imm, 8)

	val := cpu.registers.read(rt)
	cpu.memory[addr] = u8(val)
	cpu.memory[addr + 1] = u8(val >> 8)
	cpu.memory[addr + 2] = u8(val >> 16)
	cpu.memory[addr + 3] = u8(val >> 24)
	cpu.memory[addr + 4] = u8(val >> 32)
	cpu.memory[addr + 5] = u8(val >> 40)
	cpu.memory[addr + 6] = u8(val >> 48)
	cpu.memory[addr + 7] = u8(val >> 56)
}

// cmp executes CMP (opcode 0x09).
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
	cpu.flags.overflow_flag = (val1_i ^ val2_i) < 0 && (val1_i ^ diff_i) < 0
}

// bne executes B.NE (opcode 0x0A).
pub fn bne(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	if !cpu.flags.zero_flag {
		cpu.jump(target_address)
	}
}

// blt executes B.LT (opcode 0x0B).
pub fn blt(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	if cpu.flags.negative_flag != cpu.flags.overflow_flag {
		cpu.jump(target_address)
	}
}

// ble executes B.LE (opcode 0x0C).
pub fn ble(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	if cpu.flags.zero_flag || cpu.flags.negative_flag != cpu.flags.overflow_flag {
		cpu.jump(target_address)
	}
}

// bgt executes B.GT (opcode 0x0D).
pub fn bgt(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	if !cpu.flags.zero_flag && cpu.flags.negative_flag == cpu.flags.overflow_flag {
		cpu.jump(target_address)
	}
}

// bge executes B.GE (opcode 0x0E).
pub fn bge(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	if cpu.flags.negative_flag == cpu.flags.overflow_flag {
		cpu.jump(target_address)
	}
}

// addi executes ADDI (opcode 0x0F).
pub fn addi(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	sum := cpu.registers.read(rn) + imm
	cpu.registers.write(rd, sum)
}

// subi executes SUBI (opcode 0x10).
pub fn subi(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	diff := cpu.registers.read(rn) - imm
	cpu.registers.write(rd, diff)
}

// muli executes MULI (opcode 0x11).
pub fn muli(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	prod := cpu.registers.read(rn) * imm
	cpu.registers.write(rd, prod)
}

// div executes DIV (opcode 0x12).
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

// divi executes DIVI (opcode 0x13).
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

// rem executes REM (opcode 0x14).
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

// remi executes REMI (opcode 0x15).
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

// and executes AND (opcode 0x16).
pub fn and(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	val := cpu.registers.read(rn) & cpu.registers.read(rm)
	cpu.registers.write(rd, val)
}

// andi executes ANDI (opcode 0x17).
pub fn andi(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	val := cpu.registers.read(rn) & imm
	cpu.registers.write(rd, val)
}

// orr executes ORR (opcode 0x18).
pub fn orr(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	val := cpu.registers.read(rn) | cpu.registers.read(rm)
	cpu.registers.write(rd, val)
}

// orri executes ORRI (opcode 0x19).
pub fn orri(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	val := cpu.registers.read(rn) | imm
	cpu.registers.write(rd, val)
}

// eor executes EOR (opcode 0x1A).
pub fn eor(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	val := cpu.registers.read(rn) ^ cpu.registers.read(rm)
	cpu.registers.write(rd, val)
}

// eori executes EORI (opcode 0x1B).
pub fn eori(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	val := cpu.registers.read(rn) ^ imm
	cpu.registers.write(rd, val)
}

// lsl executes LSL (opcode 0x1C).
pub fn lsl(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	shift := u32(cpu.registers.read(rm) & 0x3F)
	val := cpu.registers.read(rn) << shift
	cpu.registers.write(rd, val)
}

// lsli executes LSLI (opcode 0x1D).
pub fn lsli(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm_shift := u32((instruction >> 24) & 0x3F)
	val := cpu.registers.read(rn) << imm_shift
	cpu.registers.write(rd, val)
}

// lsr executes LSR (opcode 0x1E).
pub fn lsr(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	shift := u32(cpu.registers.read(rm) & 0x3F)
	val := cpu.registers.read(rn) >> shift
	cpu.registers.write(rd, val)
}

// lsri executes LSRI (opcode 0x1F).
pub fn lsri(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm_shift := u32((instruction >> 24) & 0x3F)
	val := cpu.registers.read(rn) >> imm_shift
	cpu.registers.write(rd, val)
}

// asr executes ASR (opcode 0x20).
pub fn asr(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	rm := int((instruction >> 24) & 0xFF)
	shift := u32(cpu.registers.read(rm) & 0x3F)
	val := u64(i64(cpu.registers.read(rn)) >> shift)
	cpu.registers.write(rd, val)
}

// asri executes ASRI (opcode 0x21).
pub fn asri(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm_shift := u32((instruction >> 24) & 0x3F)
	val := u64(i64(cpu.registers.read(rn)) >> imm_shift)
	cpu.registers.write(rd, val)
}

// bl executes BL (opcode 0x22).
pub fn bl(mut cpu CPU, instruction u32) {
	target_address := u64((instruction >> 8) & 0xFFFFFF)
	cpu.registers.write(30, cpu.pc + 4)
	cpu.jump(target_address)
}

// ret executes RET (opcode 0x23).
pub fn ret(mut cpu CPU, instruction u32) {
	cpu.jump(cpu.registers.read(30))
}

// ldrb executes LDRB (opcode 0x24).
pub fn ldrb(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	target_address := cpu.memory_address(cpu.registers.read(rn), imm, 1)
	value := u64(cpu.memory[target_address])
	cpu.registers.write(rd, value)
}

// strb executes STRB (opcode 0x25).
pub fn strb(mut cpu CPU, instruction u32) {
	rt := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	imm := u64((instruction >> 24) & 0xFF)
	value := u8(cpu.registers.read(rt))
	target_address := cpu.memory_address(cpu.registers.read(rn), imm, 1)
	cpu.memory[target_address] = value
}

// movr executes MOVR (opcode 0x26).
pub fn movr(mut cpu CPU, instruction u32) {
	rd := int((instruction >> 8) & 0xFF)
	rn := int((instruction >> 16) & 0xFF)
	cpu.registers.write(rd, cpu.registers.read(rn))
}

// cmpi executes CMPI (opcode 0x27).
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
	cpu.flags.overflow_flag = (val1_i ^ imm_i) < 0 && (val1_i ^ diff_i) < 0
}

// adds executes ADDS (opcode 0x28).
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
	cpu.flags.overflow_flag = (val1_i ^ val2_i) >= 0 && (val1_i ^ sum_i) < 0
	cpu.registers.write(rd, sum)
}

// subs executes SUBS (opcode 0x29).
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
	cpu.flags.overflow_flag = (val1_i ^ val2_i) < 0 && (val1_i ^ diff_i) < 0
	cpu.registers.write(rd, diff)
}

// addis executes ADDIS (opcode 0x2A).
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
	cpu.flags.overflow_flag = (val1_i ^ imm_i) >= 0 && (val1_i ^ sum_i) < 0
	cpu.registers.write(rd, sum)
}

// subis executes SUBIS (opcode 0x2B).
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
	cpu.flags.overflow_flag = (val1_i ^ imm_i) < 0 && (val1_i ^ diff_i) < 0
	cpu.registers.write(rd, diff)
}

// InstructionFn implements one decoded instruction.
pub type InstructionFn = fn (mut cpu CPU, instruction u32)

// instruction_handlers maps the low opcode byte to its implementation.
pub const instruction_handlers = [InstructionFn(halt), mov, add, sub, mul, b, beq, ldr, str, cmp,
	bne, blt, ble, bgt, bge, addi, subi, muli, div, divi, rem, remi, and, andi, orr, orri, eor,
	eori, lsl, lsli, lsr, lsri, asr, asri, bl, ret, ldrb, strb, movr, cmpi, adds, subs, addis,
	subis]
