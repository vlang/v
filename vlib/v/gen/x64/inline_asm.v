// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import v.ssa

// Structured inline-asm lowering for the x64 native backend.
//
// V assembly templates survive the frontend as source lines with named
// operand references: this pass understands the instruction shapes the builtin
// runtime uses (syscall trampolines plus the math/bits mul/div/add family),
// keyed off the destination-first syntax GNU consumes after the C backend
// lowers it. Anything else fails the build with an explicit message instead
// of being silently dropped.
//
// Safety model against the register allocator:
// - the allocator pins long-lived values in rbx/r12/r13/r14 (callee saved),
//   and the scratch pool below never claims those registers,
// - `syscall` kills rax/rcx/r11, and mul/div kill rax/rdx plus the flags, so
//   the current-register cache is cleared around those lines; operands
//   re-place themselves on demand from their stack slots.
//
// Operand layout (built together with the spec's io order in the SSA builder):
// the inputs ride the instruction operands first, then one alloca pointer per
// secondary output; the first output defines the instruction's result value.

// asm_register_names maps structured register spellings to system-V amd64
// numbers (rax=0, rsp=4, rbp=5). The ordering matches v.gen.x64.asm's Reg
// constants so the raw encoders can be reused directly.
const asm_register_names = ['rax', 'rcx', 'rdx', 'rbx', 'rsp', 'rbp', 'rsi', 'rdi', 'r8', 'r9',
	'r10', 'r11', 'r12', 'r13', 'r14', 'r15']

// asm_scratch_regs lists the volatile GPRs worth claiming for `r`/`g`
// operands: rdx, rsi, rdi, rcx, r8..r11. rsp/rbp are frame-pinned and
// rbx/r12..r15 carry allocator pins, so they never appear here.
const asm_scratch_regs = [2, 6, 7, 1, 8, 9, 10, 11]

// AsmLowerState tracks which physical register currently holds each asm
// operand value while a block is being emitted, plus the registers the block
// reserved so lazy scratch picks stay clash-free.
struct AsmLowerState {
mut:
	// asm operand value id -> physical register it currently occupies
	operand_regs map[int]int
	// physical registers reserved by this block
	reserved []int
	// output io index -> physical register the line sequence left it in
	output_regs map[int]int
}

// constraint_reg maps a fixed GCC-style register constraint to a register.
fn constraint_reg(constraint string) ?Reg {
	return match constraint {
		'a', '=a', '+a' { Reg(0) } // rax
		'c', '=c', '+c' { Reg(1) } // rcx
		'd', '=d', '+d' { Reg(2) } // rdx
		'S', '=S', '+S' { Reg(6) } // rsi
		'D', '=D', '+D' { Reg(7) } // rdi
		else { none }
	}
}

// gen_inline_asm lowers one .inline_asm instruction into machine code.
fn (mut g Gen) gen_inline_asm(val_id int) {
	instr := g.m.instrs[g.m.values[val_id].index]
	spec := g.m.asm_spec(&instr) or {
		panic('x64: inline asm instruction ${val_id} has no spec')
	}
	if spec.arch != 'amd64' {
		panic('x64: inline asm blocks for `${spec.arch}` are not supported by the x64 backend')
	}
	mut st := AsmLowerState{
		operand_regs: map[int]int{}
		output_regs:  map[int]int{}
	}
	// Place every input up front, by constraint. Template `mov` lines may
	// re-place an operand afterwards; the placement helper records that.
	for i, io in spec.ios {
		if io.constraint.starts_with('=') {
			continue
		}
		val_id_i := asm_operand_value(i, spec, &instr, val_id)
		reg := st.asm_input_reg(spec, i, io.constraint, val_id_i)
		st.reserved << int(reg)
		g.asm_place_value(mut st, reg, val_id_i)
	}
	for i, line in spec.insts {
		g.lower_x64_asm_line(mut st, line, spec, instr, val_id, i)
	}
	// The outputs land in the registers the lines defined; the first output
	// is the instruction's result value, further outputs go through their
	// pointer operands.
	for j, io in spec.ios {
		if !io.constraint.starts_with('=') {
			continue
		}
		reg := if known := st.output_regs[j] {
			Reg(known)
		} else if fixed := constraint_reg(io.constraint) {
			fixed
		} else {
			Reg(0) // generic outputs land in rax
		}
		if j == 0 && instr.typ > 0 {
			g.store_reg_to_val(int(reg), val_id)
			continue
		}
		if j > 0 {
			ptr_id := asm_output_ptr_operand(j, &instr, spec)
			scratch := st.pick_scratch()
			g.load_val_to_reg(int(scratch), ptr_id)
			asm_mov_mem_base_reg(mut g, scratch, reg)
		}
	}
}

// asm_input_reg resolves the physical register an input's constraint asks for.
fn (mut st AsmLowerState) asm_input_reg(spec &ssa.InlineAsmSpec, io_index int, constraint string, val_id_i int) Reg {
	if digit_reg := digit_constraint_reg(spec, constraint) {
		return digit_reg
	}
	if fixed_reg := constraint_reg(constraint) {
		return fixed_reg
	}
	if constraint in ['r', 'g'] {
		if cur := st.operand_regs[val_id_i] {
			// parked by a template line: keep the position
			return Reg(cur)
		}
		return st.pick_scratch()
	}
	panic('x64: inline asm input constraint `${constraint}` is not supported by the x64 backend')
}

// asm_place_value loads an operand value into reg unless it is already there,
// updating the current-register cache. Pinned values copy across through the
// existing value helpers.
fn (mut g Gen) asm_place_value(mut st AsmLowerState, reg Reg, val_id int) {
	if val_id <= 0 {
		return
	}
	if cur := st.operand_regs[val_id] {
		if cur == int(reg) {
			return
		}
	}
	g.load_val_to_reg(int(reg), val_id)
	st.operand_regs[val_id] = int(reg)
}

// lower_x64_asm_line lowers a single template line.
fn (mut g Gen) lower_x64_asm_line(mut st AsmLowerState, line string, spec &ssa.InlineAsmSpec, instr ssa.Instruction, val_id int, line_index int) {
	mnemonic, operand_text := asm_split_instruction(line)
	asm_operands := asm_split_operands(operand_text)
	match mnemonic {
		'syscall' {
			// syscall: 0F 05; inputs were placed by the constraint pre-pass
			// and the mov lines.
			g.emit(0x0F)
			g.emit(0x05)
			// rcx, r11 and the memory are clobbered; re-placement is lazy.
			st.operand_regs.clear()
		}
		'mov', 'movq' {
			if asm_operands.len != 2 {
				panic('x64: inline asm `mov` expects two operands: ${line}')
			}
			g.lower_x64_asm_mov(mut st, asm_operands[0], asm_operands[1], spec, instr, val_id)
		}
		'mulq', 'imulq', 'divq', 'idivq', 'div', 'idiv' {
			if asm_operands.len != 1 {
				panic('x64: inline asm `${mnemonic}` expects one operand: ${line}')
			}
			reg := st.asm_instruction_operand_reg(asm_operands[0], spec, instr, val_id)
			op_ext := match mnemonic {
				'mulq' { 4 }
				'imulq' { 5 }
				'divq', 'div' { 6 }
				'idivq', 'idiv' { 7 }
				else { 4 }
			}
			// F7 /4..7, modrm 11 op rm (REX.W + REX.B for r8..r15)
			asm_one_operand_reg_op(mut g, 0xF7, op_ext, reg)
			// This line defined both result halves: rax and rdx. Sequential
			// lines (addq/adcq) refine them afterwards.
			for j, io in spec.ios {
				if !io.constraint.starts_with('=') {
					continue
				}
				st.output_regs[j] = (if j % 2 == 0 { int(Reg(0)) } else { int(Reg(2)) })
			}
			st.operand_regs.clear()
		}
		'addq', 'adcq', 'subq', 'xorq', 'orq', 'andq' {
			if asm_operands.len != 2 {
				panic('x64: inline asm `${mnemonic}` expects two operands: ${line}')
			}
			g.lower_x64_asm_regop(mut st, mnemonic, asm_operands[0], asm_operands[1], spec, instr,
				val_id)
		}
		'nop' {
			g.emit(0x90)
		}
		else {
			panic('x64: inline asm instruction `${mnemonic}` (line ${line_index + 1}) is not supported by the x64 backend: ${line}')
		}
	}
}

// lower_x64_asm_mov lowers `mov`'s meaningful pairings: reg,reg; reg,value
// (template placement); value,reg (write a register into a named output) and
// value,value (copy through scratch).
fn (mut g Gen) lower_x64_asm_mov(mut st AsmLowerState, dst string, src string, spec &ssa.InlineAsmSpec, instr ssa.Instruction, val_id int) {
	dst_reg := asm_register_of(dst)
	src_reg := asm_register_of(src)
	dst_val := asm_alias_value(dst, spec, instr, val_id)
	src_val := asm_alias_value(src, spec, instr, val_id)
	if dst_reg != none && src_reg != none {
		// register-to-register copy: no operand translation needed
		asm_mov_reg_reg(mut g, dst_reg, src_reg)
		return
	}
	if dst_reg != none && src_val > 0 {
		g.asm_place_value(mut st, dst_reg, src_val)
		return
	}
	if dst_val > 0 && src_reg != none {
		// write a register result into the named output operand
		if dst_val == val_id {
			if instr.typ > 0 {
				g.store_reg_to_val(int(src_reg), val_id)
			}
		} else {
			io_index := asm_output_index_of_alias(dst, spec)
			st.output_regs[io_index] = int(src_reg)
		}
		st.operand_regs[dst_val] = int(src_reg)
		return
	}
	if dst_val > 0 && src_val > 0 {
		// value to value copy through a scratch register
		scratch := st.pick_scratch()
		g.load_val_to_reg(int(scratch), src_val)
		g.store_reg_to_val(int(scratch), dst_val)
		st.operand_regs[dst_val] = int(scratch)
		return
	}
	panic('x64: unsupported inline asm `mov` operand form: `mov ${dst}, ${src}`')
}

// lower_x64_asm_regop lowers add/adc/sub/xor/or/and with 64-bit register
// shapes: `opq reg, reg`, `opq reg, value` and `opq reg, imm8`.
fn (mut g Gen) lower_x64_asm_regop(mut st AsmLowerState, mnemonic string, dst string, src string, spec &ssa.InlineAsmSpec, instr ssa.Instruction, val_id int) {
	dst_reg := asm_register_of(dst) or {
		panic('x64: inline asm `${mnemonic}` destination must be a register: ${mnemonic} ${dst}, ${src}')
	}
	if imm := asm_small_imm_of(src) {
		// 83 /opext imm8
		asm_imm8_reg_op(mut g, mnemonic, dst_reg, imm)
		return
	}
	src_val := asm_alias_value(src, spec, instr, val_id)
	mut src_reg := asm_register_of(src)
	if src_reg == none && src_val > 0 {
		if cur := st.operand_regs[src_val] {
			src_reg = Reg(cur)
		} else {
			scratch := st.pick_scratch()
			g.asm_place_value(mut st, scratch, src_val)
			src_reg = scratch
		}
	}
	if src_reg == none {
		panic('x64: unsupported inline asm operand for `${mnemonic}`: ${src}')
	}
	asm_two_reg_op(mut g, mnemonic, dst_reg, src_reg)
}

// asm_small_imm_of parses a plain integer immediate, none when the token has
// other shape.
fn asm_small_imm_of(token string) ?i64 {
	name := token.trim_space()
	if !(name[0].is_digit() || (name[0] == `-` && name.len > 1)) {
		return none
	}
	return name.i64()
}

// asm_instruction_operand_reg resolves a one-operand instruction's argument
// to a register: a literal register or an operand's current register.
fn (mut st AsmLowerState) asm_instruction_operand_reg(token string, spec &ssa.InlineAsmSpec, instr ssa.Instruction, val_id int) Reg {
	if reg := asm_register_of(token) {
		return reg
	}
	val_id_i := asm_alias_value(token, spec, instr, val_id)
	if cur := st.operand_regs[val_id_i] {
		return Reg(cur)
	}
	panic('x64: inline asm operand ${token} has no current register; place it with a mov first')
}

// asm_output_index_of_alias finds the io index that declares an alias, for
// tracking the register an output lands in.
fn asm_output_index_of_alias(token string, spec &ssa.InlineAsmSpec) int {
	target := token.trim_space()
	for i, io in spec.ios {
		if io.alias.len > 0 && io.alias == target {
			return i
		}
	}
	panic('x64: inline asm output alias ${target} is not declared')
}

// asm_operand_value returns the value id for io index i: the instruction's
// result for the first output, the input operand list for inputs. Secondary
// outputs ride as pointer operands (see the builder's layout note); the emit
// uses asm_output_ptr_operand for those.
fn asm_operand_value(io_index int, spec &ssa.InlineAsmSpec, instr &ssa.Instruction, val_id int) int {
	if io_index < spec.ios.len {
		io := spec.ios[io_index]
		if !io.constraint.starts_with('=') {
			input_rank := spec.asm_input_start()
			if io_index >= input_rank && io_index - input_rank < instr.operands.len {
				return int(instr.operands[io_index - input_rank])
			}
			return 0
		}
	}
	if io_index == 0 {
		return val_id
	}
	return 0
}

// asm_output_ptr_operand returns the pointer operand id of a secondary output.
fn asm_output_ptr_operand(output_index int, instr &ssa.Instruction, spec &ssa.InlineAsmSpec) int {
	input_count := spec.asm_input_count()
	slot := (output_index - 1)
	if slot < 0 || slot >= instr.operands.len - input_count {
		panic('x64: inline asm secondary output ${output_index} has no pointer operand')
	}
	return int(instr.operands[input_count + slot])
}

// asm_alias_value resolves a template token to the operand value it names,
// 0 when the token is not an io alias.
fn asm_alias_value(token string, spec &ssa.InlineAsmSpec, instr &ssa.Instruction, val_id int) int {
	target := token.trim_space()
	for i, io in spec.ios {
		if io.alias.len > 0 && io.alias == target {
			return asm_operand_value(i, spec, instr, val_id)
		}
	}
	return 0
}

// asm_register_of resolves a template token to a register if it names one.
fn asm_register_of(token string) ?Reg {
	name := token.trim_space()
	for r, reg_name in asm_register_names {
		if name == reg_name {
			return Reg(r)
		}
	}
	return none
}

// asm_split_instruction splits a template line into its mnemonic and the rest.
fn asm_split_instruction(line string) (string, string) {
	trimmed := line.trim_space()
	for i, c in trimmed {
		if c.is_space() {
			return trimmed[..i], trimmed[i + 1..].trim_space()
		}
	}
	return trimmed, ''
}

// asm_split_operands splits a comma-separated template operand list.
fn asm_split_operands(s string) []string {
	mut res := []string{}
	if s.len == 0 {
		return res
	}
	for part in s.split(',') {
		res << part.trim_space()
	}
	return res
}

// pick_scratch claims a free scratch register for this block.
fn (mut st AsmLowerState) pick_scratch() Reg {
	for reg_i in asm_scratch_regs {
		if reg_i in st.reserved {
			continue
		}
		mut taken := false
		for _, cur in st.operand_regs {
			if cur == reg_i {
				taken = true
				break
			}
		}
		if taken {
			continue
		}
		for _, cur in st.output_regs {
			if cur == reg_i {
				taken = true
				break
			}
		}
		if taken {
			continue
		}
		st.reserved << reg_i
		return Reg(reg_i)
	}
	panic('x64: inline asm ran out of scratch registers')
}

// digit_constraint_reg resolves matching-digit constraints (GCC's operand-rank
// form). Only small digits are meaningful here: digit n names output operand n
// of the same block, whose register this backend assigns by constraint.
fn digit_constraint_reg(spec &ssa.InlineAsmSpec, constraint string) ?Reg {
	if constraint.len != 1 || constraint[0] < `0` || constraint[0] > `5` {
		return none
	}
	operand_index := int(constraint[0] - `0`)
	if operand_index >= spec.ios.len {
		return none
	}
	out := spec.ios[operand_index].constraint
	if !out.starts_with('=') {
		return none
	}
	if reg := constraint_reg(out) {
		return reg
	}
	return Reg(0) // generic outputs land in rax, the defined value slot
}

// ========== raw encodings for the supported instruction shapes ==========

// asm_muldiv_op_exts maps the r/m64 multiply/divide family onto their /x
// extension bits under F7.
const asm_muldiv_op_exts = {
	'mulq':  4
	'imulq': 5
	'divq':  6
	'idivq': 7
	'div':   6
	'idiv':  7
}

// asm_regop_opcodes maps the reg,r64 forms: op r/m64, r64.
const asm_regop_opcodes = {
	'addq': 0x01
	'orq':  0x09
	'adcq': 0x11
	'andq': 0x21
	'subq': 0x29
	'xorq': 0x31
}

// asm_imm8_op_exts maps the 83 /x imm8 forms.
const asm_imm8_op_exts = {
	'addq': 0
	'orq':  1
	'adcq': 2
	'andq': 4
	'subq': 5
	'xorq': 6
}

// asm_one_operand_reg_op emits `F7 /x r64` for the multiply/divide family.
fn asm_one_operand_reg_op(mut g Gen, op_byte u8, op_ext int, reg Reg) {
	mut rex := u8(0x48)
	if reg >= 8 {
		rex |= 1 // REX.B
	}
	g.emit(rex)
	g.emit(op_byte)
	g.emit(u8(0xC0 | (op_ext << 3) | (int(reg) & 7)))
}

// asm_two_reg_op emits `op r/m64, r64` with the destination-first form 89's
// reg mapping for the add-family.
fn asm_two_reg_op(mut g Gen, mnemonic string, dst Reg, src Reg) {
	op_byte := asm_regop_opcodes[mnemonic] or {
		panic('x64: inline asm `${mnemonic}` has no register encoding table entry')
	}
	mut rex := u8(0x48)
	if int(src) >= 8 {
		rex |= 0x04 // REX.R
	}
	if int(dst) >= 8 {
		rex |= 0x01 // REX.B
	}
	g.emit(rex)
	g.emit(u8(op_byte))
	g.emit(u8(0xC0 | ((int(src) & 7) << 3) | (int(dst) & 7)))
}

// asm_imm8_reg_op emits `83 /x imm8 op r64` for the add-family.
fn asm_imm8_reg_op(mut g Gen, mnemonic string, dst Reg, imm i64) {
	op_ext := asm_imm8_op_exts[mnemonic] or {
		panic('x64: inline asm `${mnemonic}` has no imm8 encoding table entry')
	}
	mut rex := u8(0x48)
	if int(dst) >= 8 {
		rex |= 0x01 // REX.B
	}
	g.emit(rex)
	g.emit(0x83)
	g.emit(u8(0xC0 | (op_ext << 3) | (int(dst) & 7)))
	g.emit(u8(imm & 0xFF))
}

// asm_mov_mem_base_reg emits `mov [base], src` for the 64-bit register form.
fn asm_mov_mem_base_reg(mut g Gen, base Reg, src_reg Reg) {
	mut rex := u8(0x48)
	if int(src_reg) >= 8 {
		rex |= 0x04 // REX.R
	}
	if int(base) >= 8 {
		rex |= 0x01 // REX.B
	}
	g.emit(rex)
	g.emit(0x89)
	g.emit(u8(((int(src_reg) & 7) << 3) | (int(base) & 7)))
}
