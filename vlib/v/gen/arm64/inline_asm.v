// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

import v.ssa

// Structured inline-asm lowering for the arm64 native backend.
//
// The SSA builder turns each `asm arm64 { ... }` block into an .inline_asm
// instruction. Its operands are the block's inputs, followed by one pointer per
// secondary output. The template lines are lowered in order:
//
// - operand values stay in their stack slots. A register only holds a copy made
//   while the block runs, so the block never depends on what the surrounding
//   code left in registers.
// - a physical register named by the template is never handed out as scratch,
//   and an output's home register is reserved for the whole block.
// - `svc` returns in x0, so a copy cached there is dropped after it.

// asm_arm64_pool lists the registers handed out for operand values. x16 to x18,
// x29 and x30 belong to the ABI and stay out.
const asm_arm64_pool = [9, 10, 11, 12, 13, 14, 15, 1, 2, 3, 4, 5, 6, 7, 8]

// ArmAsmKind classifies a resolved template operand.
enum ArmAsmKind {
	phys
	imm
	input
	output
}

// ArmAsmOperand is one resolved template operand.
struct ArmAsmOperand {
	kind ArmAsmKind
	reg  int
	imm  i64
	val  int
	io   int
}

// ArmAsmState tracks the registers used while one block is lowered.
struct ArmAsmState {
mut:
	// operand value id -> register holding a copy of it
	val_regs map[int]int
	// output io index -> home register that receives the output
	homes map[int]int
	// registers the block must not hand out as scratch
	taken []bool
}

// gen_inline_asm lowers one .inline_asm instruction.
fn (mut g Gen) gen_inline_asm(val_id int) {
	instr := g.m.instrs[g.m.values[val_id].index]
	spec := g.m.asm_spec(&instr) or {
		panic('arm64: inline asm instruction ${val_id} has no spec')
	}
	if spec.arch != 'arm64' {
		panic('arm64: inline asm blocks for `${spec.arch}` are not supported by the arm64 backend')
	}
	mut st := ArmAsmState{
		val_regs: map[int]int{}
		homes:    map[int]int{}
		taken:    []bool{len: 32}
	}
	for line in spec.insts {
		arm64_asm_mark_registers(line, mut st.taken)
	}
	input_vals := arm64_asm_input_values(&instr, spec)
	for line in spec.insts {
		g.lower_arm64_asm_line(mut st, line, spec, input_vals)
	}
	g.finish_arm64_asm_outputs(mut st, &instr, val_id, spec)
}

// arm64_asm_input_values returns the operand value of every input, in io order.
fn arm64_asm_input_values(instr &ssa.Instruction, spec &ssa.InlineAsmSpec) []int {
	mut res := []int{}
	for io in spec.ios {
		if !io.constraint.starts_with('=') {
			res << int(instr.operands[res.len])
		}
	}
	return res
}

// arm64_asm_mark_registers marks the x registers a template names.
fn arm64_asm_mark_registers(line string, mut taken []bool) {
	mut start := -1
	for i := 0; i <= line.len; i++ {
		c := if i < line.len { line[i] } else { u8(` `) }
		if c.is_alnum() || c == `_` {
			if start < 0 {
				start = i
			}
			continue
		}
		if start >= 0 {
			word := line[start..i]
			if word.len >= 2 && word[0] == `x` && word[1..].bytes().all(it.is_digit()) {
				n := word[1..].int()
				if n < 31 {
					taken[n] = true
				}
			}
			start = -1
		}
	}
}

// lower_arm64_asm_line lowers one template line.
fn (mut g Gen) lower_arm64_asm_line(mut st ArmAsmState, line string, spec &ssa.InlineAsmSpec, input_vals []int) {
	mnemonic, operand_text := arm64_asm_split_instruction(line)
	ops := arm64_asm_split_operands(operand_text)
	match mnemonic {
		'mov' {
			if ops.len != 2 {
				panic('arm64: inline asm `mov` expects two operands: ${line}')
			}
			g.lower_arm64_asm_mov(mut st, ops[0], ops[1], spec, input_vals)
		}
		'svc' {
			imm := if ops.len > 0 { arm64_asm_imm(ops[0]) } else { i64(0) }
			g.emit32(asm_svc(u32(imm)))
			st.drop_register(0)
		}
		'nop' {
			g.emit32(asm_nop())
		}
		else {
			if ops.len != 3 {
				panic('arm64: inline asm instruction `${mnemonic}` expects three operands: ${line}')
			}
			dst := st.resolve(ops[0], spec, input_vals)
			src_a := st.resolve(ops[1], spec, input_vals)
			src_b := st.resolve(ops[2], spec, input_vals)
			a := st.read_register(mut g, src_a)
			b := st.read_register(mut g, src_b)
			d := st.write_register(mut g, dst)
			g.emit32(arm64_asm_alu_word(mnemonic, d, a, b))
			st.wrote(dst, d)
		}
	}
}

// lower_arm64_asm_mov lowers `mov dst, src` for physical and output destinations.
fn (mut g Gen) lower_arm64_asm_mov(mut st ArmAsmState, dst_text string, src_text string, spec &ssa.InlineAsmSpec, input_vals []int) {
	dst := st.resolve(dst_text, spec, input_vals)
	src := st.resolve(src_text, spec, input_vals)
	if dst.kind != .phys && dst.kind != .output {
		panic('arm64: inline asm `mov` destination must be a register: ${dst_text}')
	}
	d := st.write_register(mut g, dst)
	match src.kind {
		.imm {
			g.emit_mov_imm(d, src.imm)
		}
		.phys, .output {
			s := st.read_register(mut g, src)
			if s != d {
				g.emit32(asm_mov_reg(Reg(d), Reg(s)))
			}
		}
		.input {
			if cached := st.val_regs[src.val] {
				if cached != d {
					g.emit32(asm_mov_reg(Reg(d), Reg(cached)))
				}
			} else {
				g.load_val(src.val, d)
			}
		}
	}
	st.wrote(dst, d)
	if src.kind == .input && dst.kind == .phys {
		st.val_regs[src.val] = d
	}
}

// resolve maps a template token to its operand.
fn (st &ArmAsmState) resolve(token string, spec &ssa.InlineAsmSpec, input_vals []int) ArmAsmOperand {
	name := token.trim_space()
	if reg := arm64_asm_register_number(name) {
		return ArmAsmOperand{
			kind: .phys
			reg:  reg
		}
	}
	if name.len > 0 && (name[0].is_digit() || name[0] == `-` || name[0] == `#`) {
		return ArmAsmOperand{
			kind: .imm
			imm:  arm64_asm_imm(name)
		}
	}
	input_start := spec.asm_input_start()
	for io_index, io in spec.ios {
		if io.alias.len == 0 || io.alias != name {
			continue
		}
		if io.constraint.starts_with('=') {
			return ArmAsmOperand{
				kind: .output
				io:   io_index
			}
		}
		return ArmAsmOperand{
			kind: .input
			val:  input_vals[io_index - input_start]
		}
	}
	panic('arm64: inline asm operand `${name}` is not declared')
}

// read_register returns a register holding the operand's value.
fn (mut st ArmAsmState) read_register(mut g Gen, op ArmAsmOperand) int {
	match op.kind {
		.phys {
			return op.reg
		}
		.output {
			return st.homes[op.io] or { panic('arm64: inline asm output read before it is written') }
		}
		.input {
			if cached := st.val_regs[op.val] {
				return cached
			}
			r := st.pick()
			g.load_val(op.val, r)
			st.val_regs[op.val] = r
			return r
		}
		.imm {
			panic('arm64: inline asm immediate operand is not supported in this position')
		}
	}
}

// write_register returns the register an operation writes its result into.
fn (mut st ArmAsmState) write_register(mut g Gen, op ArmAsmOperand) int {
	match op.kind {
		.phys {
			return op.reg
		}
		.output {
			if home := st.homes[op.io] {
				return home
			}
			home := st.pick()
			st.homes[op.io] = home
			return home
		}
		else {
			panic('arm64: inline asm destination must be a register')
		}
	}
}

// wrote records that register d was overwritten by the destination operand.
fn (mut st ArmAsmState) wrote(dst ArmAsmOperand, d int) {
	if dst.kind == .phys {
		st.drop_register(d)
	}
}

// drop_register forgets every operand copy held in register reg.
fn (mut st ArmAsmState) drop_register(reg int) {
	for val, held in st.val_regs {
		if held == reg {
			st.val_regs.delete(val)
		}
	}
}

// pick hands out a register the block has not used yet.
fn (mut st ArmAsmState) pick() int {
	for reg in asm_arm64_pool {
		if !st.taken[reg] {
			st.taken[reg] = true
			return reg
		}
	}
	panic('arm64: inline asm ran out of registers')
}

// finish_arm64_asm_outputs stores the outputs: the first output is the
// instruction's result, the others go through their pointer operands.
fn (mut g Gen) finish_arm64_asm_outputs(mut st ArmAsmState, instr &ssa.Instruction, val_id int, spec &ssa.InlineAsmSpec) {
	input_count := spec.asm_input_count()
	mut ordinal := 0
	for io_index, io in spec.ios {
		if !io.constraint.starts_with('=') {
			continue
		}
		home := st.homes[io_index] or {
			r := st.pick()
			g.emit_mov_imm(r, 0)
			r
		}
		if ordinal == 0 {
			if instr.typ > 0 {
				g.store_val(home, val_id)
			}
		} else {
			ptr_id := int(instr.operands[input_count + ordinal - 1])
			elem := g.m.type_store.types[g.m.values[ptr_id].typ].elem_type
			scratch := st.pick()
			g.load_val(ptr_id, scratch)
			g.emit_store_typed(home, scratch, elem)
		}
		ordinal++
	}
}

// arm64_asm_split_instruction splits a template line into mnemonic and operands.
fn arm64_asm_split_instruction(line string) (string, string) {
	trimmed := line.trim_space()
	for i, c in trimmed {
		if c.is_space() {
			return trimmed[..i], trimmed[i + 1..].trim_space()
		}
	}
	return trimmed, ''
}

// arm64_asm_split_operands splits a comma-separated template operand list.
fn arm64_asm_split_operands(s string) []string {
	mut res := []string{}
	if s.len == 0 {
		return res
	}
	for part in s.split(',') {
		res << part.trim_space()
	}
	return res
}

// arm64_asm_register_number maps `x0`..`x30` and `xzr` to their numbers.
fn arm64_asm_register_number(name string) ?int {
	if name == 'xzr' {
		return 31
	}
	if name.len >= 2 && name[0] == `x` && name[1..].bytes().all(it.is_digit()) {
		n := name[1..].int()
		if n <= 30 {
			return n
		}
	}
	return none
}

// arm64_asm_imm parses an integer immediate, with or without a leading `#`.
fn arm64_asm_imm(token string) i64 {
	return token.trim_left('#').i64()
}

// arm64_asm_alu_word encodes a three-register operation.
fn arm64_asm_alu_word(mnemonic string, rd int, rn int, rm int) u32 {
	if mnemonic == 'add' {
		return asm_add_reg(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'adds' {
		return asm_adds_reg(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'adc' {
		return asm_adc(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'sub' {
		return asm_sub_reg(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'subs' {
		return asm_subs_reg(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'sbc' {
		return asm_sbc(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'and' {
		return asm_and(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'orr' {
		return asm_orr(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'eor' {
		return asm_eor(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'mul' {
		return asm_mul(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'umulh' {
		return asm_umulh(Reg(rd), Reg(rn), Reg(rm))
	}
	if mnemonic == 'udiv' {
		return asm_udiv(Reg(rd), Reg(rn), Reg(rm))
	}
	panic('arm64: inline asm instruction `${mnemonic}` is not supported by the arm64 backend')
}
