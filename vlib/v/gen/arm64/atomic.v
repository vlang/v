module arm64

import v.ssa

// Sequentially consistent atomics, in the form Go uses on arm64 without LSE
// (cmd/compile/internal/arm64/ssa.go, the OpARM64LoweredAtomic cases, and LDAR and
// STLR for loads and stores), which also runs on ARMv8.0 Linux machines.
//
// Operands sit in x8-x10 or in their home registers. The exclusive loops write
// only the registers below, and nothing between LDAXR and STLXR can touch memory.
const atomic_old_reg = Reg(14)
const atomic_new_reg = Reg(15)
const atomic_status_reg = Reg(16)

// atomic_size_field encodes a byte width for the size field of the exclusive
// load and store instructions.
fn atomic_size_field(bytes int) u32 {
	return match bytes {
		1 { u32(0) }
		2 { u32(1) }
		4 { u32(2) }
		else { u32(3) }
	}
}

fn (mut g Gen) gen_atomic(val_id int, instr ssa.Instruction) {
	match instr.op {
		.fence {
			g.emit32(asm_dmb_ish())
		}
		.atomic_load {
			typ := g.m.values[val_id].typ
			ptr_reg := g.load_val(instr.operands[0], 9)
			g.emit32(asm_ldar(atomic_old_reg, Reg(ptr_reg), atomic_size_field(g.m.type_size(typ))))
			g.finish_atomic_result(val_id, typ)
		}
		.atomic_store {
			value_id := instr.operands[0]
			ptr_id := instr.operands[1]
			value_reg := g.load_val(value_id, 8)
			ptr_reg := g.load_val(ptr_id, 9)
			elem := g.ptr_elem_type(ptr_id)
			typ := if int(elem) > 0 { elem } else { g.m.values[value_id].typ }
			g.emit32(asm_stlr(Reg(value_reg), Reg(ptr_reg), atomic_size_field(g.m.type_size(typ))))
		}
		.atomic_xchg, .atomic_add, .atomic_sub {
			g.gen_atomic_read_modify_write(val_id, instr)
		}
		.cmpxchg {
			g.gen_atomic_compare_exchange(val_id, instr)
		}
		else {}
	}
}

fn (mut g Gen) gen_atomic_read_modify_write(val_id int, instr ssa.Instruction) {
	typ := g.m.values[val_id].typ
	size := atomic_size_field(g.m.type_size(typ))
	ptr_reg := Reg(g.load_val(instr.operands[0], 9))
	operand_reg := Reg(g.load_val(instr.operands[1], 8))
	g.emit32(asm_ldaxr(atomic_old_reg, ptr_reg, size))
	if instr.op == .atomic_xchg {
		g.emit32(asm_stlxr(atomic_status_reg, operand_reg, ptr_reg, size))
		g.emit32(asm_cbnz(atomic_status_reg, -2))
	} else {
		g.emit32(if instr.op == .atomic_add {
			asm_add_reg(atomic_new_reg, atomic_old_reg, operand_reg)
		} else {
			asm_sub_reg(atomic_new_reg, atomic_old_reg, operand_reg)
		})
		g.emit32(asm_stlxr(atomic_status_reg, atomic_new_reg, ptr_reg, size))
		g.emit32(asm_cbnz(atomic_status_reg, -3))
	}
	g.finish_atomic_result(val_id, typ)
}

// gen_atomic_compare_exchange leaves the loop as soon as the comparison fails. It
// retries only when the exclusive store loses its reservation.
fn (mut g Gen) gen_atomic_compare_exchange(val_id int, instr ssa.Instruction) {
	typ := g.m.values[val_id].typ
	bytes := g.m.type_size(typ)
	size := atomic_size_field(bytes)
	ptr_reg := Reg(g.load_val(instr.operands[0], 9))
	mut compare_reg := Reg(g.load_val(instr.operands[1], 8))
	desired_reg := Reg(g.load_val(instr.operands[2], 10))
	// LDAXRB and LDAXRH zero-extend, so a signed narrow operand must match that.
	if bytes == 1 {
		g.emit32(asm_and_imm_0xff(atomic_new_reg, compare_reg))
		compare_reg = atomic_new_reg
	} else if bytes == 2 {
		g.emit32(asm_and_imm_0xffff(atomic_new_reg, compare_reg))
		compare_reg = atomic_new_reg
	}
	g.emit32(asm_ldaxr(atomic_old_reg, ptr_reg, size))
	g.emit32(if bytes == 8 {
		asm_cmp_reg(atomic_old_reg, compare_reg)
	} else {
		asm_cmp_reg_w(atomic_old_reg, compare_reg)
	})
	g.emit32(asm_b_cond(cond_ne, 3))
	g.emit32(asm_stlxr(atomic_status_reg, desired_reg, ptr_reg, size))
	g.emit32(asm_cbnz(atomic_status_reg, -4))
	g.finish_atomic_result(val_id, typ)
}

// finish_atomic_result gives a signed result the sign extension an ordinary load
// of the type has, then stores it in the value's home.
fn (mut g Gen) finish_atomic_result(val_id int, typ ssa.TypeID) {
	if g.is_signed_int_type(typ) {
		match g.m.type_size(typ) {
			1 { g.emit32(asm_sxtb(atomic_old_reg, atomic_old_reg)) }
			2 { g.emit32(asm_sxth(atomic_old_reg, atomic_old_reg)) }
			4 { g.emit32(asm_sxtw(atomic_old_reg, atomic_old_reg)) }
			else {}
		}
	}
	g.store_val(int(atomic_old_reg), val_id)
}
