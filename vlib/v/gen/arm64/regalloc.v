module arm64

import v.ssa

// Register allocation for the arm64 backend.
//
// Every SSA value used to live in its own stack slot, so each use was a load and
// each result a store. This pass gives scalar integer and pointer results a home
// in the callee-saved registers x19-x28. Those survive calls, so no spill code
// is needed around them, and the generator keeps using x8-x17 as scratch.
//
// It is a linear scan over live intervals: the shape of Go's allocator
// (cmd/compile/internal/ssa/regalloc.go), which also evicts the value used
// furthest away when it runs out of registers, without Go's per-block register
// state and edge shuffles. Values that get no register keep their stack slot, as
// before. Like Zig's RegisterManager (src/register_manager.zig) it records the
// registers a function allocated, and only those are saved in the prologue.
//
// A value's slot is replaced by its register, so code that reads a slot
// directly (aggregates, floats) only ever sees values this pass leaves alone.

const first_allocatable_reg = 19
const last_allocatable_reg = 28

// A function with more cross-block state than this keeps those values in slots.
const regalloc_max_bitset_bytes = 64 * 1024 * 1024

struct LiveInterval {
	value int
mut:
	start   int
	end     int
	crosses bool
}

// value_home_reg returns the register a value lives in, or 0.
@[inline]
fn (g &Gen) value_home_reg(val_id int) int {
	if val_id <= 0 || val_id >= g.value_homes.len {
		return 0
	}
	return int(g.value_homes[val_id])
}

fn (mut g Gen) clear_value_homes() {
	for val_id in g.value_home_ids {
		g.value_homes[val_id] = 0
	}
	g.value_home_ids.clear()
}

fn (g &Gen) regalloc_eligible(val_id int, instr ssa.Instruction) bool {
	if instr.op in [.alloca, .phi, .inline_asm] || instr.typ == 0 {
		return false
	}
	return g.regalloc_scalar(val_id)
}

fn (g &Gen) regalloc_scalar(val_id int) bool {
	val := g.m.values[val_id]
	if val.typ <= 0 || val.typ >= g.m.type_store.types.len || g.is_float_type(val.typ)
		|| g.is_aggregate_type(val.typ) || g.is_value_aggregate_type(val.typ) {
		return false
	}
	return g.m.type_size(val.typ) <= 8
}

// regalloc_successors returns the blocks a terminator can reach; none says the
// function has a terminator this pass does not model.
fn (g &Gen) regalloc_successors(blk_id int) ?[]int {
	instrs := g.m.blocks[blk_id].instrs
	if instrs.len == 0 {
		return []int{}
	}
	last := g.m.values[instrs.last()]
	if last.kind != .instruction {
		return []int{}
	}
	instr := g.m.instrs[last.index]
	return match instr.op {
		.br { [int(instr.operands[1]), int(instr.operands[2])] }
		.jmp { [int(instr.operands[0])] }
		.ret, .unreachable { []int{} }
		else { if instr.op == .switch_ { none } else { []int{} } }
	}
}

// regalloc_function assigns registers for one function and returns how many
// callee-saved pairs its prologue must save. The per-value and per-block tables
// are arrays reused across functions: this runs once per function of the
// program, and map lookups here were the generator's hot spot.
fn (mut g Gen) regalloc_function(func &ssa.Function) int {
	g.clear_value_homes()
	g.ensure_regalloc_tables()
	mut list := []LiveInterval{}
	mut crossing := []int{}
	mut phi_defs := map[int][]int{}
	mut has_unmodeled_flow := false
	defer {
		g.reset_regalloc_tables(func, list)
	}
	mut pos := 0
	// Definitions first: a use can precede its definition in layout order.
	for blk_id in func.blocks {
		g.ra_block_first[blk_id] = pos
		for val_id in g.m.blocks[blk_id].instrs {
			val := g.m.values[val_id]
			if val.kind != .instruction {
				pos++
				continue
			}
			instr := g.m.instrs[val.index]
			if instr.op == .inline_asm {
				return 0
			}
			if instr.op == .assign && instr.operands.len >= 2 {
				// A phi lowered to copies: the destination has one definition per
				// predecessor, so it is live across the blocks between them.
				dest := int(instr.operands[0])
				if g.m.values[dest].kind == .phi_result && g.regalloc_scalar(dest) {
					index := g.ra_interval[dest]
					if index >= 0 {
						list[index].start = arm64_min_int(list[index].start, pos)
						list[index].end = arm64_max_int(list[index].end, pos)
					} else {
						g.ra_interval[dest] = list.len
						list << LiveInterval{
							value: dest
							start: pos
							end:   pos
						}
						crossing << dest
					}
					mut blocks := phi_defs[dest] or { []int{} }
					blocks << blk_id
					phi_defs[dest] = blocks
				}
			}
			if g.regalloc_eligible(val_id, instr) {
				g.ra_value_block[val_id] = blk_id
				g.ra_interval[val_id] = list.len
				list << LiveInterval{
					value: val_id
					start: pos
					end:   pos
				}
			}
			pos++
		}
		g.ra_block_last[blk_id] = pos - 1
		if _ := g.regalloc_successors(blk_id) {
		} else {
			has_unmodeled_flow = true
		}
	}
	if list.len == 0 {
		return 0
	}
	pos = 0
	for blk_id in func.blocks {
		for val_id in g.m.blocks[blk_id].instrs {
			val := g.m.values[val_id]
			if val.kind == .instruction {
				instr := g.m.instrs[val.index]
				for i, operand_id in instr.operands {
					operand := int(operand_id)
					if !regalloc_operand_is_value(instr.op, i) || (instr.op == .assign && i == 0)
						|| operand <= 0 || operand >= g.ra_interval.len {
						continue
					}
					index := g.ra_interval[operand]
					if index < 0 {
						continue
					}
					if instr.op == .phi {
						// Used on the incoming edge, at the end of the predecessor.
						pred := int(instr.operands[i + 1])
						if pred >= 0 && pred < g.ra_block_last.len && g.ra_block_first[pred] >= 0 {
							list[index].end = arm64_max_int(list[index].end, g.ra_block_last[pred])
							if !list[index].crosses {
								list[index].crosses = true
								crossing << operand
							}
							g.ra_block_uses[pred] << operand
						}
						continue
					}
					list[index].end = arm64_max_int(list[index].end, pos)
					if operand in phi_defs || g.ra_value_block[operand] != blk_id {
						if !list[index].crosses {
							list[index].crosses = true
							crossing << operand
						}
						g.ra_block_uses[blk_id] << operand
					}
				}
			}
			pos++
		}
	}
	for val_id, _ in phi_defs {
		list[g.ra_interval[val_id]].crosses = true
	}
	mut skip_crossing := has_unmodeled_flow
	if crossing.len * func.blocks.len / 8 > regalloc_max_bitset_bytes {
		skip_crossing = true
	}
	if !skip_crossing && crossing.len > 0 {
		g.extend_cross_block_intervals(func, crossing, phi_defs, mut list)
	}
	mut live := list.filter(it.end > it.start && !(skip_crossing && it.crosses))
	live.sort(a.start < b.start)
	return g.linear_scan(mut live)
}

fn (mut g Gen) ensure_regalloc_tables() {
	if g.value_homes.len < g.m.values.len {
		g.value_homes = []u8{len: g.m.values.len}
	}
	if g.ra_interval.len < g.m.values.len {
		g.ra_interval = []int{len: g.m.values.len, init: -1}
		g.ra_value_block = []int{len: g.m.values.len}
		g.ra_global_index = []int{len: g.m.values.len, init: -1}
	}
	if g.ra_block_first.len < g.m.blocks.len {
		g.ra_block_first = []int{len: g.m.blocks.len, init: -1}
		g.ra_block_last = []int{len: g.m.blocks.len}
		g.ra_block_index = []int{len: g.m.blocks.len, init: -1}
		g.ra_block_uses = [][]int{len: g.m.blocks.len}
	}
}

fn (mut g Gen) reset_regalloc_tables(func &ssa.Function, list []LiveInterval) {
	for iv in list {
		g.ra_interval[iv.value] = -1
		g.ra_global_index[iv.value] = -1
	}
	for blk_id in func.blocks {
		g.ra_block_first[blk_id] = -1
		g.ra_block_index[blk_id] = -1
		g.ra_block_uses[blk_id].clear()
	}
}

// regalloc_operand_is_value says whether operand `index` of an instruction is a
// value, as opposed to a block.
fn regalloc_operand_is_value(op ssa.OpCode, index int) bool {
	return match op {
		.br { index == 0 }
		.jmp { false }
		.phi { index % 2 == 0 }
		else { true }
	}
}

// extend_cross_block_intervals runs backward liveness over the values used
// outside their defining block, and widens their intervals to cover each block
// they are live through, loops included.
fn (mut g Gen) extend_cross_block_intervals(func &ssa.Function, crossing []int, phi_defs map[int][]int, mut list []LiveInterval) {
	for index, val_id in crossing {
		g.ra_global_index[val_id] = index
	}
	words := (crossing.len + 63) / 64
	for i, blk_id in func.blocks {
		g.ra_block_index[blk_id] = i
	}
	nblocks := func.blocks.len
	mut live_in := []u64{len: nblocks * words}
	mut live_out := []u64{len: nblocks * words}
	mut defs := []u64{len: nblocks * words}
	mut uses := []u64{len: nblocks * words}
	for index, val_id in crossing {
		mask := u64(1) << (index % 64)
		if def_blocks := phi_defs[val_id] {
			for blk_id in def_blocks {
				defs[g.ra_block_index[blk_id] * words + index / 64] |= mask
			}
		} else {
			defs[g.ra_block_index[g.ra_value_block[val_id]] * words + index / 64] |= mask
		}
	}
	for bi, blk_id in func.blocks {
		for val_id in g.ra_block_uses[blk_id] {
			index := g.ra_global_index[val_id]
			if index >= 0 {
				uses[bi * words + index / 64] |= u64(1) << (index % 64)
			}
		}
	}
	mut succs := [][]int{len: nblocks}
	for bi, blk_id in func.blocks {
		for succ in g.regalloc_successors(blk_id) or { []int{} } {
			if succ >= 0 && succ < g.ra_block_index.len && g.ra_block_index[succ] >= 0 {
				succs[bi] << g.ra_block_index[succ]
			}
		}
	}
	mut changed := true
	for changed {
		changed = false
		for bi := nblocks - 1; bi >= 0; bi-- {
			base := bi * words
			for w in 0 .. words {
				mut out := live_out[base + w]
				for si in succs[bi] {
					out |= live_in[si * words + w]
				}
				live_out[base + w] = out
				updated := uses[base + w] | (out & ~defs[base + w])
				if updated != live_in[base + w] {
					live_in[base + w] = updated
					changed = true
				}
			}
		}
	}
	for index, val_id in crossing {
		interval_index := g.ra_interval[val_id]
		word := index / 64
		mask := u64(1) << (index % 64)
		for bi, blk_id in func.blocks {
			if live_in[bi * words + word] & mask != 0 {
				list[interval_index].start = arm64_min_int(list[interval_index].start, g.ra_block_first[blk_id])
				list[interval_index].end = arm64_max_int(list[interval_index].end, g.ra_block_last[blk_id])
			}
			if live_out[bi * words + word] & mask != 0 {
				list[interval_index].end = arm64_max_int(list[interval_index].end, g.ra_block_last[blk_id])
			}
		}
	}
}

// linear_scan hands out x19-x28 by interval start, evicting the interval that
// ends furthest away, and returns the number of register pairs in use.
fn (mut g Gen) linear_scan(mut intervals []LiveInterval) int {
	mut free := []int{cap: 10}
	for reg := last_allocatable_reg; reg >= first_allocatable_reg; reg-- {
		free << reg
	}
	mut active := []LiveInterval{cap: 10}
	mut assigned := map[int]int{}
	mut highest := 0
	for iv in intervals {
		mut i := 0
		for i < active.len {
			if active[i].end <= iv.start {
				free << assigned[active[i].value]
				active.delete(i)
			} else {
				i++
			}
		}
		if free.len > 0 {
			reg := free.pop()
			assigned[iv.value] = reg
			highest = arm64_max_int(highest, reg)
			active << iv
			continue
		}
		mut furthest := 0
		for j, candidate in active {
			if candidate.end > active[furthest].end {
				furthest = j
			}
		}
		if active[furthest].end > iv.end {
			victim := active[furthest]
			assigned[iv.value] = assigned[victim.value]
			assigned.delete(victim.value)
			active[furthest] = iv
		}
	}
	for val_id, reg in assigned {
		g.value_homes[val_id] = u8(reg)
		g.value_home_ids << val_id
	}
	if highest == 0 {
		return 0
	}
	return (highest - first_allocatable_reg) / 2 + 1
}

// retarget_last_instruction makes the instruction that just computed scratch
// register `from` write `to` instead, so a result lands in its home register
// without a move. Only instructions of the current SSA instruction are touched:
// shapes whose bits 0-4 are a destination register.
fn (mut g Gen) retarget_last_instruction(from int, to int) bool {
	last := g.text_words.len - 1
	if last < g.current_instr_start || last < 0 {
		return false
	}
	word := g.text_words[last]
	if word & 0x1f != u32(from) || !instruction_writes_rd(word) {
		return false
	}
	g.text_words[last] = (word & 0xffffffe0) | u32(to)
	return true
}

// instruction_writes_rd covers the data-processing and load encodings the
// generator emits for results.
fn instruction_writes_rd(word u32) bool {
	top := word >> 24
	return match top {
		// add, sub, and, orr, eor, mul, variable shifts, div, csel/cset,
		// sign and zero extends
		0x8b, 0xcb, 0x0b, 0x4b, 0x8a, 0xaa, 0xca, 0x9b, 0x9a, 0x93, 0x53, 0xd3, 0x91, 0xd1 { true }
		// ldr x, w, h, b (unsigned offset)
		0xf9, 0xb9, 0x79, 0x39 { word & (1 << 22) != 0 }
		else { false }
	}
}

// emit_save_callee_pairs stores the callee-saved pairs the function uses, in the
// order frame-mode compact unwinding expects: x19, x20 just below fp, then the
// following pairs.
fn (mut g Gen) emit_save_callee_pairs(pairs int) {
	for i in 0 .. pairs {
		g.emit32(asm_stp_offset(Reg(20 + 2 * i), Reg(19 + 2 * i), fp, i32(-2 * (i + 1))))
	}
}

fn (mut g Gen) emit_restore_callee_pairs(pairs int) {
	for i in 0 .. pairs {
		g.emit32(asm_ldp_offset(Reg(20 + 2 * i), Reg(19 + 2 * i), fp, i32(-2 * (i + 1))))
	}
}

// emit_function_return is the epilogue of every return path.
fn (mut g Gen) emit_function_return() {
	g.emit_restore_callee_pairs(g.saved_callee_pairs)
	if g.stack_size > 0 {
		g.emit_add_sp(g.stack_size)
	}
	g.emit32(asm_ldp_fp_lr_post())
	g.emit32(asm_ret())
}
