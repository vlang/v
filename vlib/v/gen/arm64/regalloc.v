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
	start int
	end   int
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
// callee-saved pairs its prologue must save.
fn (mut g Gen) regalloc_function(func &ssa.Function) int {
	g.clear_value_homes()
	if g.value_homes.len < g.m.values.len {
		g.value_homes = []u8{len: g.m.values.len}
	}
	mut block_first := map[int]int{}
	mut block_last := map[int]int{}
	mut value_block := map[int]int{}
	mut intervals := map[int]int{} // value -> index in `list`
	mut list := []LiveInterval{}
	mut crosses := map[int]bool{}
	mut phi_def_blocks := map[int][]int{}
	mut has_unmodeled_flow := false
	mut pos := 0
	// Definitions first: a use can precede its definition in layout order.
	for blk_id in func.blocks {
		block_first[blk_id] = pos
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
					if index := intervals[dest] {
						list[index].start = arm64_min_int(list[index].start, pos)
						list[index].end = arm64_max_int(list[index].end, pos)
					} else {
						intervals[dest] = list.len
						list << LiveInterval{
							value: dest
							start: pos
							end:   pos
						}
					}
					mut blocks := phi_def_blocks[dest] or { []int{} }
					blocks << blk_id
					phi_def_blocks[dest] = blocks
					crosses[dest] = true
				}
			}
			if g.regalloc_eligible(val_id, instr) {
				value_block[val_id] = blk_id
				intervals[val_id] = list.len
				list << LiveInterval{
					value: val_id
					start: pos
					end:   pos
				}
			}
			pos++
		}
		block_last[blk_id] = pos - 1
		if _ := g.regalloc_successors(blk_id) {
		} else {
			has_unmodeled_flow = true
		}
	}
	if list.len == 0 {
		return 0
	}
	mut block_uses := map[int][]int{}
	pos = 0
	for blk_id in func.blocks {
		for val_id in g.m.blocks[blk_id].instrs {
			val := g.m.values[val_id]
			if val.kind == .instruction {
				instr := g.m.instrs[val.index]
				for i, operand_id in instr.operands {
					operand := int(operand_id)
					if !regalloc_operand_is_value(instr.op, i) || (instr.op == .assign && i == 0) {
						continue
					}
					if instr.op == .phi {
						// Used on the incoming edge, at the end of the predecessor.
						pred := int(instr.operands[i + 1])
						if index := intervals[operand] {
							if pred in block_last {
								list[index].end = arm64_max_int(list[index].end, block_last[pred])
							}
							crosses[operand] = true
							mut pred_uses := block_uses[pred] or { []int{} }
							pred_uses << operand
							block_uses[pred] = pred_uses
						}
						continue
					}
					if index := intervals[operand] {
						list[index].end = arm64_max_int(list[index].end, pos)
						if operand in phi_def_blocks || value_block[operand] != blk_id {
							crosses[operand] = true
							mut uses := block_uses[blk_id] or { []int{} }
							uses << operand
							block_uses[blk_id] = uses
						}
					}
				}
			}
			pos++
		}
	}
	mut unused := map[int]bool{}
	for iv in list {
		if iv.end == iv.start {
			unused[iv.value] = true
		}
	}
	if has_unmodeled_flow || crosses.len * func.blocks.len / 8 > regalloc_max_bitset_bytes {
		for val_id, _ in crosses {
			unused[val_id] = true
		}
	} else if crosses.len > 0 {
		g.extend_cross_block_intervals(func, crosses, value_block, phi_def_blocks, block_uses,
			block_first, block_last, intervals, mut list)
	}
	mut live := list.filter(it.value !in unused)
	live.sort(a.start < b.start)
	return g.linear_scan(mut live)
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
fn (g &Gen) extend_cross_block_intervals(func &ssa.Function, crosses map[int]bool, value_block map[int]int, phi_def_blocks map[int][]int, block_uses map[int][]int, block_first map[int]int, block_last map[int]int, intervals map[int]int, mut list []LiveInterval) {
	mut global_index := map[int]int{}
	mut globals := []int{}
	for val_id, _ in crosses {
		global_index[val_id] = globals.len
		globals << val_id
	}
	words := (globals.len + 63) / 64
	mut block_index := map[int]int{}
	for i, blk_id in func.blocks {
		block_index[blk_id] = i
	}
	mut live_in := [][]u64{len: func.blocks.len, init: []u64{len: words}}
	mut defs := [][]u64{len: func.blocks.len, init: []u64{len: words}}
	mut uses := [][]u64{len: func.blocks.len, init: []u64{len: words}}
	for val_id, index in global_index {
		if def_blocks := phi_def_blocks[val_id] {
			for blk_id in def_blocks {
				defs[block_index[blk_id]][index / 64] |= u64(1) << (index % 64)
			}
			continue
		}
		b := block_index[value_block[val_id]]
		defs[b][index / 64] |= u64(1) << (index % 64)
	}
	for blk_id, list_of_uses in block_uses {
		b := block_index[blk_id]
		for val_id in list_of_uses {
			index := global_index[val_id]
			uses[b][index / 64] |= u64(1) << (index % 64)
		}
	}
	mut live_out := [][]u64{len: func.blocks.len, init: []u64{len: words}}
	mut changed := true
	for changed {
		changed = false
		for bi := func.blocks.len - 1; bi >= 0; bi-- {
			succs := g.regalloc_successors(func.blocks[bi]) or { []int{} }
			for w in 0 .. words {
				mut out := live_out[bi][w]
				for succ in succs {
					if si := block_index[succ] {
						out |= live_in[si][w]
					}
				}
				live_out[bi][w] = out
				updated := uses[bi][w] | (out & ~defs[bi][w])
				if updated != live_in[bi][w] {
					live_in[bi][w] = updated
					changed = true
				}
			}
		}
	}
	for index, val_id in globals {
		list_index := intervals[val_id]
		word := index / 64
		mask := u64(1) << (index % 64)
		for bi, blk_id in func.blocks {
			if live_in[bi][word] & mask != 0 {
				list[list_index].start = arm64_min_int(list[list_index].start, block_first[blk_id])
				list[list_index].end = arm64_max_int(list[list_index].end, block_last[blk_id])
			}
			if live_out[bi][word] & mask != 0 {
				list[list_index].end = arm64_max_int(list[list_index].end, block_last[blk_id])
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
