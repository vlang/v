module arm64

import v.ssa

// Register allocation for the arm64 backend.
//
// Every SSA value used to live in its own stack slot, so each use was a load and
// each result a store. This pass gives scalar integer and pointer results a home
// in the callee-saved registers x19-x28, and floats in d8-d15. Those survive
// calls, so no spill code is needed around them, and the generator keeps using
// x8-x17 and d0-d7 as scratch.
//
// It is a linear scan over live intervals: the shape of Go's allocator
// (cmd/compile/internal/ssa/regalloc.go), which also evicts the value used
// furthest away when it runs out of registers, without Go's per-block register
// state and edge shuffles. Values that get no register keep their stack slot, as
// before. Like Zig's RegisterManager (src/register_manager.zig) it records the
// registers a function allocated, and only those are saved in the prologue.
//
// A value's slot is replaced by its register, so code that reads a slot
// directly (aggregates) only ever sees values this pass leaves alone.

const first_allocatable_reg = 19
const last_allocatable_reg = 28
const first_float_reg = 8
const last_float_reg = 15

// A float value's home is d8-d15, recorded above the integer register numbers.
const float_home_base = 64

@[inline]
fn is_float_home(home int) bool {
	return home >= float_home_base
}

@[inline]
fn float_home_reg(home int) int {
	return home - float_home_base
}

struct LiveInterval {
	value int
mut:
	start   int
	end     int
	crosses bool
}

// RaLink is a node of the per-interval lists of use and definition blocks.
struct RaLink {
	next  int
	block int
}

// RaActive is an interval that currently holds a register.
struct RaActive {
	value int
	end   int
	reg   int
}

// value_home_reg returns the register a value lives in, or 0. The table covers
// the current function's value ids (see reset_value_slots).
@[inline]
fn (g &Gen) value_home_reg(val_id int) int {
	index := val_id - g.slot_value_base
	if val_id <= 0 || index < 0 || index >= g.value_homes.len {
		return 0
	}
	return int(g.value_homes[index])
}

fn (mut g Gen) clear_value_homes() {
	for index in g.value_home_ids {
		g.value_homes[index] = 0
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
	if val.typ <= 0 || val.typ >= g.m.type_store.types.len || g.is_aggregate_type(val.typ)
		|| g.is_value_aggregate_type(val.typ) {
		return false
	}
	return g.m.type_size(val.typ) <= 8
}

// regalloc_successors returns the blocks a terminator can reach, -1 for none.
fn (g &Gen) regalloc_successors(blk_id int) (int, int) {
	instrs := g.m.blocks[blk_id].instrs
	if instrs.len == 0 {
		return -1, -1
	}
	last := g.m.values[instrs.last()]
	if last.kind != .instruction {
		return -1, -1
	}
	instr := g.m.instrs[last.index]
	if instr.op == .br {
		return int(instr.operands[1]), int(instr.operands[2])
	}
	if instr.op == .jmp {
		return int(instr.operands[0]), -1
	}
	return -1, -1
}

// regalloc_flow_is_modeled says whether this pass understands a block's
// terminator.
fn (g &Gen) regalloc_flow_is_modeled(blk_id int) bool {
	instrs := g.m.blocks[blk_id].instrs
	if instrs.len == 0 {
		return true
	}
	last := g.m.values[instrs.last()]
	return last.kind != .instruction || g.m.instrs[last.index].op != .switch_
}

// regalloc_block_index returns a block's position in the current function, or
// -1 for a block outside it. The table covers the function's block ids (see
// reset_block_offsets) and holds position + 1.
@[inline]
fn (g &Gen) regalloc_block_index(blk_id int) int {
	index := blk_id - g.block_offset_base
	if index < 0 || index >= g.block_offset_count {
		return -1
	}
	return g.ra_block_index[index] - 1
}

// regalloc_interval_index returns the interval of a value in the current
// function, or -1. The table covers the function's value ids and holds index + 1.
@[inline]
fn (g &Gen) regalloc_interval_index(val_id int) int {
	index := val_id - g.slot_value_base
	if val_id <= 0 || index < 0 || index >= g.slot_value_count {
		return -1
	}
	return g.ra_interval[index] - 1
}

// regalloc_add_interval starts the interval of a value at `pos`.
fn (mut g Gen) regalloc_add_interval(val_id int, pos int) {
	g.ra_interval[val_id - g.slot_value_base] = g.ra_list.len + 1
	g.ra_list << LiveInterval{
		value: val_id
		start: pos
		end:   pos
	}
	g.ra_use_heads << 0
	g.ra_def_heads << 0
}

// regalloc_mark_crossing records that an interval's value is live outside the
// block that defines it.
@[inline]
fn (mut g Gen) regalloc_mark_crossing(index int) {
	if !g.ra_list[index].crosses {
		g.ra_list[index].crosses = true
		g.ra_crossing << index
	}
}

// regalloc_function assigns registers for one function and returns how many
// callee-saved pairs its prologue must save. The per-value and per-block tables
// are arrays reused across functions: this runs once per function of the
// program, and map lookups here were the generator's hot spot.
fn (mut g Gen) regalloc_function(func &ssa.Function) int {
	g.clear_value_homes()
	g.saved_float_pairs = 0
	g.ensure_regalloc_tables(func)
	g.ra_list.clear()
	g.ra_crossing.clear()
	g.ra_use_heads.clear()
	g.ra_def_heads.clear()
	g.ra_links.clear()
	defer {
		g.reset_regalloc_tables(func)
	}
	mut has_unmodeled_flow := false
	for bi, blk_id in func.blocks {
		g.ra_block_index[int(blk_id) - g.block_offset_base] = bi + 1
	}
	mut pos := 0
	// Definitions first: a use can precede its definition in layout order.
	for bi, blk_id in func.blocks {
		g.ra_block_first[bi] = pos
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
					mut index := g.regalloc_interval_index(dest)
					if index >= 0 {
						g.ra_list[index].start = arm64_min_int(g.ra_list[index].start, pos)
						g.ra_list[index].end = arm64_max_int(g.ra_list[index].end, pos)
					} else {
						index = g.ra_list.len
						g.regalloc_add_interval(dest, pos)
					}
					g.ra_links << RaLink{
						next:  g.ra_def_heads[index]
						block: bi
					}
					g.ra_def_heads[index] = g.ra_links.len
					// A phi is live across blocks whether or not anything reads it
					// outside the blocks that assign it.
					g.regalloc_mark_crossing(index)
				}
			}
			if g.regalloc_eligible(val_id, instr) {
				g.ra_value_block[int(val_id) - g.slot_value_base] = bi
				g.regalloc_add_interval(val_id, pos)
			}
			pos++
		}
		g.ra_block_last[bi] = pos - 1
		if !g.regalloc_flow_is_modeled(blk_id) {
			has_unmodeled_flow = true
		}
	}
	if g.ra_list.len == 0 {
		return 0
	}
	pos = 0
	for bi, blk_id in func.blocks {
		for val_id in g.m.blocks[blk_id].instrs {
			val := g.m.values[val_id]
			if val.kind == .instruction {
				g.regalloc_scan_operands(g.m.instrs[val.index], pos, bi)
			}
			pos++
		}
	}
	if !has_unmodeled_flow && g.ra_crossing.len > 0 {
		g.extend_cross_block_intervals(func)
	}
	g.ra_integers.clear()
	g.ra_floats.clear()
	g.ra_live.clear()
	for iv in g.ra_list {
		if iv.end > iv.start && !(has_unmodeled_flow && iv.crosses) {
			g.ra_live << iv
		}
	}
	g.ra_live.sort(a.start < b.start)
	for iv in g.ra_live {
		if g.is_float_type(g.m.values[iv.value].typ) {
			g.ra_floats << iv
		} else {
			g.ra_integers << iv
		}
	}
	g.saved_float_pairs = g.linear_scan(g.ra_floats, first_float_reg, last_float_reg, float_home_base)
	return g.linear_scan(g.ra_integers, first_allocatable_reg, last_allocatable_reg, 0)
}

// regalloc_scan_operands extends the intervals of an instruction's operands to
// `pos` and records the blocks that use a value outside its own block.
fn (mut g Gen) regalloc_scan_operands(instr ssa.Instruction, pos int, bi int) {
	for i, operand_id in instr.operands {
		operand := int(operand_id)
		if !regalloc_operand_is_value(instr.op, i) || (instr.op == .assign && i == 0) {
			continue
		}
		index := g.regalloc_interval_index(operand)
		if index < 0 {
			continue
		}
		if instr.op == .phi {
			// Used on the incoming edge, at the end of the predecessor.
			pred := g.regalloc_block_index(int(instr.operands[i + 1]))
			if pred >= 0 {
				g.ra_list[index].end = arm64_max_int(g.ra_list[index].end, g.ra_block_last[pred])
				g.regalloc_mark_crossing(index)
				g.regalloc_add_use(index, pred)
			}
			continue
		}
		g.ra_list[index].end = arm64_max_int(g.ra_list[index].end, pos)
		if g.ra_def_heads[index] != 0 || g.ra_value_block[operand - g.slot_value_base] != bi {
			g.regalloc_mark_crossing(index)
			g.regalloc_add_use(index, bi)
		}
	}
}

@[inline]
fn (mut g Gen) regalloc_add_use(index int, bi int) {
	g.ra_links << RaLink{
		next:  g.ra_use_heads[index]
		block: bi
	}
	g.ra_use_heads[index] = g.ra_links.len
}

fn (mut g Gen) ensure_regalloc_tables(func &ssa.Function) {
	if g.value_homes.len < g.slot_value_count {
		new_len := arm64_max_int(g.slot_value_count, arm64_max_int(64, g.value_homes.len * 2))
		free_zeroed_table(g.value_homes.data, g.value_homes.len * 1)
		free_zeroed_table(g.ra_interval.data, g.ra_interval.len * int(sizeof(int)))
		free_zeroed_table(g.ra_value_block.data, g.ra_value_block.len * int(sizeof(int)))
		g.value_homes = zeroed_u8_table(new_len)
		g.ra_interval = zeroed_int_table(new_len)
		g.ra_value_block = zeroed_int_table(new_len)
	}
	if g.ra_block_index.len < g.block_offset_count {
		new_len := arm64_max_int(g.block_offset_count, arm64_max_int(16, g.ra_block_index.len * 2))
		free_zeroed_table(g.ra_block_index.data, g.ra_block_index.len * int(sizeof(int)))
		g.ra_block_index = zeroed_int_table(new_len)
	}
	if g.ra_block_first.len < func.blocks.len {
		new_len := arm64_max_int(func.blocks.len, arm64_max_int(16, g.ra_block_first.len * 2))
		g.ra_block_first = []int{len: new_len}
		g.ra_block_last = []int{len: new_len}
		g.ra_pred_start = []int{len: new_len + 1}
		g.ra_live_stamp = []int{len: new_len}
		g.ra_def_stamp = []int{len: new_len}
	}
}

fn (mut g Gen) reset_regalloc_tables(func &ssa.Function) {
	for iv in g.ra_list {
		g.ra_interval[iv.value - g.slot_value_base] = 0
	}
	for blk_id in func.blocks {
		g.ra_block_index[int(blk_id) - g.block_offset_base] = 0
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

// regalloc_build_predecessors records the predecessors of every block of the
// function, by position, as one flat array indexed by ra_pred_start.
fn (mut g Gen) regalloc_build_predecessors(func &ssa.Function) {
	nblocks := func.blocks.len
	for i in 0 .. nblocks + 1 {
		g.ra_pred_start[i] = 0
	}
	for blk_id in func.blocks {
		first, second := g.regalloc_successors(blk_id)
		g.regalloc_count_edge(first)
		g.regalloc_count_edge(second)
	}
	for i in 0 .. nblocks {
		g.ra_pred_start[i + 1] += g.ra_pred_start[i]
	}
	edges := g.ra_pred_start[nblocks]
	if g.ra_preds.len < edges {
		g.ra_preds = []int{len: arm64_max_int(edges, g.ra_preds.len * 2)}
	}
	for bi := nblocks - 1; bi >= 0; bi-- {
		first, second := g.regalloc_successors(func.blocks[bi])
		g.regalloc_add_edge(first, bi)
		g.regalloc_add_edge(second, bi)
	}
	// The fill walked every start back by its own count; restore the bounds.
	for i in 0 .. nblocks {
		g.ra_pred_start[i] = g.ra_pred_start[i + 1]
	}
	g.ra_pred_start[nblocks] = edges
}

fn (mut g Gen) regalloc_count_edge(succ int) {
	si := if succ >= 0 { g.regalloc_block_index(succ) } else { -1 }
	if si >= 0 {
		g.ra_pred_start[si + 1]++
	}
}

fn (mut g Gen) regalloc_add_edge(succ int, pred int) {
	si := if succ >= 0 { g.regalloc_block_index(succ) } else { -1 }
	if si >= 0 {
		g.ra_pred_start[si + 1]--
		g.ra_preds[g.ra_pred_start[si + 1]] = pred
	}
}

// extend_cross_block_intervals widens the interval of each value used outside
// its defining block over every block it is live through, loops included.
//
// Liveness is computed one value at a time, walking backward from its uses until
// its definitions, like the path exploration of Go's
// StackAllocState.computeLive (cmd/compile/internal/ssa/stackalloc.go). The work
// is proportional to the blocks each value is live in. Dense bitsets over all
// values and blocks, iterated to a fixed point, took most of the generator's
// time once inlining made functions large.
//
// A block is live-in for a value when it uses it, or when a successor is live-in
// and the block does not define the value. A block is live-out when a successor
// is live-in.
fn (mut g Gen) extend_cross_block_intervals(func &ssa.Function) {
	g.regalloc_build_predecessors(func)
	for index in g.ra_crossing {
		mut use_link := g.ra_use_heads[index]
		if use_link == 0 {
			continue
		}
		g.ra_stamp++
		stamp := g.ra_stamp
		mut def_link := g.ra_def_heads[index]
		if def_link == 0 {
			g.ra_def_stamp[g.ra_value_block[g.ra_list[index].value - g.slot_value_base]] = stamp
		}
		for def_link != 0 {
			link := g.ra_links[def_link - 1]
			g.ra_def_stamp[link.block] = stamp
			def_link = link.next
		}
		g.ra_worklist.clear()
		for use_link != 0 {
			link := g.ra_links[use_link - 1]
			if g.ra_live_stamp[link.block] != stamp {
				g.ra_live_stamp[link.block] = stamp
				g.ra_worklist << link.block
			}
			use_link = link.next
		}
		mut start := g.ra_list[index].start
		mut end := g.ra_list[index].end
		for g.ra_worklist.len > 0 {
			bi := g.ra_worklist.pop()
			start = arm64_min_int(start, g.ra_block_first[bi])
			end = arm64_max_int(end, g.ra_block_last[bi])
			for pred_at in g.ra_pred_start[bi] .. g.ra_pred_start[bi + 1] {
				pred := g.ra_preds[pred_at]
				end = arm64_max_int(end, g.ra_block_last[pred])
				if g.ra_live_stamp[pred] != stamp && g.ra_def_stamp[pred] != stamp {
					g.ra_live_stamp[pred] = stamp
					g.ra_worklist << pred
				}
			}
		}
		g.ra_list[index].start = start
		g.ra_list[index].end = end
	}
}

// regalloc_assign_home makes `reg` the home of a value.
@[inline]
fn (mut g Gen) regalloc_assign_home(val_id int, reg int, home_base int) {
	index := val_id - g.slot_value_base
	g.value_homes[index] = u8(reg + home_base)
	g.value_home_ids << index
}

// linear_scan hands out x19-x28 by interval start, evicting the interval that
// ends furthest away, and returns the number of register pairs in use.
fn (mut g Gen) linear_scan(intervals []LiveInterval, first_reg int, last_reg int, home_base int) int {
	mut free_regs := [32]int{}
	mut free_len := 0
	for reg := last_reg; reg >= first_reg; reg-- {
		free_regs[free_len] = reg
		free_len++
	}
	mut active := [32]RaActive{}
	mut active_len := 0
	mut highest := 0
	for iv in intervals {
		mut i := 0
		for i < active_len {
			if active[i].end <= iv.start {
				free_regs[free_len] = active[i].reg
				free_len++
				for j in i .. active_len - 1 {
					active[j] = active[j + 1]
				}
				active_len--
			} else {
				i++
			}
		}
		if free_len > 0 {
			free_len--
			reg := free_regs[free_len]
			g.regalloc_assign_home(iv.value, reg, home_base)
			highest = arm64_max_int(highest, reg)
			active[active_len] = RaActive{
				value: iv.value
				end:   iv.end
				reg:   reg
			}
			active_len++
			continue
		}
		mut furthest := 0
		for j in 0 .. active_len {
			if active[j].end > active[furthest].end {
				furthest = j
			}
		}
		if active[furthest].end > iv.end {
			victim := active[furthest]
			g.value_homes[victim.value - g.slot_value_base] = 0
			g.regalloc_assign_home(iv.value, victim.reg, home_base)
			active[furthest] = RaActive{
				value: iv.value
				end:   iv.end
				reg:   victim.reg
			}
		}
	}
	if highest == 0 {
		return 0
	}
	return (highest - first_reg) / 2 + 1
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

// gen_float_binop_in_registers computes an f64 operation directly on the home
// registers of its operands and result. A float without a home, or an f32
// operand widened by the operation, takes the general path.
fn (mut g Gen) gen_float_binop_in_registers(fop ssa.OpCode, lhs_id int, rhs_id int, val_id int) bool {
	result_home := g.value_home_reg(val_id)
	if !is_float_home(result_home) || g.is_f32_type(g.m.values[lhs_id].typ)
		|| g.is_f32_type(g.m.values[rhs_id].typ) {
		return false
	}
	lhs_home := g.value_home_reg(lhs_id)
	rhs_home := g.value_home_reg(rhs_id)
	lhs_scratch, rhs_scratch := float_operand_regs(lhs_id, rhs_id)
	lhs_reg := if is_float_home(lhs_home) { float_home_reg(lhs_home) } else { lhs_scratch }
	rhs_reg := if is_float_home(rhs_home) { float_home_reg(rhs_home) } else { rhs_scratch }
	if !is_float_home(lhs_home) {
		g.load_float_operand(lhs_id, lhs_scratch)
	}
	if !is_float_home(rhs_home) && rhs_scratch != lhs_scratch {
		g.load_float_operand(rhs_id, rhs_scratch)
	}
	dest := float_home_reg(result_home)
	g.emit32(match fop {
		.fadd { asm_fadd_d(dest, lhs_reg, rhs_reg) }
		.fsub { asm_fsub_d(dest, lhs_reg, rhs_reg) }
		.fmul { asm_fmul_d(dest, lhs_reg, rhs_reg) }
		else { asm_fdiv_d(dest, lhs_reg, rhs_reg) }
	})
	return true
}

// emit_save_callee_pairs stores the callee-saved pairs the function uses, in the
// order frame-mode compact unwinding expects: x19, x20 just below fp, then the
// following pairs.
fn (mut g Gen) emit_save_callee_pairs(pairs int) {
	for i in 0 .. pairs {
		g.emit32(asm_stp_offset(Reg(20 + 2 * i), Reg(19 + 2 * i), fp, i32(-2 * (i + 1))))
	}
	for i in 0 .. g.saved_float_pairs {
		g.emit32(asm_stp_d_offset(9 + 2 * i, 8 + 2 * i, fp, i32(-2 * (pairs + i + 1))))
	}
}

fn (mut g Gen) emit_restore_callee_pairs(pairs int) {
	for i in 0 .. pairs {
		g.emit32(asm_ldp_offset(Reg(20 + 2 * i), Reg(19 + 2 * i), fp, i32(-2 * (i + 1))))
	}
	for i in 0 .. g.saved_float_pairs {
		g.emit32(asm_ldp_d_offset(9 + 2 * i, 8 + 2 * i, fp, i32(-2 * (pairs + i + 1))))
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
