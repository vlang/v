module optimize

import v.ssa

// Inlining of small direct calls.
//
// Candidates follow Go's rule (cmd/compile/internal/inline/inl.go): a function is
// inlined when its cost fits a budget, calls cost more than other nodes, and
// callees are processed before their callers, so a small wrapper of a small
// function can itself be inlined. Zig's Sema expands `inline` calls the same way
// on its own IR (analyzeInlineBody in src/Sema.zig).
//
// Go counts AST nodes against a budget of 80; here the unit is the SSA
// instruction of unoptimized code, which is about three times denser.

const inline_budget = 40
const inline_call_cost = 12
const inline_caller_growth_limit = 4000

fn inline_functions(mut m ssa.Module) {
	mut state := []u8{len: m.funcs.len}
	mut order := []int{cap: m.funcs.len}
	for fi in 0 .. m.funcs.len {
		inline_post_order(m, fi, mut state, mut order)
	}
	mut costs := []int{len: m.funcs.len, init: -1}
	for fi in order {
		inline_calls_in(mut m, fi, costs)
		costs[fi] = inline_function_cost(m, fi)
	}
}

// inline_post_order visits callees first; a cycle stops at the first function
// already on the path, and such calls are simply not inlined.
fn inline_post_order(m &ssa.Module, fi int, mut state []u8, mut order []int) {
	if state[fi] != 0 {
		return
	}
	state[fi] = 1
	for blk_id in m.funcs[fi].blocks {
		for val_id in m.blocks[blk_id].instrs {
			if callee := direct_callee(m, val_id) {
				inline_post_order(m, callee, mut state, mut order)
			}
		}
	}
	order << fi
}

// direct_callee returns the function a `call` instruction names, when its body
// is available.
fn direct_callee(m &ssa.Module, val_id ssa.ValueID) ?int {
	val := m.values[val_id]
	if val.kind != .instruction {
		return none
	}
	instr := m.instrs[val.index]
	if instr.op != .call || instr.operands.len == 0 {
		return none
	}
	callee := m.values[instr.operands[0]]
	if callee.kind != .func_ref || callee.index < 0 || callee.index >= m.funcs.len {
		return none
	}
	target := m.funcs[callee.index]
	if target.is_c_extern || target.is_prototype || target.blocks.len == 0 {
		return none
	}
	return callee.index
}

fn is_inline_scalar(m &ssa.Module, typ ssa.TypeID) bool {
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return false
	}
	return m.type_store.types[typ].kind !in [.struct_t, .array_t] && m.type_size(typ) <= 8
}

// inline_function_cost returns the function's cost, or -1 when it cannot be
// copied into a caller.
fn inline_function_cost(m &ssa.Module, fi int) int {
	func := m.funcs[fi]
	if func.name == 'main' || func.is_variadic || func.is_c_extern || func.blocks.len == 0
		|| func.name in m.noinline_fns {
		return -1
	}
	if func.typ != 0 && !is_inline_scalar(m, func.typ) && m.type_store.types[func.typ].kind != .void_t {
		return -1
	}
	for param in func.params {
		if !is_inline_scalar(m, m.values[param].typ) {
			return -1
		}
	}
	mut cost := 0
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			val := m.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := m.instrs[val.index]
			match instr.op {
				.inline_asm, .call_sret, .switch_ {
					return -1
				}
				.alloca {
					if instr.operands.len > 0 && m.values[instr.operands[0]].kind != .constant {
						return -1
					}
				}
				.call, .call_indirect {
					cost += inline_call_cost
				}
				.store {
					// Spilling a parameter to its slot disappears with mem2reg.
					if m.values[instr.operands[0]].kind != .argument {
						cost++
					}
				}
				else {
					cost++
				}
			}
			if cost > inline_budget {
				return -1
			}
		}
	}
	return cost
}

fn inline_calls_in(mut m ssa.Module, caller int, costs []int) {
	mut queue := m.funcs[caller].blocks.clone()
	mut grown := 0
	mut head := 0
	for head < queue.len {
		blk_id := queue[head]
		head++
		mut at := 0
		for at < m.blocks[blk_id].instrs.len {
			val_id := m.blocks[blk_id].instrs[at]
			callee := direct_callee(m, val_id) or {
				at++
				continue
			}
			if callee == caller || costs[callee] < 0 || grown > inline_caller_growth_limit
				|| !inline_arguments_match(m, val_id, callee) {
				at++
				continue
			}
			cont := inline_call(mut m, caller, blk_id, at, callee) or {
				at++
				continue
			}
			grown += costs[callee]
			queue << int(cont)
			break
		}
	}
}

fn inline_arguments_match(m &ssa.Module, call_id ssa.ValueID, callee int) bool {
	instr := m.instrs[m.values[call_id].index]
	params := m.funcs[callee].params
	if instr.operands.len - 1 != params.len {
		return false
	}
	for i, param in params {
		arg := m.values[instr.operands[i + 1]]
		if arg.typ != m.values[param].typ {
			return false
		}
	}
	return true
}

// inline_call replaces the call at position `at` of `blk_id` with a copy of the
// callee and returns the block holding the instructions that followed it.
fn inline_call(mut m ssa.Module, caller int, blk_id int, at int, callee int) ?int {
	call_id := m.blocks[blk_id].instrs[at]
	if at + 1 >= m.blocks[blk_id].instrs.len {
		return none
	}
	call := m.instrs[m.values[call_id].index]
	call_type := m.values[call_id].typ

	cont := int(m.add_block(caller, 'inline_cont'))
	tail := m.blocks[blk_id].instrs[at + 1..].clone()
	m.blocks[cont].instrs = tail
	for moved in tail {
		if m.values[moved].kind == .instruction {
			mut instr := m.instrs[m.values[moved].index]
			instr.block = ssa.BlockID(cont)
			m.instrs[m.values[moved].index] = instr
		}
	}
	mut remaining := m.blocks[blk_id].instrs.clone()
	remaining.trim(at)
	m.blocks[blk_id].instrs = remaining
	retarget_phi_predecessors(mut m, blk_id, cont)

	mut block_map := map[int]int{}
	mut new_blocks := []int{}
	for old_block in m.funcs[callee].blocks {
		duplicate := int(m.add_block(caller, 'inlined'))
		block_map[old_block] = duplicate
		new_blocks << duplicate
	}
	mut value_map := map[int]ssa.ValueID{}
	for i, param in m.funcs[callee].params {
		value_map[int(param)] = call.operands[i + 1]
	}
	entry := m.funcs[caller].blocks[0]
	mut copied := [][]ssa.ValueID{}
	mut originals := [][]ssa.ValueID{}
	for old_block in m.funcs[callee].blocks {
		mut blk_copied := []ssa.ValueID{}
		mut blk_originals := []ssa.ValueID{}
		for old_id in m.blocks[old_block].instrs {
			old := m.values[old_id]
			if old.kind != .instruction {
				continue
			}
			instr := m.instrs[old.index]
			mapped_ops := []ssa.ValueID{len: instr.operands.len}
			new_id := if instr.op == .alloca {
				// A slot of the callee belongs to the caller's frame once, not
				// once per iteration of a loop around the call.
				m.add_instr_front(.alloca, entry, instr.typ, mapped_ops)
			} else {
				m.add_instr(instr.op, ssa.BlockID(block_map[old_block]), instr.typ, mapped_ops)
			}
			mut fresh := m.instrs[m.values[new_id].index]
			fresh.pos = instr.pos
			fresh.atomic_ord = instr.atomic_ord
			fresh.inline = instr.inline
			fresh.asm_id = instr.asm_id
			m.instrs[m.values[new_id].index] = fresh
			value_map[int(old_id)] = new_id
			blk_copied << new_id
			blk_originals << old_id
			// Code after a block's terminator never runs. It exists after `$if`
			// branches that end in `return`; copied, a second `ret` would give
			// the phi a second value from the same block.
			if instr.op in [.ret, .br, .jmp, .switch_, .unreachable] {
				break
			}
		}
		copied << blk_copied
		originals << blk_originals
	}
	mut returns := []ssa.ValueID{}
	mut return_blocks := []ssa.BlockID{}
	for bi, old_block in m.funcs[callee].blocks {
		for vi, new_id in copied[bi] {
			instr := m.instrs[m.values[originals[bi][vi]].index]
			mut fresh := m.instrs[m.values[new_id].index]
			for oi, operand in instr.operands {
				fresh.operands[oi] = if instr.is_value_operand(oi) {
					value_map[int(operand)] or { operand }
				} else {
					ssa.ValueID(block_map[int(operand)] or { int(operand) })
				}
			}
			if instr.op == .ret {
				if fresh.operands.len > 0 {
					returns << fresh.operands[0]
					return_blocks << ssa.BlockID(block_map[old_block])
				}
				fresh.op = .jmp
				fresh.operands = [ssa.ValueID(cont)]
			}
			m.instrs[m.values[new_id].index] = fresh
		}
	}
	m.add_instr(.jmp, ssa.BlockID(blk_id), 0, [ssa.ValueID(new_blocks[0])])

	if m.type_store.types[call_type].kind != .void_t && call_type != 0 {
		result := if returns.len == 1 {
			returns[0]
		} else if returns.len == 0 {
			m.get_or_add_const(call_type, '0')
		} else {
			mut phi_ops := []ssa.ValueID{cap: returns.len * 2}
			for i, value in returns {
				phi_ops << value
				phi_ops << ssa.ValueID(return_blocks[i])
			}
			m.add_instr_front(.phi, ssa.BlockID(cont), call_type, phi_ops)
		}
		m.replace_uses(call_id, result)
	}
	m.detach_instruction_uses(call_id)
	place_inlined_blocks(mut m, caller, blk_id, new_blocks, cont)
	return cont
}

// retarget_phi_predecessors renames `from` to `to` in the phis of the blocks the
// moved terminator reaches.
fn retarget_phi_predecessors(mut m ssa.Module, from int, to int) {
	instrs := m.blocks[to].instrs
	if instrs.len == 0 {
		return
	}
	last := m.values[instrs.last()]
	if last.kind != .instruction {
		return
	}
	terminator := m.instrs[last.index]
	targets := match terminator.op {
		.br { [int(terminator.operands[1]), int(terminator.operands[2])] }
		.jmp { [int(terminator.operands[0])] }
		else { []int{} }
	}
	for target in targets {
		for val_id in m.blocks[target].instrs {
			val := m.values[val_id]
			if val.kind != .instruction || m.instrs[val.index].op != .phi {
				continue
			}
			mut phi := m.instrs[val.index]
			for i := 1; i < phi.operands.len; i += 2 {
				if int(phi.operands[i]) == from {
					phi.operands[i] = ssa.ValueID(to)
				}
			}
			m.instrs[val.index] = phi
		}
	}
}

// place_inlined_blocks keeps the copy next to its call, so branches stay short
// and live intervals stay tight.
fn place_inlined_blocks(mut m ssa.Module, caller int, blk_id int, new_blocks []int, cont int) {
	mut layout := []ssa.BlockID{cap: m.funcs[caller].blocks.len}
	for existing in m.funcs[caller].blocks {
		if existing == cont || existing in new_blocks {
			continue
		}
		layout << existing
		if existing == blk_id {
			for added in new_blocks {
				layout << ssa.BlockID(added)
			}
			layout << ssa.BlockID(cont)
		}
	}
	m.funcs[caller].blocks = layout
}
