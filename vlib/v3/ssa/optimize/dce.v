module optimize

import v3.ssa

// dead_code_elimination supports dead code elimination handling for optimize.
fn dead_code_elimination(mut m ssa.Module) {
	mut state := collect_dead_store_state(m)
	mut worklist := []int{}
	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			for val_id in m.blocks[blk_id].instrs {
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				instr := m.instrs[val.index]
				if !instruction_has_side_effects(instr.op) && val.uses.len == 0 {
					worklist << val_id
				}
			}
		}
	}
	for alloca_id, stores in state.stores {
		if alloca_id !in state.escapes && state.load_counts[alloca_id] == 0 {
			worklist << stores
		}
	}
	if worklist.len == 0 {
		return
	}

	mut dead := []bool{len: m.values.len}
	for worklist.len > 0 {
		val_id := worklist.pop()
		if val_id <= 0 || val_id >= m.values.len || dead[val_id]
			|| m.values[val_id].kind != .instruction {
			continue
		}
		dead[val_id] = true
		instr := m.instrs[m.values[val_id].index]
		if instr.op == .load {
			if alloca_id := state.load_ptrs[val_id] {
				remaining := state.load_counts[alloca_id] - 1
				state.load_counts[alloca_id] = remaining
				if remaining == 0 && alloca_id !in state.escapes {
					if stores := state.stores[alloca_id] {
						worklist << stores
					}
				}
			}
		}
		for oi, op_id in instr.operands {
			if !instr.is_value_operand(oi) {
				continue
			}
			remove_use(mut m, op_id, val_id)
			if op_id > 0 && op_id < m.values.len && m.values[op_id].kind == .instruction
				&& m.values[op_id].uses.len == 0 {
				op_instr := m.instrs[m.values[op_id].index]
				if !instruction_has_side_effects(op_instr.op) {
					worklist << op_id
				}
			}
		}
	}

	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			blk := m.blocks[blk_id]
			mut removed := 0
			for val_id in blk.instrs {
				if val_id > 0 && val_id < dead.len && dead[val_id] {
					removed++
				}
			}
			if removed == 0 {
				continue
			}
			mut kept := []ssa.ValueID{cap: blk.instrs.len - removed}
			for val_id in blk.instrs {
				if val_id <= 0 || val_id >= dead.len || !dead[val_id] {
					kept << val_id
				}
			}
			mut compacted := blk
			compacted.instrs = kept
			m.blocks[blk_id] = compacted
		}
	}
}

fn instruction_has_side_effects(op ssa.OpCode) bool {
	return op in [.store, .call, .call_indirect, .call_sret, .ret, .br, .jmp, .switch_, .unreachable,
		.assign, .fence, .cmpxchg, .atomicrmw, .go_call, .spawn_call]
}

// remove_use updates remove use state for optimize.
fn remove_use(mut m ssa.Module, val_id int, user_id int) {
	if val_id < 0 || val_id >= m.values.len {
		return
	}
	mut val := m.values[val_id]
	for i := val.uses.len - 1; i >= 0; i-- {
		if val.uses[i] == user_id {
			val.uses.delete(i)
		}
	}
	m.values[val_id] = val
}

struct DeadStoreState {
mut:
	stores      map[int][]int
	load_counts map[int]int
	load_ptrs   map[int]int
	escapes     map[int]bool
}

fn collect_dead_store_state(m &ssa.Module) DeadStoreState {
	mut state := DeadStoreState{
		stores:      map[int][]int{}
		load_counts: map[int]int{}
		load_ptrs:   map[int]int{}
		escapes:     map[int]bool{}
	}
	mut allocas := map[int]bool{}
	for func in m.funcs {
		for blk_id in func.blocks {
			for val_id in m.blocks[blk_id].instrs {
				val := m.values[val_id]
				if val.kind == .instruction && m.instrs[val.index].op in [.alloca, .heap_alloc] {
					allocas[val_id] = true
				}
			}
		}
	}
	for alloca_id, _ in allocas {
		for use_id in m.values[alloca_id].uses {
			if use_id <= 0 || use_id >= m.values.len || m.values[use_id].kind != .instruction {
				state.escapes[alloca_id] = true
				continue
			}
			instr := m.instrs[m.values[use_id].index]
			if instr.op == .store && instr.operands.len > 1 && instr.operands[1] == alloca_id {
				mut stores := state.stores[alloca_id]
				stores << use_id
				state.stores[alloca_id] = stores
			} else if instr.op == .load && instr.operands.len > 0 && instr.operands[0] == alloca_id {
				state.load_counts[alloca_id]++
				state.load_ptrs[use_id] = alloca_id
			} else {
				state.escapes[alloca_id] = true
			}
		}
	}
	return state
}
