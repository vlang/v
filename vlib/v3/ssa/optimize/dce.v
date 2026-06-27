module optimize

import v3.ssa

// dead_code_elimination supports dead code elimination handling for optimize.
fn dead_code_elimination(mut m ssa.Module) {
	mut changed := true
	for changed {
		changed = false
		for fi in 0 .. m.funcs.len {
			dead_stores := find_dead_stores(m, m.funcs[fi])

			for blk_id in m.funcs[fi].blocks {
				mut dead_instrs := map[int]bool{}
				for val_id in m.blocks[blk_id].instrs {
					val := m.values[val_id]
					if val.kind != .instruction {
						continue
					}
					instr := m.instrs[val.index]
					if instr.op == .store && val_id in dead_stores {
						dead_instrs[val_id] = true
						continue
					}
					has_side_effects := instr.op in [.store, .call, .call_indirect, .call_sret,
						.ret, .br, .jmp, .switch_, .unreachable, .assign, .fence, .cmpxchg,
						.atomicrmw, .go_call, .spawn_call]
					if !has_side_effects && val.uses.len == 0 {
						dead_instrs[val_id] = true
					}
				}

				if dead_instrs.len > 0 {
					for val_id, _ in dead_instrs {
						instr := m.instrs[m.values[val_id].index]

						for op_id in instr.value_operands() {
							remove_use(mut m, op_id, val_id)
						}
					}
					mut new_instrs := []int{cap: m.blocks[blk_id].instrs.len}
					for val_id in m.blocks[blk_id].instrs {
						if val_id !in dead_instrs {
							new_instrs << val_id
						}
					}
					mut blk := m.blocks[blk_id]
					blk.instrs = new_instrs
					m.blocks[blk_id] = blk
					changed = true
				}
			}
		}
	}
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

// find_dead_stores resolves find dead stores information for optimize.
fn find_dead_stores(m &ssa.Module, func ssa.Function) map[int]bool {
	mut dead_stores := map[int]bool{}
	mut allocas := map[int]bool{}
	mut alloca_stores := map[int][]int{}
	mut alloca_escapes := map[int]bool{}

	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			val := m.values[val_id]
			if val.kind != .instruction {
				continue
			}
			op := m.instrs[val.index].op
			if op == .alloca || op == .heap_alloc {
				allocas[val_id] = true
			}
		}
	}

	for alloca_id, _ in allocas {
		for use_id in m.values[alloca_id].uses {
			if use_id < 0 || use_id >= m.values.len {
				continue
			}
			val := m.values[use_id]
			if val.kind != .instruction {
				alloca_escapes[alloca_id] = true
				continue
			}
			instr := m.instrs[val.index]
			match instr.op {
				.store {
					if instr.operands.len > 1 && instr.operands[1] == alloca_id {
						alloca_stores[alloca_id] << use_id
					} else {
						alloca_escapes[alloca_id] = true
					}
				}
				.load {}
				else {
					alloca_escapes[alloca_id] = true
				}
			}
		}
	}

	// An alloca with no loads and no escapes → all stores to it are dead
	for alloca_id, _ in allocas {
		if alloca_id in alloca_escapes {
			continue
		}
		mut has_load := false
		for use_id in m.values[alloca_id].uses {
			if use_id >= 0 && use_id < m.values.len && m.values[use_id].kind == .instruction {
				if m.instrs[m.values[use_id].index].op == .load {
					has_load = true
					break
				}
			}
		}
		if !has_load {
			if store_ids := alloca_stores[alloca_id] {
				for store_id in store_ids {
					dead_stores[store_id] = true
				}
			}
		}
	}

	return dead_stores
}
