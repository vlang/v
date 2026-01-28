// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

fn dead_code_elimination(mut m ssa.Module) bool {
	mut any_changed := false
	mut changed := true
	for changed {
		changed = false
		for func in m.funcs {
			for blk_id in func.blocks {
				// First pass: find dead instructions (don't modify yet)
				mut dead_instrs := []int{}
				for val_id in m.blocks[blk_id].instrs {
					val := m.values[val_id]
					if val.kind == .instruction {
						instr := m.instrs[val.index]
						side_effects := instr.op in [.store, .call, .call_indirect, .ret, .br,
							.jmp, .switch_, .unreachable, .assign, .fence, .atomicrmw]
						if !side_effects && val.uses.len == 0 {
							dead_instrs << val_id
						}
					}
				}

				// Only rebuild the instruction list if we found dead instructions
				if dead_instrs.len > 0 {
					// Remove uses from dead instructions
					for val_id in dead_instrs {
						instr := m.instrs[m.values[val_id].index]
						for op_id in instr.operands {
							remove_use(mut m, op_id, val_id)
						}
					}

					// Build set for O(1) lookup
					mut dead_set := map[int]bool{}
					for d in dead_instrs {
						dead_set[d] = true
					}

					// Filter out dead instructions
					mut new_instrs := []int{cap: m.blocks[blk_id].instrs.len}
					for val_id in m.blocks[blk_id].instrs {
						if !dead_set[val_id] {
							new_instrs << val_id
						}
					}
					m.blocks[blk_id].instrs = new_instrs
					changed = true
					any_changed = true
				}
			}
		}
	}
	return any_changed
}

fn remove_use(mut m ssa.Module, val_id int, user_id int) {
	if val_id >= m.values.len {
		return
	}
	mut val := &m.values[val_id]
	// Remove all occurrences (handles instructions that use same value multiple times)
	// Iterate in reverse to safely delete while iterating
	for i := val.uses.len - 1; i >= 0; i-- {
		if val.uses[i] == user_id {
			val.uses.delete(i)
		}
	}
}
