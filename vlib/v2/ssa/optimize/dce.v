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
			// Dead store elimination: find stores to local allocas that are never read
			dead_stores := find_dead_stores(m, func)

			for blk_id in func.blocks {
				// First pass: find dead instructions (don't modify yet)
				mut dead_instrs := []int{}
				for val_id in m.blocks[blk_id].instrs {
					val := m.values[val_id]
					if val.kind == .instruction {
						instr := m.instrs[val.index]
						// Check if this is a dead store
						if instr.op == .store && val_id in dead_stores {
							dead_instrs << val_id
							continue
						}
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

// Find stores to local allocas that are never read (dead stores).
// A store is dead if:
// 1. It stores to a local alloca
// 2. The alloca is ONLY used by stores (no loads, GEPs, calls, or other uses)
//
// We must be conservative: if the alloca pointer escapes (used by GEP, passed to
// a function, etc.), we cannot eliminate stores because the value might be read
// through the escaped pointer.
fn find_dead_stores(m ssa.Module, func ssa.Function) map[int]bool {
	mut dead_stores := map[int]bool{}

	// Find all local allocas
	mut allocas := map[int]bool{}
	mut alloca_stores := map[int][]int{} // alloca_id -> list of store val_ids
	mut alloca_escapes := map[int]bool{} // alloca_id -> true if pointer escapes

	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			val := m.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := m.instrs[val.index]

			if instr.op == .alloca || instr.op == .heap_alloc {
				allocas[val_id] = true
			}
		}
	}

	// Check how each alloca is used
	for alloca_id, _ in allocas {
		alloca_val := m.values[alloca_id]
		for use_id in alloca_val.uses {
			use_val := m.values[use_id]
			if use_val.kind != .instruction {
				// Non-instruction use - alloca escapes
				alloca_escapes[alloca_id] = true
				continue
			}
			use_instr := m.instrs[use_val.index]
			match use_instr.op {
				.store {
					// Check if alloca is the destination (operand[1]), not the value being stored
					if use_instr.operands.len > 1 && use_instr.operands[1] == alloca_id {
						alloca_stores[alloca_id] << use_id
					} else {
						// Alloca is being stored somewhere - it escapes
						alloca_escapes[alloca_id] = true
					}
				}
				.load {
					// Alloca is being read - stores are not dead
					alloca_escapes[alloca_id] = true
				}
				else {
					// Any other use (GEP, call, etc.) - alloca escapes
					alloca_escapes[alloca_id] = true
				}
			}
		}
	}

	// Mark stores to non-escaping allocas as dead
	for alloca_id, store_ids in alloca_stores {
		if !alloca_escapes[alloca_id] {
			for store_id in store_ids {
				dead_stores[store_id] = true
			}
		}
	}

	return dead_stores
}
