// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// --- Mem2Reg (Promote Allocas) ---
struct Mem2RegCtx {
mut:
	defs           map[int][]int
	uses           map[int][]int
	phi_placements map[int][]int
	stacks         map[int][]int
}

fn promote_memory_to_register(mut m ssa.Module) {
	for func in m.funcs {
		mut ctx := Mem2RegCtx{
			defs:           map[int][]int{}
			uses:           map[int][]int{}
			phi_placements: map[int][]int{}
			stacks:         map[int][]int{}
		}

		// 1. Analyze Allocas
		mut promotable := []int{}
		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			for val_id in blk.instrs {
				instr := m.instrs[m.values[val_id].index]
				if instr.op == .alloca {
					if is_promotable(m, val_id) {
						promotable << val_id
						ctx.stacks[val_id] = []
					}
				}

				if instr.op == .store {
					ptr := instr.operands[1]
					// Avoid duplicate def entries for same block
					if blk_id !in ctx.defs[ptr] {
						ctx.defs[ptr] << blk_id
					}
				} else if instr.op == .load {
					ptr := instr.operands[0]
					// Avoid duplicate use entries for same block
					if blk_id !in ctx.uses[ptr] {
						ctx.uses[ptr] << blk_id
					}
				}
			}
		}

		// 2. Insert Phis (Dominance Frontier)
		df := compute_dominance_frontier(m, func)

		for alloc_id in promotable {
			mut worklist := ctx.defs[alloc_id].clone()
			mut visited := map[int]bool{}
			mut has_phi := map[int]bool{}

			for worklist.len > 0 {
				b := worklist.pop()
				for d in df[b] {
					if !has_phi[d] {
						ctx.phi_placements[d] << alloc_id
						has_phi[d] = true
						if !visited[d] {
							visited[d] = true
							worklist << d
						}
					}
				}
			}
		}

		// Insert Phis
		for blk_id, allocs in ctx.phi_placements {
			for alloc_id in allocs {
				typ := m.type_store.types[m.values[alloc_id].typ].elem_type
				phi_val := m.add_instr_front(.phi, blk_id, typ, [])
				m.values[phi_val].name = '${m.values[alloc_id].name}.phi_${blk_id}'
			}
		}

		// 3. Rename Variables
		if func.blocks.len > 0 {
			entry := func.blocks[0]
			rename_recursive(mut m, entry, mut ctx)
		}
	}
}

fn is_promotable(m ssa.Module, alloc_id int) bool {
	uses := m.values[alloc_id].uses
	for u in uses {
		if u >= m.values.len {
			continue
		}
		user := m.values[u]
		if user.kind != .instruction {
			return false
		}
		instr := m.instrs[user.index]
		match instr.op {
			.load {
				if instr.operands.len == 0 || instr.operands[0] != alloc_id {
					return false
				}
			}
			.store {
				// Only safe if used as pointer (index 1)
				if instr.operands.len < 2 || instr.operands[1] != alloc_id {
					return false
				}
			}
			else {
				// Escape (GEP, Call, Phi, etc.)
				return false
			}
		}
	}
	return true
}

fn compute_dominance_frontier(m ssa.Module, func ssa.Function) map[int][]int {
	mut df := map[int][]int{}

	for blk_id in func.blocks {
		preds := m.blocks[blk_id].preds
		if preds.len >= 2 {
			for p in preds {
				mut runner := p
				idom := m.blocks[blk_id].idom
				// Safety check: idom != -1
				for runner != -1 && runner != idom {
					// Avoid duplicate entries in dominance frontier
					if blk_id !in df[runner] {
						df[runner] << blk_id
					}
					if runner == m.blocks[runner].idom {
						break
					}
					runner = m.blocks[runner].idom
				}
			}
		}
	}
	return df
}

fn rename_recursive(mut m ssa.Module, blk_id int, mut ctx Mem2RegCtx) {
	blk := m.blocks[blk_id]

	// Save stack counts BEFORE pushing phis (so we can pop them later)
	mut stack_counts := map[int]int{}
	for k, v in ctx.stacks {
		stack_counts[k] = v.len
	}

	// 1. Push Phis to stack
	if phis := ctx.phi_placements[blk_id] {
		for alloc_id in phis {
			name := '${m.values[alloc_id].name}.phi_${blk_id}'
			for val_id in blk.instrs {
				instr := m.instrs[m.values[val_id].index]
				if instr.op != .phi {
					break
				}
				if m.values[val_id].name == name {
					ctx.stacks[alloc_id] << val_id
					break
				}
			}
		}
	}

	// 2. Process Instructions
	mut instrs_to_nop := []int{}

	for val_id in blk.instrs {
		instr := m.instrs[m.values[val_id].index]
		match instr.op {
			.store {
				ptr := instr.operands[1]
				val := instr.operands[0]
				if _ := ctx.stacks[ptr] {
					ctx.stacks[ptr] << val
					instrs_to_nop << val_id
				}
			}
			.load {
				ptr := instr.operands[0]
				if stack := ctx.stacks[ptr] {
					mut repl := 0
					if stack.len > 0 {
						repl = stack.last()
					} else {
						// Undef - reading uninitialized memory
						res_type := m.values[val_id].typ
						repl = m.add_value_node(.constant, res_type, 'undef', 0)
					}
					m.replace_uses(val_id, repl)
					instrs_to_nop << val_id
				}
			}
			.alloca {
				if _ := ctx.stacks[val_id] {
					instrs_to_nop << val_id
				}
			}
			else {}
		}
	}

	for vid in instrs_to_nop {
		m.instrs[m.values[vid].index].op = .bitcast
		m.instrs[m.values[vid].index].operands = []
	}

	// 3. Update Successor Phi Operands
	for succ_id in blk.succs {
		if phis := ctx.phi_placements[succ_id] {
			for alloc_id in phis {
				succ_blk := m.blocks[succ_id]
				for vid in succ_blk.instrs {
					v := m.values[vid]
					if v.kind != .instruction {
						continue
					}
					ins := m.instrs[v.index]
					if ins.op == .phi && v.name == '${m.values[alloc_id].name}.phi_${succ_id}' {
						mut val := 0
						if ctx.stacks[alloc_id].len > 0 {
							val = ctx.stacks[alloc_id].last()
						} else {
							// Undef - reading uninitialized memory
							typ := m.type_store.types[m.values[alloc_id].typ].elem_type
							val = m.add_value_node(.constant, typ, 'undef', 0)
						}
						m.instrs[v.index].operands << val
						m.instrs[v.index].operands << m.blocks[blk_id].val_id
						// FIX: Update uses so DCE doesn't remove the value
						if val < m.values.len && vid !in m.values[val].uses {
							m.values[val].uses << vid
						}
					}
				}
			}
		}
	}

	// 4. Recurse Dom Children
	for child in blk.dom_tree {
		rename_recursive(mut m, child, mut ctx)
	}

	// 5. Pop Stacks
	for k, count in stack_counts {
		for ctx.stacks[k].len > count {
			ctx.stacks[k].pop()
		}
	}
}
