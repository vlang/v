// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// --- Mem2Reg (Promote Allocas) ---
// All data structures use flat arrays indexed by value_id or block_id
// to avoid map operations which are extremely slow in the ARM64 backend.
struct Mem2RegCtx {
mut:
	// Indexed by alloc_id (value_id): list of block_ids where this alloc is stored to
	defs [][]int
	// Indexed by alloc_id (value_id): list of block_ids where this alloc is loaded from
	uses [][]int
	// Indexed by block_id: list of alloc_ids that need phi nodes in this block
	phi_placements [][]int
	// Indexed by alloc_id (value_id): stack of current SSA values during renaming
	stacks [][]int
	// Track which alloc_ids have been initialized (for cleanup)
	is_promotable []bool
	// Track which block_ids have phi placements (for cleanup and iteration)
	phi_blocks []int
}

fn promote_memory_to_register(mut m ssa.Module) {
	n_values := m.values.len
	n_blocks := m.blocks.len

	// Pre-allocate flat arrays ONCE outside the function loop.
	mut ctx := Mem2RegCtx{
		defs:           [][]int{len: n_values}
		uses:           [][]int{len: n_values}
		phi_placements: [][]int{len: n_blocks}
		stacks:         [][]int{len: n_values}
		is_promotable:  []bool{len: n_values}
		phi_blocks:     []int{}
	}

	// Pre-allocate stack_counts array (for rename_recursive).
	mut stack_counts := []int{len: n_values}
	for sci in 0 .. n_values {
		stack_counts[sci] = -1
	}

	// Dominance frontier: indexed by block_id
	mut df := [][]int{len: n_blocks}
	// Track which blocks have DF entries for cleanup
	mut df_blocks := []int{}

	// Per-alloc visited/has_phi bitsets (reused across allocs within a function)
	mut visited := []bool{len: n_blocks}
	mut has_phi := []bool{len: n_blocks}

	for fi in 0 .. m.funcs.len {
		// 1. Analyze Allocas
		mut promotable := []int{}
		for blk_id in m.funcs[fi].blocks {
			n_blk_instrs := m.blocks[blk_id].instrs.len
			for ii in 0 .. n_blk_instrs {
				val_id := m.blocks[blk_id].instrs[ii]
				instr := m.instrs[m.values[val_id].index]
				if instr.op == .alloca {
					if is_promotable(m, val_id) {
						promotable << val_id
						ctx.is_promotable[val_id] = true
					}
				}

				if instr.op == .store {
					ptr := instr.operands[1]
					// Avoid duplicate def entries for same block
					if ptr < n_values && blk_id !in ctx.defs[ptr] {
						ctx.defs[ptr] << blk_id
					}
				} else if instr.op == .load {
					ptr := instr.operands[0]
					// Avoid duplicate use entries for same block
					if ptr < n_values && blk_id !in ctx.uses[ptr] {
						ctx.uses[ptr] << blk_id
					}
				}
			}
		}

		// 2. Compute Dominance Frontier (flat array version)
		compute_dominance_frontier_flat(m, fi, mut df, mut df_blocks)

		// 3. Insert Phis (Dominance Frontier)
		for alloc_id in promotable {
			mut worklist := ctx.defs[alloc_id].clone()
			// Track blocks we mark visited/has_phi for cleanup
			mut visited_list := []int{}
			mut has_phi_list := []int{}

			for worklist.len > 0 {
				b := worklist.pop()
				if b < 0 || b >= n_blocks {
					continue
				}
				for d in df[b] {
					if !has_phi[d] {
						ctx.phi_placements[d] << alloc_id
						if d !in ctx.phi_blocks {
							ctx.phi_blocks << d
						}
						has_phi[d] = true
						has_phi_list << d
						if !visited[d] {
							visited[d] = true
							visited_list << d
							worklist << d
						}
					}
				}
			}

			// Reset visited/has_phi for this alloc
			for v in visited_list {
				visited[v] = false
			}
			for v in has_phi_list {
				has_phi[v] = false
			}
		}

		// Insert Phis
		for blk_id in ctx.phi_blocks {
			for alloc_id in ctx.phi_placements[blk_id] {
				typ := m.type_store.types[m.values[alloc_id].typ].elem_type
				phi_val := m.add_instr_front(.phi, blk_id, typ, [])
				m.values[phi_val].name = '${m.values[alloc_id].name}.phi_${blk_id}'
			}
		}

		// 4. Rename Variables
		if m.funcs[fi].blocks.len > 0 {
			entry := m.funcs[fi].blocks[0]
			rename_recursive(mut m, entry, mut ctx, promotable, mut stack_counts)
		}

		// 5. Cleanup for next function: reset only entries we touched
		for alloc_id in promotable {
			ctx.defs[alloc_id] = []
			ctx.uses[alloc_id] = []
			ctx.stacks[alloc_id] = []
			ctx.is_promotable[alloc_id] = false
		}
		for blk_id in ctx.phi_blocks {
			ctx.phi_placements[blk_id] = []
		}
		ctx.phi_blocks = []
		for bid in df_blocks {
			df[bid] = []
		}
		df_blocks = []
	}
}

fn is_promotable(m &ssa.Module, alloc_id int) bool {
	// Keep array-backed slots in memory. Promoting pointer-to-array allocas can
	// lose correct addressing semantics for fixed-array literals/indexing.
	if alloc_id > 0 && alloc_id < m.values.len {
		alloc_typ_id := m.values[alloc_id].typ
		if alloc_typ_id > 0 && alloc_typ_id < m.type_store.types.len {
			alloc_typ := m.type_store.types[alloc_typ_id]
			if alloc_typ.kind == .ptr_t {
				elem_typ_id := alloc_typ.elem_type
				if elem_typ_id > 0 && elem_typ_id < m.type_store.types.len {
					elem_typ := m.type_store.types[elem_typ_id]
					if elem_typ.kind == .ptr_t {
						return false
					}
					if elem_typ.kind == .array_t {
						return false
					}
				}
			}
		}
	}

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

fn compute_dominance_frontier_flat(m &ssa.Module, func_idx int, mut df [][]int, mut df_blocks []int) {
	func_blocks := m.funcs[func_idx].blocks
	for bi in 0 .. func_blocks.len {
		blk_id := func_blocks[bi]
		if blk_id < 0 || blk_id >= m.blocks.len {
			continue
		}
		num_preds := m.blocks[blk_id].preds.len
		if num_preds >= 2 {
			for pi in 0 .. num_preds {
				mut runner := m.blocks[blk_id].preds[pi]
				idom := m.blocks[blk_id].idom
				// Safety check: idom != -1
				for runner != -1 && runner != idom {
					if runner < 0 || runner >= m.blocks.len {
						break
					}
					// Avoid duplicate entries in dominance frontier
					if blk_id !in df[runner] {
						df[runner] << blk_id
						if runner !in df_blocks {
							df_blocks << runner
						}
					}
					if runner == m.blocks[runner].idom {
						break
					}
					runner = m.blocks[runner].idom
				}
			}
		}
	}
}

fn rename_recursive(mut m ssa.Module, blk_id int, mut ctx Mem2RegCtx, promotable []int, mut stack_counts []int) {
	// Save stack counts BEFORE pushing phis (so we can pop them later)
	// Uses pre-allocated flat array indexed by alloc_id instead of map
	for alloc_id in promotable {
		stack_counts[alloc_id] = ctx.stacks[alloc_id].len
	}

	// 1. Push Phis to stack
	if blk_id < ctx.phi_placements.len && ctx.phi_placements[blk_id].len > 0 {
		for alloc_id in ctx.phi_placements[blk_id] {
			name := '${m.values[alloc_id].name}.phi_${blk_id}'
			n_instrs := m.blocks[blk_id].instrs.len
			for ii in 0 .. n_instrs {
				val_id := m.blocks[blk_id].instrs[ii]
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

	n_instrs2 := m.blocks[blk_id].instrs.len
	for ii in 0 .. n_instrs2 {
		val_id := m.blocks[blk_id].instrs[ii]
		instr := m.instrs[m.values[val_id].index]
		match instr.op {
			.store {
				ptr := instr.operands[1]
				if ptr < ctx.is_promotable.len && ctx.is_promotable[ptr] {
					ctx.stacks[ptr] << instr.operands[0]
					instrs_to_nop << val_id
				}
			}
			.load {
				ptr := instr.operands[0]
				if ptr < ctx.is_promotable.len && ctx.is_promotable[ptr] {
					stack := ctx.stacks[ptr]
					mut repl := 0
					if stack.len > 0 {
						repl = stack.last()
					} else {
						// Undef - reading uninitialized memory
						res_type := m.values[val_id].typ
						repl = m.get_or_add_const(res_type, 'undef')
					}
					m.replace_uses(val_id, repl)
					instrs_to_nop << val_id
				}
			}
			.alloca {
				if val_id < ctx.is_promotable.len && ctx.is_promotable[val_id] {
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
	n_succs := m.blocks[blk_id].succs.len
	for si in 0 .. n_succs {
		succ_id := m.blocks[blk_id].succs[si]
		if succ_id < ctx.phi_placements.len && ctx.phi_placements[succ_id].len > 0 {
			for alloc_id in ctx.phi_placements[succ_id] {
				n_succ_instrs := m.blocks[succ_id].instrs.len
				for vi in 0 .. n_succ_instrs {
					vid := m.blocks[succ_id].instrs[vi]
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
							val = m.get_or_add_const(typ, 'undef')
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
	n_children := m.blocks[blk_id].dom_tree.len
	for ci in 0 .. n_children {
		child := m.blocks[blk_id].dom_tree[ci]
		rename_recursive(mut m, child, mut ctx, promotable, mut stack_counts)
	}

	// 5. Pop Stacks - only iterate promotable allocas (not all entries)
	for alloc_id in promotable {
		count := stack_counts[alloc_id]
		if count >= 0 {
			for ctx.stacks[alloc_id].len > count {
				ctx.stacks[alloc_id].pop()
			}
		}
	}
}
