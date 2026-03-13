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
	// Indexed by block_id: parallel to phi_placements, stores the phi value_ids
	// phi_vals[blk_id][i] is the phi val_id for phi_placements[blk_id][i]
	phi_vals [][]int
	// Indexed by alloc_id (value_id): stack of current SSA values during renaming
	stacks [][]int
	// Track which alloc_ids have been initialized (for cleanup)
	is_promotable []bool
	// Track which block_ids have phi placements (for cleanup and iteration)
	phi_blocks []int
	// Deferred phi operands: flat list of [instr_idx, val, bvid, instr_idx, val, bvid, ...].
	// Applied after rename to avoid m.instrs[idx].operands << x which is broken
	// in ARM64 self-hosted binaries (chained array-element-field-append bug).
	deferred_phi_ops []int
}

fn promote_memory_to_register(mut m ssa.Module, dom DomInfo, cfg &CfgData) {
	n_values := m.values.len
	n_blocks := m.blocks.len

	// Pre-allocate flat arrays ONCE outside the function loop.
	mut ctx := Mem2RegCtx{
		defs:           [][]int{len: n_values}
		uses:           [][]int{len: n_values}
		phi_placements: [][]int{len: n_blocks}
		phi_vals:       [][]int{len: n_blocks}
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

	mut total_allocas := 0
	mut total_promoted := 0
	mut total_not_promoted := 0
	mut total_blocks_visited := 0
	mut total_blocks_skipped := 0
	mut total_blocks_in_funcs := 0

	for fi in 0 .. m.funcs.len {
		// 1. Analyze Allocas
		mut promotable := []int{}
		func := m.funcs[fi]
		n_func_blocks := func.blocks.len
		for fbi in 0 .. n_func_blocks {
			blk_id := func.blocks[fbi]
			blk := m.blocks[blk_id]
			n_blk_instrs := blk.instrs.len
			for ii in 0 .. n_blk_instrs {
				val_id := blk.instrs[ii]
				val := m.values[val_id]
				instr := m.instrs[val.index]
				if instr.op == .alloca {
					total_allocas += 1
					if is_promotable(m, val_id) {
						promotable << val_id
						ctx.is_promotable[val_id] = true
						total_promoted += 1
					} else {
						total_not_promoted += 1
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
		compute_dominance_frontier_flat(m, fi, &dom, cfg, mut df, mut df_blocks)

		// 3. Insert Phis (Dominance Frontier)
		n_promotable := promotable.len
		for pri in 0 .. n_promotable {
			alloc_id := promotable[pri]
			mut worklist := ctx.defs[alloc_id].clone()
			// Track blocks we mark visited/has_phi for cleanup
			mut visited_list := []int{}
			mut has_phi_list := []int{}

			for worklist.len > 0 {
				b := worklist.pop()
				if b < 0 || b >= n_blocks {
					continue
				}
				n_df_b := df[b].len
				for di in 0 .. n_df_b {
					d := df[b][di]
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
			for vli in 0 .. visited_list.len {
				visited[visited_list[vli]] = false
			}
			for hli in 0 .. has_phi_list.len {
				has_phi[has_phi_list[hli]] = false
			}
		}

		// Insert Phis
		n_phi_blocks := ctx.phi_blocks.len
		for pbi in 0 .. n_phi_blocks {
			blk_id := ctx.phi_blocks[pbi]
			n_phi_allocs := ctx.phi_placements[blk_id].len
			for pai in 0 .. n_phi_allocs {
				alloc_id := ctx.phi_placements[blk_id][pai]
				alloc_val := m.values[alloc_id]
				alloc_typ := m.type_store.types[alloc_val.typ]
				typ := alloc_typ.elem_type
				phi_val := m.add_instr_front(.phi, blk_id, typ, [])
				// Avoid m.values[phi_val].name = ... -- chained field assign broken in ARM64 self-hosted
				mut pv := m.values[phi_val]
				pv.name = '${alloc_val.name}.phi_${blk_id}'
				m.values[phi_val] = pv
				// Store phi_val_id in parallel array for direct lookup (no string matching)
				ctx.phi_vals[blk_id] << phi_val
			}
		}

		// 4. Rename Variables
		func2 := m.funcs[fi]
		if func2.blocks.len > 0 {
			entry := func2.blocks[0]
			total_blocks_in_funcs += func2.blocks.len
			bv, bs := rename_recursive(mut m, entry, mut ctx, promotable, mut stack_counts,
				&dom, cfg)
			total_blocks_visited += bv
			total_blocks_skipped += bs
		}

		// 4b. Apply deferred phi operands.
		// Builds complete operand lists for each phi and assigns them all at once.
		// This avoids m.instrs[idx].operands << x which is broken in v3-compiled binaries
		// (chained array-element-field-append copies the struct instead of modifying in-place).
		if ctx.deferred_phi_ops.len > 0 {
			n_deferred := ctx.deferred_phi_ops.len
			n_phi_blocks3 := ctx.phi_blocks.len
			mut n_applied := 0
			mut n_verified := 0
			mut n_mismatch := 0
			for pbi3 in 0 .. n_phi_blocks3 {
				pb_id := ctx.phi_blocks[pbi3]
				n_phi_allocs3 := ctx.phi_vals[pb_id].len
				for pai3 in 0 .. n_phi_allocs3 {
					phi_vid := ctx.phi_vals[pb_id][pai3]
					phi_v := m.values[phi_vid]
					phi_instr_idx := phi_v.index
					// Collect all operands for this phi from deferred list
					mut ops := []int{}
					mut dj := 0
					for dj + 2 < n_deferred {
						if ctx.deferred_phi_ops[dj] == phi_instr_idx {
							ops << ctx.deferred_phi_ops[dj + 1]
							ops << ctx.deferred_phi_ops[dj + 2]
						}
						dj += 3
					}
					if ops.len > 0 {
						// Avoid m.instrs[X].operands = ops -- chained field assign broken in ARM64 self-hosted
						mut phi_instr := m.instrs[phi_instr_idx]
						phi_instr.operands = ops
						m.instrs[phi_instr_idx] = phi_instr
						n_applied += 1
						// Verify the assignment was actually written
						actual_len := m.instrs[phi_instr_idx].operands.len
						if actual_len != ops.len {
							n_mismatch += 1
							if n_mismatch <= 5 {
								eprintln('  DEFERRED MISMATCH: phi_vid=${phi_vid} instr=${phi_instr_idx} wanted=${ops.len} got=${actual_len}')
							}
						} else {
							n_verified += 1
							// Also verify first operand value
							if ops.len >= 2 {
								actual0 := m.instrs[phi_instr_idx].operands[0]
								actual1 := m.instrs[phi_instr_idx].operands[1]
								if actual0 != ops[0] || actual1 != ops[1] {
									n_mismatch += 1
									if n_mismatch <= 5 {
										eprintln('  DEFERRED VALUE MISMATCH: phi_vid=${phi_vid} instr=${phi_instr_idx} wanted[0]=${ops[0]},${ops[1]} got=${actual0},${actual1}')
									}
								}
							}
						}
					}
				}
			}
			if n_mismatch > 0 {
				eprintln('  DEFERRED phi ops: applied=${n_applied} verified=${n_verified} MISMATCH=${n_mismatch}')
			}
			ctx.deferred_phi_ops = []
		}
		// Update uses for phi operands (deferred from step 3)
		n_phi_blocks4 := ctx.phi_blocks.len
		for pbi4 in 0 .. n_phi_blocks4 {
			pb_id := ctx.phi_blocks[pbi4]
			n_phi_allocs4 := ctx.phi_vals[pb_id].len
			for pai4 in 0 .. n_phi_allocs4 {
				phi_vid := ctx.phi_vals[pb_id][pai4]
				phi_v := m.values[phi_vid]
				phi_instr_idx := phi_v.index
				instr := m.instrs[phi_instr_idx]
				for oi := 0; oi < instr.operands.len; oi += 2 {
					val_op := instr.operands[oi]
					if val_op < m.values.len && phi_vid !in m.values[val_op].uses {
						// Read whole struct, modify, write back (chained broken in ARM64)
						mut vop := m.values[val_op]
						vop.uses << phi_vid
						m.values[val_op] = vop
					}
				}
			}
		}

		// 5. Cleanup for next function: reset only entries we touched
		for cli in 0 .. promotable.len {
			alloc_id := promotable[cli]
			ctx.defs[alloc_id] = []
			ctx.uses[alloc_id] = []
			ctx.stacks[alloc_id] = []
			ctx.is_promotable[alloc_id] = false
		}
		n_phi_blocks2 := ctx.phi_blocks.len
		for cli2 in 0 .. n_phi_blocks2 {
			ctx.phi_placements[ctx.phi_blocks[cli2]] = []
			ctx.phi_vals[ctx.phi_blocks[cli2]] = []
		}
		ctx.phi_blocks = []
		n_df_blocks := df_blocks.len
		for cli3 in 0 .. n_df_blocks {
			df[df_blocks[cli3]] = []
		}
		df_blocks = []
	}
	eprintln('  mem2reg stats: allocas=${total_allocas} promoted=${total_promoted} not_promoted=${total_not_promoted} blocks_total=${total_blocks_in_funcs} blocks_visited=${total_blocks_visited} blocks_skipped=${total_blocks_skipped}')
}

fn is_promotable(m &ssa.Module, alloc_id int) bool {
	// Keep array-backed slots in memory. Promoting pointer-to-array allocas can
	// lose correct addressing semantics for fixed-array literals/indexing.
	if alloc_id > 0 && alloc_id < m.values.len {
		alloc_val := m.values[alloc_id]
		alloc_typ_id := alloc_val.typ
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

	alloc_val2 := m.values[alloc_id]
	uses := alloc_val2.uses
	n_uses := uses.len
	for ui in 0 .. n_uses {
		u := uses[ui]
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

fn compute_dominance_frontier_flat(m &ssa.Module, func_idx int, dom &DomInfo, cfg &CfgData, mut df [][]int, mut df_blocks []int) {
	func := m.funcs[func_idx]
	n_func_blocks := func.blocks.len
	for bi in 0 .. n_func_blocks {
		blk_id := func.blocks[bi]
		if blk_id < 0 || blk_id >= m.blocks.len {
			continue
		}
		num_preds := cfg.preds[blk_id].len
		if num_preds >= 2 {
			for pi in 0 .. num_preds {
				mut runner := cfg.preds[blk_id][pi]
				idom := dom.idom[blk_id]
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
					if runner == dom.idom[runner] {
						break
					}
					runner = dom.idom[runner]
				}
			}
		}
	}
}

// RenameFrame holds the state for one level of the iterative dom tree walk.
struct RenameFrame {
mut:
	blk_id        int
	child_idx     int   // next child to process in dom_tree
	pushed_allocs []int // alloc_ids pushed in this block (for cleanup)
	processed     bool  // whether steps 1-3 have been run for this block
}

fn rename_iterative(mut m ssa.Module, root_blk int, mut ctx Mem2RegCtx, promotable []int, mut stack_counts []int, dom &DomInfo, cfg &CfgData) (int, int) {
	// Pre-build block_val_ids[] by scanning values for basic_block kind.
	// This avoids m.blocks[blk_id].val_id which produces wrong results
	// in ARM64-compiled binaries (large struct field access bug).
	n_blks := m.blocks.len
	mut block_val_ids := []int{len: n_blks}
	n_vals := m.values.len
	mut n_bb_found := 0
	mut n_bb_valid := 0
	for vi in 0 .. n_vals {
		if m.values[vi].kind == .basic_block {
			bid := m.values[vi].index
			n_bb_found += 1
			if bid >= 0 && bid < n_blks {
				block_val_ids[bid] = vi
				n_bb_valid += 1
			}
		}
	}
	// Verify: also check using m.blocks[bid].val_id approach and compare
	mut n_match := 0
	mut n_mismatch := 0
	for bid in 0 .. n_blks {
		bval := m.blocks[bid].val_id
		if block_val_ids[bid] != bval {
			n_mismatch += 1
			if n_mismatch <= 3 {
				eprintln('  mem2reg block_val_ids MISMATCH: bid=${bid} scan=${block_val_ids[bid]} blocks=${bval}')
			}
		} else {
			n_match += 1
		}
	}
	if n_mismatch > 0 {
		eprintln('  mem2reg block_val_ids: bb_found=${n_bb_found} valid=${n_bb_valid} match=${n_match} mismatch=${n_mismatch}')
	}

	mut work := []RenameFrame{}
	work << RenameFrame{
		blk_id: root_blk
	}
	mut visited := []bool{len: m.blocks.len}
	mut blocks_visited := 0
	mut blocks_skipped := 0

	for work.len > 0 {
		fi := work.len - 1
		blk_id := work[fi].blk_id

		if !work[fi].processed {
			// Cycle detection: skip blocks already visited in this traversal
			if blk_id >= 0 && blk_id < visited.len && visited[blk_id] {
				blocks_skipped += 1
				work.pop()
				continue
			}
			if blk_id >= 0 && blk_id < visited.len {
				visited[blk_id] = true
			}
			blocks_visited += 1
			// Steps 1-3: process this block (runs once per block)
			// Avoid work[fi].processed = true — chained field assignment broken in ARM64 self-hosted.
			mut frame2 := work[fi]
			frame2.processed = true
			work[fi] = frame2

			// 1. Push Phis to stack (use phi_vals[] for direct lookup, no string matching)
			if blk_id < ctx.phi_placements.len && ctx.phi_placements[blk_id].len > 0 {
				n_phi_allocs := ctx.phi_placements[blk_id].len
				for pai in 0 .. n_phi_allocs {
					alloc_id := ctx.phi_placements[blk_id][pai]
					// Direct lookup: phi_vals[blk_id][pai] is the phi for this alloc
					if blk_id < ctx.phi_vals.len && pai < ctx.phi_vals[blk_id].len {
						phi_val_id := ctx.phi_vals[blk_id][pai]
						// Avoid ctx.stacks[X] << Y — chained append broken in ARM64 self-hosted.
						mut new_stack := ctx.stacks[alloc_id].clone()
						new_stack << phi_val_id
						ctx.stacks[alloc_id] = new_stack
						// Avoid work[fi].pushed_allocs = X — chained field assignment broken in ARM64.
						mut wf := work[fi]
						wf.pushed_allocs << alloc_id
						work[fi] = wf
					}
				}
			}

			// 2. Process Instructions
			mut instrs_to_nop := []int{}

			blk2 := m.blocks[blk_id]
			n_instrs2 := blk2.instrs.len
			for ii in 0 .. n_instrs2 {
				val_id := blk2.instrs[ii]
				val := m.values[val_id]
				instr := m.instrs[val.index]
				if instr.op == .store {
					ptr := instr.operands[1]
					if ptr < ctx.is_promotable.len && ctx.is_promotable[ptr] {
						// Avoid ctx.stacks[X] << Y — chained append broken in ARM64 self-hosted.
						mut new_stack := ctx.stacks[ptr].clone()
						new_stack << instr.operands[0]
						ctx.stacks[ptr] = new_stack
						// Avoid work[fi].pushed_allocs = X — chained field assignment broken in ARM64.
						mut wf := work[fi]
						wf.pushed_allocs << ptr
						work[fi] = wf
						instrs_to_nop << val_id
					}
				} else if instr.op == .load {
					ptr := instr.operands[0]
					if ptr < ctx.is_promotable.len && ctx.is_promotable[ptr] {
						stack := ctx.stacks[ptr]
						mut repl := 0
						if stack.len > 0 {
							repl = stack.last()
						} else {
							// Undef - reading uninitialized memory
							res_type := val.typ
							repl = m.get_or_add_const(res_type, 'undef')
						}
						m.replace_uses(val_id, repl)
						instrs_to_nop << val_id
					}
				} else if instr.op == .alloca {
					if val_id < ctx.is_promotable.len && ctx.is_promotable[val_id] {
						instrs_to_nop << val_id
					}
				}
			}

			for nopi in 0 .. instrs_to_nop.len {
				vid := instrs_to_nop[nopi]
				vid_val := m.values[vid]
				// Avoid m.instrs[X].op/operands = ... -- chained field assign broken in ARM64 self-hosted
				mut nop_instr := m.instrs[vid_val.index]
				nop_instr.op = .bitcast
				nop_instr.operands = []
				m.instrs[vid_val.index] = nop_instr
			}

			// 3. Update Successor Phi Operands
			n_succs := cfg.succs[blk_id].len
			for si in 0 .. n_succs {
				succ_id := cfg.succs[blk_id][si]
				if succ_id < ctx.phi_placements.len && ctx.phi_placements[succ_id].len > 0 {
					n_succ_phi_allocs := ctx.phi_placements[succ_id].len
					for spai in 0 .. n_succ_phi_allocs {
						alloc_id := ctx.phi_placements[succ_id][spai]
						// Direct lookup: phi_vals[succ_id][spai] is the phi for this alloc
						if succ_id < ctx.phi_vals.len && spai < ctx.phi_vals[succ_id].len {
							vid := ctx.phi_vals[succ_id][spai]
							v := m.values[vid]
							mut val := 0
							if ctx.stacks[alloc_id].len > 0 {
								val = ctx.stacks[alloc_id].last()
							} else {
								// Undef - reading uninitialized memory
								alloc_v := m.values[alloc_id]
								alloc_typ := m.type_store.types[alloc_v.typ]
								typ := alloc_typ.elem_type
								val = m.get_or_add_const(typ, 'undef')
							}
							bvid := block_val_ids[blk_id]
							// Defer phi operand append to avoid m.instrs[idx].operands << x
							// which is broken in ARM64 self-hosted binaries.
							ctx.deferred_phi_ops << v.index
							ctx.deferred_phi_ops << val
							ctx.deferred_phi_ops << bvid
							// FIX: Update uses so DCE doesn't remove the value
							// Avoid m.values[X].uses << Y — chained append broken in ARM64 self-hosted.
							if val < m.values.len {
								if vid !in m.values[val].uses {
									// Read whole struct, modify, write back (chained broken in ARM64)
									mut vv := m.values[val]
									vv.uses << vid
									m.values[val] = vv
								}
							}
						}
					}
				}
			}
		}

		// 4. Push next unvisited dom child
		n_children := dom.dom_tree[blk_id].len
		child_idx := work[fi].child_idx
		if child_idx < n_children {
			child := dom.dom_tree[blk_id][child_idx]
			// Avoid work[fi].child_idx++ — chained field increment broken in ARM64 self-hosted.
			mut frame := work[fi]
			frame.child_idx++
			work[fi] = frame
			work << RenameFrame{
				blk_id: child
			}
		} else {
			// 5. All children processed - pop stacks (cleanup)
			pushed := work[fi].pushed_allocs.clone()
			work.pop()
			for i := pushed.len - 1; i >= 0; i-- {
				// Avoid ctx.stacks[X].pop() — chained method broken in ARM64 self-hosted.
				mut s := ctx.stacks[pushed[i]].clone()
				s.pop()
				ctx.stacks[pushed[i]] = s
			}
		}
	}
	return blocks_visited, blocks_skipped
}

fn rename_recursive(mut m ssa.Module, blk_id int, mut ctx Mem2RegCtx, promotable []int, mut stack_counts []int, dom &DomInfo, cfg &CfgData) (int, int) {
	return rename_iterative(mut m, blk_id, mut ctx, promotable, mut stack_counts, dom,
		cfg)
}
