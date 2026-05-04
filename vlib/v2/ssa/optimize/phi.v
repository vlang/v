// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// --- Simplify Phi Nodes ---
// Remove trivial phi nodes where all operands are the same or self-referential.
// A phi is trivial if all its non-self operands resolve to the same value.
// This reduces unnecessary instructions before phi elimination.
fn simplify_phi_nodes(mut m ssa.Module) bool {
	mut any_changed := false
	mut changed := true
	for changed {
		changed = false
		for fi in 0 .. m.funcs.len {
			func := m.funcs[fi]
			n_func_blocks := func.blocks.len
			for fbi in 0 .. n_func_blocks {
				blk_id := func.blocks[fbi]
				blk := m.blocks[blk_id]
				n_blk_instrs := blk.instrs.len
				for ii in 0 .. n_blk_instrs {
					val_id := blk.instrs[ii]
					val := m.values[val_id]
					if val.kind != .instruction {
						continue
					}
					instr := m.instrs[val.index]
					if instr.op != .phi {
						continue
					}

					// Dead phi removal: phi with no uses can be removed
					if val.uses.len == 0 {
						// Remove uses from this phi's operands
						for i := 0; i < instr.operands.len; i += 2 {
							op_val := instr.operands[i]
							if op_val != val_id { // Don't try to remove self-reference
								remove_phi_use(mut m, op_val, val_id)
							}
						}
						// Mark phi as dead
						// Avoid m.instrs[X].field = ... -- chained field assign broken in ARM64 self-hosted
						mut dead_phi := m.instrs[val.index]
						dead_phi.op = .bitcast
						dead_phi.operands = []
						m.instrs[val.index] = dead_phi
						changed = true
						any_changed = true
						continue
					}

					// Check if phi is trivial (all non-self operands are the same)
					mut unique_val := -1
					mut is_trivial := true

					for i := 0; i < instr.operands.len; i += 2 {
						op_val := instr.operands[i]
						// Skip self-references (phi refers to itself)
						if op_val == val_id {
							continue
						}
						if unique_val == -1 {
							unique_val = op_val
						} else if unique_val != op_val {
							is_trivial = false
							break
						}
					}

					// If trivial and we found a unique value, replace all uses
					if is_trivial && unique_val != -1 {
						// Replace all uses of this phi with the unique value
						m.replace_uses(val_id, unique_val)
						// Mark phi as dead (will be cleaned up by DCE or ignored)
						// Avoid m.instrs[X].field = ... -- chained field assign broken in ARM64 self-hosted
						mut triv_phi := m.instrs[val.index]
						triv_phi.op = .bitcast
						triv_phi.operands = []
						m.instrs[val.index] = triv_phi
						changed = true
						any_changed = true
					}
				}
			}
		}
	}
	return any_changed
}

// Helper to remove a use from a value's uses list (for dead phi removal)
fn remove_phi_use(mut m ssa.Module, val_id int, user_id int) {
	if val_id >= m.values.len {
		return
	}
	mut val := &m.values[val_id]
	for i := val.uses.len - 1; i >= 0; i-- {
		if val.uses[i] == user_id {
			val.uses.delete(i)
		}
	}
}

// get_block_val_id returns the value ID for a block, using a pre-built lookup table.
// This avoids m.blocks[bid].val_id which can return wrong values for the large
// BasicBlock struct (168 bytes) in ARM64-compiled binaries.
fn get_block_val_id(m &ssa.Module, bid int, block_val_ids []int) int {
	if bid >= 0 && bid < block_val_ids.len {
		return block_val_ids[bid]
	}
	return 0
}

// build_block_val_ids scans values for basic_block kind and builds block_id → val_id mapping.
fn build_block_val_ids(m &ssa.Module) []int {
	n_blks := m.blocks.len
	mut block_val_ids := []int{len: n_blks}
	n_vals := m.values.len
	for vi in 0 .. n_vals {
		if m.values[vi].kind == .basic_block {
			bid := m.values[vi].index
			if bid >= 0 && bid < n_blks {
				block_val_ids[bid] = vi
			}
		}
	}
	return block_val_ids
}

// --- Critical Edge Splitting ---
fn split_critical_edges(mut m ssa.Module) {
	mut cfg := build_cfg(mut m)

	for fi in 0 .. m.funcs.len {
		mut new_blocks := []ssa.BlockID{}

		// Collect edges to split (can't modify while iterating)
		mut edges_to_split := [][]ssa.BlockID{} // [pred_id, succ_id]

		// Find all critical edges
		func := m.funcs[fi]
		func_blocks_len := func.blocks.len
		for bi in 0 .. func_blocks_len {
			blk_id := func.blocks[bi]
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			n_blk_succs := cfg.succs[blk_id].len
			if n_blk_succs > 1 {
				for si in 0 .. n_blk_succs {
					succ_id := cfg.succs[blk_id][si]
					if succ_id < 0 || succ_id >= m.blocks.len {
						continue
					}
					if cfg.preds[succ_id].len > 1 {
						edges_to_split << [blk_id, succ_id]
					}
				}
			}
		}

		// Split each critical edge
		n_edges := edges_to_split.len
		for ei in 0 .. n_edges {
			pred_id := edges_to_split[ei][0]
			succ_id := edges_to_split[ei][1]

			if pred_id < 0 || pred_id >= m.blocks.len || succ_id < 0 || succ_id >= m.blocks.len {
				continue
			}

			// Create new intermediate block
			func2 := m.funcs[fi]
			split_blk := m.add_block(func2.id, 'split_${pred_id}_${succ_id}')
			new_blocks << split_blk

			// Re-build block_val_ids after adding the split block
			// (new blocks get new val_ids that need to be in the table)
			block_val_ids := build_block_val_ids(m)

			succ_val := get_block_val_id(m, succ_id, block_val_ids)
			// Add unconditional jump from split block to original successor
			m.add_instr(.jmp, split_blk, 0, [succ_val])

			// Update predecessor's terminator to jump to split block instead of successor
			pred_blk := m.blocks[pred_id]
			if pred_blk.instrs.len > 0 {
				term_val_id := pred_blk.instrs[pred_blk.instrs.len - 1]
				if term_val_id >= 0 && term_val_id < m.values.len {
					term_val := m.values[term_val_id]
					mut term := &m.instrs[term_val.index]

					old_succ_val := succ_val
					new_succ_val := get_block_val_id(m, split_blk, block_val_ids)

					// Replace ALL occurrences (handles switch with duplicate targets)
					for i in 0 .. term.operands.len {
						if term.operands[i] == old_succ_val {
							term.operands[i] = new_succ_val
						}
					}
				}
			}

			// Update phi nodes in successor to reference split block instead of pred
			succ_blk2 := m.blocks[succ_id]
			n_succ_instrs := succ_blk2.instrs.len
			for vi in 0 .. n_succ_instrs {
				val_id := succ_blk2.instrs[vi]
				if val_id < 0 || val_id >= m.values.len {
					continue
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				idx := val.index
				if idx < 0 || idx >= m.instrs.len {
					continue
				}
				mut instr := &m.instrs[idx]
				if instr.op == .phi {
					old_pred_val := get_block_val_id(m, pred_id, block_val_ids)
					new_pred_val := get_block_val_id(m, split_blk, block_val_ids)
					// Replace all occurrences (defensive - handles edge cases)
					for i := 1; i < instr.operands.len; i += 2 {
						if instr.operands[i] == old_pred_val {
							instr.operands[i] = new_pred_val
						}
					}
				}
			}
		}
	}

	// Rebuild CFG after splitting
	cfg = build_cfg(mut m)
	_ = cfg
}

fn eliminate_phi_nodes(mut m ssa.Module) {
	// First split critical edges to ensure correct copy placement
	split_critical_edges(mut m)

	n_blocks := m.blocks.len
	n_funcs := m.funcs.len

	// Build reverse map: val_id → block_id for block-kind values.
	// Scan values array for basic_block kind instead of using m.blocks[bid].val_id
	// which returns wrong results in ARM64-compiled binaries (large struct field access bug).
	mut val_to_block := []int{len: m.values.len, init: -1}
	n_vals := m.values.len
	for vi in 0 .. n_vals {
		if m.values[vi].kind == .basic_block {
			bid := m.values[vi].index
			if bid >= 0 && bid < n_blocks {
				val_to_block[vi] = bid
			}
		}
	}

	// Use flat parallel arrays indexed by block_id instead of [][]ParallelCopy.
	// pred_copy_dests[blk_id] and pred_copy_srcs[blk_id] store the dest/src pairs.
	mut pred_copy_dests := [][]int{len: n_blocks}
	mut pred_copy_srcs := [][]int{len: n_blocks}
	mut pred_copy_blocks := []int{}

	// Pre-allocate shared src_ref_count array for resolve_parallel_copies_flat.
	// Sized to m.values.len + headroom for temps. Reused across all calls.
	mut src_ref_count := []int{len: m.values.len + 1024}
	mut touched_ids := []int{cap: 256}

	for fi in 0 .. n_funcs {
		func := m.funcs[fi]
		func_blocks_len2 := func.blocks.len
		for bi2 in 0 .. func_blocks_len2 {
			blk_id := func.blocks[bi2]
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			blk := m.blocks[blk_id]
			n_instrs := blk.instrs.len
			for ii in 0 .. n_instrs {
				val_id := blk.instrs[ii]
				if val_id < 0 || val_id >= m.values.len {
					continue
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				idx := val.index
				if idx < 0 || idx >= m.instrs.len {
					continue
				}
				instr := m.instrs[idx]
				if instr.op == .phi {
					is_do_loop := func.name == 'do_loop'
					if is_do_loop {
						eprintln('  do_loop phi: val=${val_id} blk=${blk_id} nops=${instr.operands.len}')
					}
					// Phi operands: [val0, blk0, val1, blk1, ...]
					for i := 0; i < instr.operands.len; i += 2 {
						if i + 1 >= instr.operands.len {
							break
						}
						val_in := instr.operands[i]
						blk_val := instr.operands[i + 1]
						if blk_val < 0 || blk_val >= m.values.len {
							if is_do_loop {
								eprintln('    op[${i}]: val_in=${val_in} blk_val=${blk_val} SKIP(oob)')
							}
							continue
						}
						// Use val_to_block reverse map instead of blk_v.index
						pred_blk_idx := val_to_block[blk_val]
						if is_do_loop {
							blk_kind := m.values[blk_val].kind
							blk_index := m.values[blk_val].index
							eprintln('    op[${i}]: val_in=${val_in} blk_val=${blk_val} kind=${blk_kind} index=${blk_index} pred_blk_idx=${pred_blk_idx}')
						}

						if pred_blk_idx >= 0 && pred_blk_idx < n_blocks {
							if pred_copy_dests[pred_blk_idx].len == 0 {
								pred_copy_blocks << pred_blk_idx
							}
							mut pcd := pred_copy_dests[pred_blk_idx]
							pcd << val_id
							pred_copy_dests[pred_blk_idx] = pcd
							mut pcs := pred_copy_srcs[pred_blk_idx]
							pcs << val_in
							pred_copy_srcs[pred_blk_idx] = pcs
						}
					}
				}
			}
		}

		// For each predecessor that has copies, resolve with Briggs algorithm
		for pki in 0 .. pred_copy_blocks.len {
			pred_blk := pred_copy_blocks[pki]
			resolve_parallel_copies_flat(mut m, pred_blk, pred_copy_dests[pred_blk],
				pred_copy_srcs[pred_blk], mut src_ref_count, mut touched_ids)
		}

		// Cleanup: clear pred_copies for blocks we touched
		for pki2 in 0 .. pred_copy_blocks.len {
			pred_copy_dests[pred_copy_blocks[pki2]] = []
			pred_copy_srcs[pred_copy_blocks[pki2]] = []
		}
		pred_copy_blocks.clear()

		// Remove phi instructions (mark as nop/bitcast with no operands)
		func2 := m.funcs[fi]
		func_blocks_len3 := func2.blocks.len
		for bi3 in 0 .. func_blocks_len3 {
			blk_id := func2.blocks[bi3]
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			blk := m.blocks[blk_id]
			n_instrs3 := blk.instrs.len
			for ii3 in 0 .. n_instrs3 {
				val_id := blk.instrs[ii3]
				if val_id < 0 || val_id >= m.values.len {
					continue
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				idx := val.index
				if idx < 0 || idx >= m.instrs.len {
					continue
				}
				if m.instrs[idx].op == .phi {
					// Avoid m.instrs[X].field = ... -- chained field assign broken in ARM64 self-hosted
					mut cleanup_instr := m.instrs[idx]
					cleanup_instr.op = .bitcast
					cleanup_instr.operands = []
					m.instrs[idx] = cleanup_instr
				}
			}
		}
	}
}

// resolve_parallel_copies_flat sequences a set of parallel copies (dest[i] = src[i])
// using the Briggs algorithm with worklist-based scheduling.
// A copy dest←src is "ready" when no other pending copy reads from dest.
// Uses caller-provided flat array src_ref_count[] indexed by value ID (no maps).
// touched_ids tracks which entries were modified so we can zero only those on cleanup.
fn resolve_parallel_copies_flat(mut m ssa.Module, blk_id int, dests []int, srcs []int, mut src_ref_count []int, mut touched_ids []int) {
	if dests.len == 0 {
		return
	}

	n := dests.len

	// Build pending copies, filtering self-copies
	mut p_dest := []int{cap: n}
	mut p_src := []int{cap: n}
	for ci in 0 .. n {
		if dests[ci] != srcs[ci] {
			p_dest << dests[ci]
			p_src << srcs[ci]
		}
	}

	np := p_dest.len
	if np == 0 {
		return
	}

	// Ensure src_ref_count is large enough for all value IDs we'll encounter.
	// Check current max needed against array length.
	mut need_len := m.values.len + np + 4
	for i in 0 .. np {
		if p_dest[i] >= need_len {
			need_len = p_dest[i] + np + 4
		}
		if p_src[i] >= need_len {
			need_len = p_src[i] + np + 4
		}
	}
	if need_len > src_ref_count.len {
		// Grow array
		for _ in 0 .. need_len - src_ref_count.len {
			src_ref_count << 0
		}
	}

	// src_ref_count[val] = how many pending copies use val as source
	// (array is pre-zeroed or cleaned from previous call via touched_ids)
	touched_ids.clear()
	for i in 0 .. np {
		s := p_src[i]
		if src_ref_count[s] == 0 {
			touched_ids << s
		}
		src_ref_count[s] = src_ref_count[s] + 1
	}
	// Also track dests we'll read from src_ref_count
	for i in 0 .. np {
		d := p_dest[i]
		if src_ref_count[d] == 0 {
			// dest not in touched_ids yet but we read it; track for safety
			// (it's already 0, but if cycle-breaking modifies it we need to clean it)
		}
	}

	// Use int instead of bool for alive - ARM64 codegen may mishandle []bool{init: true}
	// alive[i] = 1 if copy i is still pending, 0 if done
	mut alive := []int{len: np}
	for ai in 0 .. np {
		alive[ai] = 1
	}

	// Worklist: use manual stack pointer instead of .delete() which may be buggy in ARM64
	mut worklist := []int{len: np + np + 4}
	mut wl_top := 0 // stack pointer: worklist[0..wl_top] are valid entries
	for i in 0 .. np {
		d := p_dest[i]
		if src_ref_count[d] == 0 {
			worklist[wl_top] = i
			wl_top += 1
		}
	}

	// Sequenced output copies
	mut s_dest := []int{cap: np + 4}
	mut s_src := []int{cap: np + 4}

	mut remaining := np
	for remaining > 0 {
		if wl_top > 0 {
			// Pop a ready copy from the worklist
			wl_top -= 1
			idx := worklist[wl_top]
			if idx < 0 || idx >= np || alive[idx] == 0 {
				continue
			}
			alive[idx] = 0
			remaining -= 1

			d := p_dest[idx]
			s := p_src[idx]
			s_dest << d
			s_src << s

			// Decrement ref count for the source
			src_ref_count[s] = src_ref_count[s] - 1
			if src_ref_count[s] == 0 {
				// s is no longer used as source; any pending copy with dest==s is now ready
				for j in 0 .. np {
					if alive[j] == 1 && p_dest[j] == s {
						// Grow worklist if needed
						if wl_top >= worklist.len {
							worklist << 0
						}
						worklist[wl_top] = j
						wl_top += 1
					}
				}
			}
			continue
		}

		// Worklist empty but copies remain → cycle. Break it with a temp.
		mut ci := -1
		for k in 0 .. np {
			if alive[k] == 1 {
				ci = k
				break
			}
		}
		if ci < 0 {
			break
		}

		cycle_src := p_src[ci]
		mut typ := 0
		if cycle_src >= 0 && cycle_src < m.values.len {
			typ = m.values[cycle_src].typ
		}
		if typ == 0 && p_dest[ci] >= 0 && p_dest[ci] < m.values.len {
			typ = m.values[p_dest[ci]].typ
		}
		temp := insert_temp_in_block(mut m, blk_id, cycle_src, typ)

		// Grow src_ref_count if temp exceeds array bounds
		if temp >= src_ref_count.len {
			for _ in 0 .. temp + 1 - src_ref_count.len {
				src_ref_count << 0
			}
		}

		// Replace all references to cycle_src with temp in pending copies
		src_ref_count[temp] = 0
		touched_ids << temp
		for i in 0 .. np {
			if alive[i] == 1 && p_src[i] == cycle_src {
				p_src[i] = temp
				src_ref_count[temp] = src_ref_count[temp] + 1
			}
		}
		src_ref_count[cycle_src] = 0

		// Check if any copy whose dest was cycle_src is now ready
		for j in 0 .. np {
			if alive[j] == 1 && p_dest[j] == cycle_src {
				if wl_top >= worklist.len {
					worklist << 0
				}
				worklist[wl_top] = j
				wl_top += 1
			}
		}
	}

	// Clean up src_ref_count: zero only the entries we touched
	for ti in 0 .. touched_ids.len {
		tid := touched_ids[ti]
		if tid < src_ref_count.len {
			src_ref_count[tid] = 0
		}
	}

	// Emit the sequenced copies
	for si in 0 .. s_dest.len {
		insert_copy_in_block(mut m, blk_id, s_dest[si], s_src[si])
	}
}

fn insert_temp_in_block(mut m ssa.Module, blk_id int, src int, typ int) int {
	m.instrs << ssa.Instruction{
		op:       .bitcast
		block:    blk_id
		typ:      typ
		operands: [ssa.ValueID(src)]
	}
	temp_id := m.add_value_node(.instruction, typ, 'phi_tmp_${m.values.len}', m.instrs.len - 1)

	// Read whole struct, modify, write back (chained field assign broken in ARM64)
	if src < m.values.len && temp_id !in m.values[src].uses {
		mut sv := m.values[src]
		sv.uses << temp_id
		m.values[src] = sv
	}

	// Insert before terminator — build new instrs array
	mut blk := m.blocks[blk_id]
	mut insert_idx := blk.instrs.len
	if insert_idx > 0 {
		last_val_id := blk.instrs[blk.instrs.len - 1]
		last_val := m.values[last_val_id]
		last_instr := m.instrs[last_val.index]
		if last_instr.op in [.ret, .br, .jmp, .switch_, .unreachable] {
			insert_idx = blk.instrs.len - 1
		}
	}
	mut new_instrs := []int{cap: blk.instrs.len + 1}
	for ii in 0 .. insert_idx {
		new_instrs << blk.instrs[ii]
	}
	new_instrs << temp_id
	for ii in insert_idx .. blk.instrs.len {
		new_instrs << blk.instrs[ii]
	}
	blk.instrs = new_instrs
	m.blocks[blk_id] = blk
	return temp_id
}

fn insert_copy_in_block(mut m ssa.Module, blk_id int, dest int, src int) {
	dest_val := m.values[dest]
	typ := dest_val.typ
	m.instrs << ssa.Instruction{
		op:       .assign
		block:    blk_id
		typ:      typ
		operands: [ssa.ValueID(dest), src]
	}
	val_id := m.add_value_node(.instruction, typ, 'copy', m.instrs.len - 1)
	// Read whole struct, modify, write back (chained field assign broken in ARM64)
	if dest < m.values.len && val_id !in m.values[dest].uses {
		mut dv := m.values[dest]
		dv.uses << val_id
		m.values[dest] = dv
	}
	if src < m.values.len && val_id !in m.values[src].uses {
		mut sv := m.values[src]
		sv.uses << val_id
		m.values[src] = sv
	}

	// Insert before terminator — build new instrs array
	mut blk := m.blocks[blk_id]
	mut insert_idx := blk.instrs.len
	if insert_idx > 0 {
		last_val_id := blk.instrs[blk.instrs.len - 1]
		last_val := m.values[last_val_id]
		last_instr := m.instrs[last_val.index]
		if last_instr.op in [.ret, .br, .jmp, .switch_, .unreachable] {
			insert_idx = blk.instrs.len - 1
		}
	}
	mut new_instrs := []int{cap: blk.instrs.len + 1}
	for ii in 0 .. insert_idx {
		new_instrs << blk.instrs[ii]
	}
	new_instrs << val_id
	for ii in insert_idx .. blk.instrs.len {
		new_instrs << blk.instrs[ii]
	}
	blk.instrs = new_instrs
	m.blocks[blk_id] = blk
}
