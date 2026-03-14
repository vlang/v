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

	// Build block_val_ids ONCE before all function loops.
	// Cannot use m.blocks[bid].val_id directly — in ARM64-compiled binaries,
	// BasicBlock is 168 bytes and array[i] returns a copy with potentially
	// wrong field values for large structs.
	mut block_val_ids := build_block_val_ids(m)

	for fi in 0 .. m.funcs.len {
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

			// Extend block_val_ids for newly added block(s)
			for block_val_ids.len <= split_blk {
				block_val_ids << 0
			}
			// Find val_id for the new split block by scanning recently added values
			for bvi := m.values.len - 1; bvi >= 0; bvi-- {
				if m.values[bvi].kind == .basic_block && m.values[bvi].index == split_blk {
					block_val_ids[split_blk] = bvi
					break
				}
			}

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

	// Pre-allocate shared arrays for resolve_parallel_copies_batched.
	// Sized to m.values.len + headroom for temps. Reused across all calls.
	headroom := m.values.len + 1024
	mut src_ref_count := []int{len: headroom}
	mut touched_ids := []int{cap: 256}
	// Pre-allocate dest_to_indices: dest_value → list of copy indices.
	// This was the #1 bottleneck — allocating [][]int{len: 260K+} per call.
	mut dest_to_indices := [][]int{len: headroom}
	mut dest_touched := []int{cap: 256}

	// Collect all copies per block across all functions, then batch-insert.
	// pending_block_copies_dests/srcs accumulate the sequenced copies from Briggs.
	mut pending_block_copies_dests := [][]int{len: n_blocks}
	mut pending_block_copies_srcs := [][]int{len: n_blocks}
	mut pending_blocks := []int{cap: 256}

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
					// Phi operands: [val0, blk0, val1, blk1, ...]
					for i := 0; i < instr.operands.len; i += 2 {
						if i + 1 >= instr.operands.len {
							break
						}
						val_in := instr.operands[i]
						blk_val := instr.operands[i + 1]
						if blk_val < 0 || blk_val >= m.values.len {
							continue
						}
						// Use val_to_block reverse map instead of blk_v.index
						pred_blk_idx := val_to_block[blk_val]

						if pred_blk_idx >= 0 && pred_blk_idx < n_blocks {
							if pred_copy_dests[pred_blk_idx].len == 0 {
								pred_copy_blocks << pred_blk_idx
							}
							pred_copy_dests[pred_blk_idx] << val_id
							pred_copy_srcs[pred_blk_idx] << val_in
						}
					}
				}
			}
		}

		// For each predecessor that has copies, resolve with Briggs algorithm
		// and collect sequenced copies for batch insertion
		for pki in 0 .. pred_copy_blocks.len {
			pred_blk := pred_copy_blocks[pki]
			resolve_parallel_copies_batched(mut m, pred_blk,
				pred_copy_dests[pred_blk], pred_copy_srcs[pred_blk],
				mut src_ref_count, mut touched_ids,
				mut dest_to_indices, mut dest_touched,
				mut pending_block_copies_dests,
				mut pending_block_copies_srcs, mut pending_blocks)
		}

		// Batch-insert all collected copies into their blocks (one rebuild per block)
		for pbi in 0 .. pending_blocks.len {
			bid := pending_blocks[pbi]
			batch_insert_copies_in_block(mut m, bid,
				pending_block_copies_dests[bid], pending_block_copies_srcs[bid])
			pending_block_copies_dests[bid] = []
			pending_block_copies_srcs[bid] = []
		}
		pending_blocks.clear()

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

// resolve_parallel_copies_batched sequences a set of parallel copies (dest[i] = src[i])
// using the Briggs algorithm, and collects the results into pending_block arrays
// for batch insertion (instead of inserting one-by-one).
fn resolve_parallel_copies_batched(mut m ssa.Module, blk_id int, dests []int, srcs []int, mut src_ref_count []int, mut touched_ids []int, mut dest_to_indices [][]int, mut dest_touched []int, mut pending_dests [][]int, mut pending_srcs [][]int, mut pending_blocks []int) {
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
		for _ in 0 .. need_len - src_ref_count.len {
			src_ref_count << 0
		}
	}

	// src_ref_count[val] = how many pending copies use val as source
	touched_ids.clear()
	for i in 0 .. np {
		s := p_src[i]
		if src_ref_count[s] == 0 {
			touched_ids << s
		}
		src_ref_count[s] = src_ref_count[s] + 1
	}

	// Build reverse index: dest_value → list of copy indices for O(1) lookup
	// when a source becomes available as a destination
	// dest_to_indices and dest_touched are pre-allocated and passed in.
	if need_len > dest_to_indices.len {
		for _ in 0 .. need_len - dest_to_indices.len {
			dest_to_indices << []int{}
		}
	}
	dest_touched.clear()
	for i in 0 .. np {
		d := p_dest[i]
		if dest_to_indices[d].len == 0 {
			dest_touched << d
		}
		// Avoid dest_to_indices[d] << i — chained array-element append
		// returns a copy in ARM64-compiled binaries (len not updated in original).
		mut dti := dest_to_indices[d]
		dti << i
		dest_to_indices[d] = dti
	}

	// alive[i] = 1 if copy i is still pending, 0 if done
	mut alive := []int{len: np}
	for ai in 0 .. np {
		alive[ai] = 1
	}

	// Worklist: use manual stack pointer
	mut worklist := []int{len: np + np + 4}
	mut wl_top := 0
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
				// Use dest_to_indices for O(1) lookup instead of scanning all copies
				if s < dest_to_indices.len {
					for ji in 0 .. dest_to_indices[s].len {
						j := dest_to_indices[s][ji]
						if alive[j] == 1 {
							if wl_top >= worklist.len {
								worklist << 0
							}
							worklist[wl_top] = j
							wl_top += 1
						}
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
		if cycle_src < dest_to_indices.len {
			for ji in 0 .. dest_to_indices[cycle_src].len {
				j := dest_to_indices[cycle_src][ji]
				if alive[j] == 1 {
					if wl_top >= worklist.len {
						worklist << 0
					}
					worklist[wl_top] = j
					wl_top += 1
				}
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
	// Clean up dest_to_indices
	for dti in 0 .. dest_touched.len {
		dest_to_indices[dest_touched[dti]] = []
	}

	// Collect sequenced copies for batch insertion
	// Avoid pending_dests[blk_id] << x — chained array-element append
	// returns a copy in ARM64-compiled binaries (len not updated in original).
	if s_dest.len > 0 {
		if pending_dests[blk_id].len == 0 {
			pending_blocks << blk_id
		}
		mut pd := pending_dests[blk_id]
		mut ps := pending_srcs[blk_id]
		for si in 0 .. s_dest.len {
			pd << s_dest[si]
			ps << s_src[si]
		}
		pending_dests[blk_id] = pd
		pending_srcs[blk_id] = ps
	}
}

// batch_insert_copies_in_block creates all copy instructions for a block and
// inserts them before the terminator in a single array rebuild.
fn batch_insert_copies_in_block(mut m ssa.Module, blk_id int, dests []int, srcs []int) {
	if dests.len == 0 {
		return
	}

	// Create all copy instructions and value nodes first.
	// Skip use-list updates: copy assigns write to dest's slot (no own slot needed),
	// and are always processed in codegen pass 2 (gen_instr iterates all instrs).
	// The dead-code skip in pass 1 correctly skips these (uses.len == 0) since
	// copies don't need their own stack slot.
	mut copy_val_ids := []int{cap: dests.len}
	for ci in 0 .. dests.len {
		dest := dests[ci]
		src := srcs[ci]
		dest_val := m.values[dest]
		typ := dest_val.typ
		m.instrs << ssa.Instruction{
			op:       .assign
			block:    blk_id
			typ:      typ
			operands: [ssa.ValueID(dest), src]
		}
		val_id := m.add_value_node(.instruction, typ, 'copy', m.instrs.len - 1)
		copy_val_ids << val_id
	}

	// Single array rebuild: insert all copies before the terminator
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
	n_copies := copy_val_ids.len
	mut new_instrs := []int{cap: blk.instrs.len + n_copies}
	for ii in 0 .. insert_idx {
		new_instrs << blk.instrs[ii]
	}
	for ci in 0 .. n_copies {
		new_instrs << copy_val_ids[ci]
	}
	for ii in insert_idx .. blk.instrs.len {
		new_instrs << blk.instrs[ii]
	}
	blk.instrs = new_instrs
	m.blocks[blk_id] = blk
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
