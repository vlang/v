// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

fn remove_unreachable_blocks(mut m ssa.Module) {
	// Re-build CFG first
	build_cfg(mut m)
	for fi in 0 .. m.funcs.len {
		if m.funcs[fi].blocks.len == 0 {
			continue
		}
		// BFS/DFS from entry
		mut reachable := map[int]bool{}
		mut q := [m.funcs[fi].blocks[0]]
		reachable[m.funcs[fi].blocks[0]] = true

		for q.len > 0 {
			curr := q[q.len - 1]
			q.delete_last()
			for succ in m.blocks[curr].succs {
				if !reachable[succ] {
					reachable[succ] = true
					q << succ
				}
			}
		}

		mut new_blocks := []int{}
		for blk in m.funcs[fi].blocks {
			if reachable[blk] {
				new_blocks << blk
			}
		}
		// Avoid m.funcs[fi].blocks = ... -- chained field assign broken in ARM64 self-hosted
		mut func := m.funcs[fi]
		func.blocks = new_blocks
		m.funcs[fi] = func
	}
}

fn merge_blocks(mut m ssa.Module) {
	// If Block A jumps unconditionally to B, and B has only A as predecessor:
	// 1. Move instructions from B to A
	// 2. Update A's successors to B's successors
	// 3. Remove B

	// We need to be careful about iteration while modifying.
	// Loop until no changes.
	mut changed := true
	mut first_iter := true
	for changed {
		changed = false
		// Only rebuild CFG on first iteration or after actual changes
		if first_iter {
			build_cfg(mut m)
			first_iter = false
		}

		for fi in 0 .. m.funcs.len {
			// We iterate through blocks.
			// If we merge A->B, we can't merge B->C in same pass easily.
			mut merged := map[int]bool{}

			for blk_id in m.funcs[fi].blocks {
				if merged[blk_id] {
					continue
				}

				// Check if unconditional jump
				if m.blocks[blk_id].instrs.len > 0 {
					last_val := m.blocks[blk_id].instrs[m.blocks[blk_id].instrs.len - 1]
					last_instr := m.instrs[m.values[last_val].index]

					if last_instr.op == .jmp {
						target_val := last_instr.operands[0]
						target_id := m.get_block_from_val(target_val)

						// Check if target has phi nodes - don't merge if it does
						mut has_phi := false
						for vid in m.blocks[target_id].instrs {
							if m.values[vid].kind == .instruction {
								if m.instrs[m.values[vid].index].op == .phi {
									has_phi = true
									break
								}
							}
						}

						// Candidate: target_id (only if no phi nodes)
						if target_id != blk_id && m.blocks[target_id].preds.len == 1
							&& m.blocks[target_id].preds[0] == blk_id && !has_phi {
							// MERGE: Remove JMP from A, then append B's instrs to A
							// Read whole struct, modify, write back (chained broken in ARM64)
							mut merge_blk := m.blocks[blk_id]
							merge_blk.instrs.delete_last()
							merge_blk.instrs << m.blocks[target_id].instrs
							m.blocks[blk_id] = merge_blk

							// Update instructions in B to point to A (for their 'block' field)?
							// Not strictly needed if we just use the list.
							// But we need to update Phis in successors of B?
							// If B has successors, their Phis might refer to B.
							// Since B is gone, they now refer to A.
							for succ_id in m.blocks[target_id].succs {
								n_succ_instrs := m.blocks[succ_id].instrs.len
								for ivi in 0 .. n_succ_instrs {
									iv := m.blocks[succ_id].instrs[ivi]
									v := m.values[iv]
									if v.kind != .instruction {
										continue
									}
									ins := m.instrs[v.index]
									if ins.op == .phi {
										// Replace all occurrences (defensive - handles edge cases)
										// i=1,3,5... are block references in phi [val0, blk0, val1, blk1, ...]
										// Avoid m.instrs[X].operands[i] = ... -- chained broken in ARM64 self-hosted
										mut phi_ops := ins.operands.clone()
										mut phi_modified := false
										for i := 1; i < phi_ops.len; i += 2 {
											if phi_ops[i] == m.blocks[target_id].val_id {
												phi_ops[i] = m.blocks[blk_id].val_id
												phi_modified = true
											}
										}
										if phi_modified {
											mut phi_ins := m.instrs[v.index]
											phi_ins.operands = phi_ops
											m.instrs[v.index] = phi_ins
										}
									}
								}
							}

							// Remove B from func
							merged[target_id] = true
							changed = true
						}
					}
				}
			}

			// Filter out merged blocks
			if merged.len > 0 {
				mut new_blks := []int{}
				for b in m.funcs[fi].blocks {
					if !merged[b] {
						new_blks << b
					}
				}
				// Avoid m.funcs[fi].blocks = ... -- chained field assign broken in ARM64 self-hosted
				mut func2 := m.funcs[fi]
				func2.blocks = new_blks
				m.funcs[fi] = func2
			}
		}
		// Rebuild CFG for next iteration if we made changes
		if changed {
			build_cfg(mut m)
		}
	}
}
