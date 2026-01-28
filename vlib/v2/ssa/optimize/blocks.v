// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

fn remove_unreachable_blocks(mut m ssa.Module) {
	// Re-build CFG first
	build_cfg(mut m)
	for mut func in m.funcs {
		if func.blocks.len == 0 {
			continue
		}
		// BFS/DFS from entry
		mut reachable := map[int]bool{}
		mut q := [func.blocks[0]]
		reachable[func.blocks[0]] = true

		for q.len > 0 {
			curr := q.pop()
			for succ in m.blocks[curr].succs {
				if !reachable[succ] {
					reachable[succ] = true
					q << succ
				}
			}
		}

		mut new_blocks := []int{}
		for blk in func.blocks {
			if reachable[blk] {
				new_blocks << blk
			}
		}
		func.blocks = new_blocks
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

		for mut func in m.funcs {
			// We iterate through blocks.
			// If we merge A->B, we can't merge B->C in same pass easily.
			mut merged := map[int]bool{}

			for blk_id in func.blocks {
				if merged[blk_id] {
					continue
				}
				blk := m.blocks[blk_id]

				// Check if unconditional jump
				if blk.instrs.len > 0 {
					last_val := blk.instrs.last()
					last_instr := m.instrs[m.values[last_val].index]

					if last_instr.op == .jmp {
						target_val := last_instr.operands[0]
						target_id := m.get_block_from_val(target_val)

						// Candidate: target_id
						if target_id != blk_id && m.blocks[target_id].preds.len == 1
							&& m.blocks[target_id].preds[0] == blk_id {
							// MERGE
							// Remove JMP from A
							m.blocks[blk_id].instrs.delete_last()

							// Append B's instrs to A
							m.blocks[blk_id].instrs << m.blocks[target_id].instrs

							// Update instructions in B to point to A (for their 'block' field)?
							// Not strictly needed if we just use the list.
							// But we need to update Phis in successors of B?
							// If B has successors, their Phis might refer to B.
							// Since B is gone, they now refer to A.
							for succ_id in m.blocks[target_id].succs {
								succ := m.blocks[succ_id]
								for iv in succ.instrs {
									v := m.values[iv]
									if v.kind != .instruction {
										continue
									}
									ins := m.instrs[v.index]
									if ins.op == .phi {
										// Replace all occurrences (defensive - handles edge cases)
										// i=1,3,5... are block references in phi [val0, blk0, val1, blk1, ...]
										for i := 1; i < ins.operands.len; i += 2 {
											if ins.operands[i] == m.blocks[target_id].val_id {
												m.instrs[v.index].operands[i] = m.blocks[blk_id].val_id
											}
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
				for b in func.blocks {
					if !merged[b] {
						new_blks << b
					}
				}
				func.blocks = new_blks
			}
		}
		// Rebuild CFG for next iteration if we made changes
		if changed {
			build_cfg(mut m)
		}
	}
}
