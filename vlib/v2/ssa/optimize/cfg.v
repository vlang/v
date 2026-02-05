// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// --- CFG Construction ---
pub fn build_cfg(mut m ssa.Module) {
	// Use a set for deduplication instead of linear search
	mut seen_succs := map[int]bool{}
	mut seen_preds := map[int]bool{}

	for func in m.funcs {
		// Clear existing preds/succs
		for blk_id in func.blocks {
			m.blocks[blk_id].preds = []
			m.blocks[blk_id].succs = []
		}

		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			if blk.instrs.len == 0 {
				continue
			}
			term_val_id := blk.instrs.last()
			term := m.instrs[m.values[term_val_id].index]

			// Clear the set for reuse
			seen_succs.clear()

			match term.op {
				.br {
					s1 := m.get_block_from_val(term.operands[1])
					s2 := m.get_block_from_val(term.operands[2])
					if !seen_succs[s1] {
						seen_succs[s1] = true
						m.blocks[blk_id].succs << s1
					}
					if !seen_succs[s2] {
						seen_succs[s2] = true
						m.blocks[blk_id].succs << s2
					}
				}
				.jmp {
					s := m.get_block_from_val(term.operands[0])
					seen_succs[s] = true
					m.blocks[blk_id].succs << s
				}
				.switch_ {
					// default
					s := m.get_block_from_val(term.operands[1])
					if !seen_succs[s] {
						seen_succs[s] = true
						m.blocks[blk_id].succs << s
					}
					// cases
					for i := 3; i < term.operands.len; i += 2 {
						cs := m.get_block_from_val(term.operands[i])
						if !seen_succs[cs] {
							seen_succs[cs] = true
							m.blocks[blk_id].succs << cs
						}
					}
				}
				else {}
			}

			// Build predecessors - use seen_preds to check if already added
			for s in m.blocks[blk_id].succs {
				// Reset seen_preds for each successor check
				seen_preds.clear()
				for p in m.blocks[s].preds {
					seen_preds[p] = true
				}
				if !seen_preds[blk_id] {
					m.blocks[s].preds << blk_id
				}
			}
		}
	}
}
