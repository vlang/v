module optimize

import v3.ssa

// arr_contains reports whether arr contains applies in optimize.
fn arr_contains(arr []int, val int) bool {
	for v in arr {
		if v == val {
			return true
		}
	}
	return false
}

// build_cfg builds cfg data for optimize.
fn build_cfg(mut m ssa.Module) {
	n_blocks := m.blocks.len
	n_values := m.values.len

	for bi in 0 .. n_blocks {
		mut blk := m.blocks[bi]
		blk.succs = []
		blk.preds = []
		m.blocks[bi] = blk
	}

	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			blk := m.blocks[blk_id]
			if blk.instrs.len == 0 {
				continue
			}
			term_val_id := blk.instrs.last()
			if term_val_id < 0 || term_val_id >= n_values {
				continue
			}
			term_val := m.values[term_val_id]
			if term_val.kind != .instruction {
				continue
			}
			term := m.instrs[term_val.index]

			match term.op {
				.br {
					if term.operands.len >= 3 {
						for oi in 1 .. 3 {
							s := int(term.operands[oi])
							if s >= 0 && s < n_blocks && !arr_contains(m.blocks[blk_id].succs, s) {
								mut b := m.blocks[blk_id]
								b.succs << s
								m.blocks[blk_id] = b
							}
						}
					}
				}
				.jmp {
					if term.operands.len >= 1 {
						s := int(term.operands[0])
						if s >= 0 && s < n_blocks {
							mut b := m.blocks[blk_id]
							b.succs << s
							m.blocks[blk_id] = b
						}
					}
				}
				.switch_ {
					// switch_ cond, default_blk, [case_val, blk]...
					if term.operands.len >= 2 {
						s := int(term.operands[1])
						if s >= 0 && s < n_blocks && !arr_contains(m.blocks[blk_id].succs, s) {
							mut b := m.blocks[blk_id]
							b.succs << s
							m.blocks[blk_id] = b
						}
						for oi := 3; oi < term.operands.len; oi += 2 {
							cs := int(term.operands[oi])
							if cs >= 0 && cs < n_blocks && !arr_contains(m.blocks[blk_id].succs, cs) {
								mut b := m.blocks[blk_id]
								b.succs << cs
								m.blocks[blk_id] = b
							}
						}
					}
				}
				else {}
			}

			for s in m.blocks[blk_id].succs {
				if s >= 0 && s < n_blocks && !arr_contains(m.blocks[s].preds, blk_id) {
					mut b := m.blocks[s]
					b.preds << blk_id
					m.blocks[s] = b
				}
			}
		}
	}
}
