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

			for oi, operand in term.operands {
				if !term.is_successor_operand(oi) {
					continue
				}
				succ_id := int(operand)
				if succ_id >= 0 && succ_id < n_blocks
					&& !arr_contains(m.blocks[blk_id].succs, succ_id) {
					mut b := m.blocks[blk_id]
					b.succs << succ_id
					m.blocks[blk_id] = b
				}
			}

			for succ_id in m.blocks[blk_id].succs {
				if succ_id >= 0 && succ_id < n_blocks
					&& !arr_contains(m.blocks[succ_id].preds, blk_id) {
					mut b := m.blocks[succ_id]
					b.preds << blk_id
					m.blocks[succ_id] = b
				}
			}
		}
	}
}
