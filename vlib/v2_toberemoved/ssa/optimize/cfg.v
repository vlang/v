// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// --- CFG Construction ---
// Flat CFG data: succs and preds stored outside BasicBlock to avoid struct field access issues
// in v3 (ARM64-compiled binary). Accessing m.blocks[i].succs returns garbage for large structs.
struct CfgData {
mut:
	succs [][]int
	preds [][]int
}

// Helper: check if value is already in a small array (avoids map allocation)
fn arr_contains(arr []int, val int) bool {
	for i in 0 .. arr.len {
		if arr[i] == val {
			return true
		}
	}
	return false
}

pub fn build_cfg(mut m ssa.Module) CfgData {
	n_blocks := m.blocks.len
	n_values := m.values.len
	n_funcs := m.funcs.len
	n_instrs_total := m.instrs.len

	mut cfg := CfgData{
		succs: [][]int{len: n_blocks}
		preds: [][]int{len: n_blocks}
	}

	for fi in 0 .. n_funcs {
		func := m.funcs[fi]
		n_func_blocks := func.blocks.len

		for fbi2 in 0 .. n_func_blocks {
			blk_id := func.blocks[fbi2]
			blk := m.blocks[blk_id]
			n_instrs := blk.instrs.len
			if n_instrs == 0 {
				continue
			}
			term_val_id := blk.instrs[n_instrs - 1]
			if term_val_id < 0 || term_val_id >= n_values {
				continue
			}
			term_val := m.values[term_val_id]
			if term_val.index < 0 || term_val.index >= n_instrs_total {
				continue
			}
			term := m.instrs[term_val.index]

			n_operands := term.operands.len
			match term.op {
				.br {
					if n_operands >= 3 {
						op1 := term.operands[1]
						op2 := term.operands[2]
						mut s1 := -1
						if op1 >= 0 {
							if op1 < n_values {
								v1 := m.values[op1]
								s1 = v1.index
							}
						}
						mut s2 := -1
						if op2 >= 0 {
							if op2 < n_values {
								v2 := m.values[op2]
								s2 = v2.index
							}
						}
						if s1 >= 0 {
							if s1 < n_blocks {
								if !arr_contains(cfg.succs[blk_id], s1) {
									cfg.succs[blk_id] << s1
								}
							}
						}
						if s2 >= 0 {
							if s2 < n_blocks {
								if !arr_contains(cfg.succs[blk_id], s2) {
									cfg.succs[blk_id] << s2
								}
							}
						}
					}
				}
				.jmp {
					if n_operands >= 1 {
						op0 := term.operands[0]
						mut s := -1
						if op0 >= 0 {
							if op0 < n_values {
								v0 := m.values[op0]
								s = v0.index
							}
						}
						if s >= 0 {
							if s < n_blocks {
								cfg.succs[blk_id] << s
							}
						}
					}
				}
				.switch_ {
					if n_operands >= 2 {
						op_def := term.operands[1]
						mut s := -1
						if op_def >= 0 {
							if op_def < n_values {
								vd := m.values[op_def]
								s = vd.index
							}
						}
						if s >= 0 {
							if s < n_blocks {
								if !arr_contains(cfg.succs[blk_id], s) {
									cfg.succs[blk_id] << s
								}
							}
						}
						for i := 3; i < n_operands; i += 2 {
							op_case := term.operands[i]
							mut cs := -1
							if op_case >= 0 {
								if op_case < n_values {
									vc := m.values[op_case]
									cs = vc.index
								}
							}
							if cs >= 0 {
								if cs < n_blocks {
									if !arr_contains(cfg.succs[blk_id], cs) {
										cfg.succs[blk_id] << cs
									}
								}
							}
						}
					}
				}
				else {}
			}

			// Build predecessors from succs
			n_succs := cfg.succs[blk_id].len
			for si in 0 .. n_succs {
				s := cfg.succs[blk_id][si]
				if s < 0 || s >= n_blocks {
					continue
				}
				if !arr_contains(cfg.preds[s], blk_id) {
					cfg.preds[s] << blk_id
				}
			}
		}
	}

	// Also write back to m.blocks for compatibility with other passes
	for bi in 0 .. n_blocks {
		// Avoid m.blocks[X].succs/preds = ... -- chained field assign broken in ARM64 self-hosted
		mut blk := m.blocks[bi]
		blk.succs = cfg.succs[bi]
		blk.preds = cfg.preds[bi]
		m.blocks[bi] = blk
	}

	return cfg
}
