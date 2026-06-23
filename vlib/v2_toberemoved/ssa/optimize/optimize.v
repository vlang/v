// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import os
import v2.ssa
import time

pub struct OptimizeOptions {
pub:
	verify_each_pass bool
	strict_verify    bool
}

// Optimize Module
pub fn optimize(mut m ssa.Module) {
	strict_verify := os.getenv('V2_VERIFY_STRICT') != ''
	optimize_with_options(mut m, OptimizeOptions{
		verify_each_pass: os.getenv('V2_VERIFY') != ''
		strict_verify:    strict_verify
	})
}

pub fn optimize_with_options(mut m ssa.Module, opts OptimizeOptions) {
	mut t0 := time.ticks()
	verify_pipeline_checkpoint(m, opts, 'input')
	rebuild_use_lists(mut m)

	// 1. Build Control Flow Graph (Predecessors)
	cfg := build_cfg(mut m)
	verify_pipeline_checkpoint(m, opts, 'build_cfg')
	mut t1 := time.ticks()
	eprintln('  opt: build_cfg: ${t1 - t0}ms')
	t0 = t1

	// 2. Compute Dominator Tree (Lengauer-Tarjan)
	dom := compute_dominators(mut m, &cfg)
	verify_pipeline_checkpoint(m, opts, 'compute_dominators')
	t1 = time.ticks()
	// Count dom tree stats
	mut n_with_idom := 0
	mut n_in_dom_tree := 0
	mut n_no_idom := 0
	mut n_idom_self := 0
	mut n_idom_diff := 0
	for bi in 0 .. dom.idom.len {
		if dom.idom[bi] >= 0 {
			n_with_idom += 1
			if dom.idom[bi] == bi {
				n_idom_self += 1
			} else {
				n_idom_diff += 1
			}
		} else {
			n_no_idom += 1
		}
		if bi < dom.dom_tree.len {
			n_in_dom_tree += dom.dom_tree[bi].len
		}
	}
	eprintln('  opt: compute_dominators: ${t1 - t0}ms (with_idom=${n_with_idom} no_idom=${n_no_idom} self=${n_idom_self} diff=${n_idom_diff} dom_tree=${n_in_dom_tree})')
	t0 = t1

	// 3. Promote Memory to Register (Construct SSA / Phi Nodes)
	promote_memory_to_register(mut m, dom, &cfg)
	verify_pipeline_checkpoint(m, opts, 'promote_memory_to_register')
	rebuild_use_lists(mut m)
	t1 = time.ticks()
	// Count remaining allocas
	mut n_alloca := 0
	for vi in 0 .. m.values.len {
		val := m.values[vi]
		if val.kind == .instruction {
			idx := val.index
			if idx >= 0 && idx < m.instrs.len {
				if m.instrs[idx].op == .alloca {
					n_alloca += 1
				}
			}
		}
	}
	eprintln('  opt: mem2reg: ${t1 - t0}ms (remaining allocas: ${n_alloca})')
	t0 = t1

	// 4. Simplify trivial Phi Nodes
	simplify_phi_nodes(mut m)
	verify_pipeline_checkpoint(m, opts, 'simplify_phi_nodes')
	rebuild_use_lists(mut m)
	t1 = time.ticks()
	eprintln('  opt: simplify_phi: ${t1 - t0}ms')
	t0 = t1

	// 5. Eliminate Phi Nodes (convert to copies in predecessor blocks)
	eliminate_phi_nodes(mut m)
	verify_pipeline_checkpoint(m, opts, 'eliminate_phi_nodes')
	rebuild_use_lists(mut m)
	t1 = time.ticks()
	eprintln('  opt: eliminate_phi: ${t1 - t0}ms')
	t0 = t1

	// 6. Remove dead instructions after structural SSA rewrites are complete.
	dead_code_elimination(mut m)
	verify_pipeline_checkpoint(m, opts, 'dead_code_elimination')
	t1 = time.ticks()
	eprintln('  opt: dce: ${t1 - t0}ms')
}

fn rebuild_use_lists(mut m ssa.Module) {
	for vi in 0 .. m.values.len {
		mut val := m.values[vi]
		val.uses = []
		m.values[vi] = val
	}
	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			for val_id in m.blocks[blk_id].instrs {
				if val_id <= 0 || val_id >= m.values.len || m.values[val_id].kind != .instruction {
					continue
				}
				instr_idx := m.values[val_id].index
				if instr_idx < 0 || instr_idx >= m.instrs.len {
					continue
				}
				for op_id in m.instrs[instr_idx].operands {
					if op_id >= 0 && op_id < m.values.len && val_id !in m.values[op_id].uses {
						mut op_val := m.values[op_id]
						op_val.uses << val_id
						m.values[op_id] = op_val
					}
				}
			}
		}
	}
}

fn verify_pipeline_checkpoint(m &ssa.Module, opts OptimizeOptions, pass_name string) {
	if opts.verify_each_pass || opts.strict_verify {
		verify_and_panic_with_options(m, pass_name, VerifyPanicOptions{
			allow_noncritical: !opts.strict_verify
		})
	}
}
