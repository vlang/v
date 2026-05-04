// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa
import time

// Set to true to enable verification after each optimization pass (for debugging)
const debug_verify = false

// Optimize Module
pub fn optimize(mut m ssa.Module) {
	mut t0 := time.ticks()

	// 1. Build Control Flow Graph (Predecessors)
	cfg := build_cfg(mut m)
	$if debug_verify {
		verify_and_panic(m, 'build_cfg')
	}
	mut t1 := time.ticks()
	eprintln('  opt: build_cfg: ${t1 - t0}ms')
	t0 = t1

	// 2. Compute Dominator Tree (Lengauer-Tarjan)
	dom := compute_dominators(mut m, &cfg)
	$if debug_verify {
		verify_and_panic(m, 'compute_dominators')
	}
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
	$if debug_verify {
		verify_and_panic(m, 'promote_memory_to_register')
	}
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
	t1 = time.ticks()
	eprintln('  opt: simplify_phi: ${t1 - t0}ms')
	t0 = t1

	// 5. Eliminate Phi Nodes (convert to copies in predecessor blocks)
	eliminate_phi_nodes(mut m)
	t1 = time.ticks()
	eprintln('  opt: eliminate_phi: ${t1 - t0}ms')
}
