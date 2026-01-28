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
	t := time.now()
	// 1. Build Control Flow Graph (Predecessors)
	build_cfg(mut m)
	$if debug_verify {
		verify_and_panic(m, 'build_cfg')
	}

	// 2. Compute Dominator Tree (Lengauer-Tarjan)
	compute_dominators(mut m)
	$if debug_verify {
		verify_and_panic(m, 'compute_dominators')
	}

	// 3. Promote Memory to Register (Construct SSA / Phi Nodes)
	promote_memory_to_register(mut m)
	$if debug_verify {
		verify_and_panic(m, 'promote_memory_to_register')
	}

	// 4. Scalar Optimizations (run until fixed point)
	mut opt_changed := true
	for opt_changed {
		opt_changed = false
		opt_changed = constant_fold(mut m) || opt_changed
		opt_changed = branch_fold(mut m) || opt_changed
		opt_changed = dead_code_elimination(mut m) || opt_changed
		opt_changed = simplify_phi_nodes(mut m) || opt_changed
	}
	$if debug_verify {
		verify_and_panic(m, 'scalar_optimizations')
	}

	merge_blocks(mut m)
	remove_unreachable_blocks(mut m)
	$if debug_verify {
		verify_and_panic(m, 'block_optimizations')
	}

	// 6. Eliminate Phi Nodes (Lower to Copies for Backend)
	// This includes Critical Edge Splitting and Briggs Parallel Copy Resolution
	eliminate_phi_nodes(mut m)
	$if debug_verify {
		verify_and_panic(m, 'eliminate_phi_nodes')
	}

	println('SSA optimization took ${time.since(t)}')
}
