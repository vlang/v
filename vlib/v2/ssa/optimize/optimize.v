// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa
import time

// Optimize Module
pub fn optimize(mut m ssa.Module) {
	t := time.now()
	// 1. Build Control Flow Graph (Predecessors)
	build_cfg(mut m)

	// 2. Compute Dominator Tree (Lengauer-Tarjan)
	compute_dominators(mut m)

	// 3. Promote Memory to Register (Construct SSA / Phi Nodes)
	// NOTE: Disabled due to bug with nested loops containing break statements
	// promote_memory_to_register(mut m)

	// 4. Scalar Optimizations (run until fixed point)
	mut opt_changed := true
	for opt_changed {
		opt_changed = false
		opt_changed = constant_fold(mut m) || opt_changed
		opt_changed = branch_fold(mut m) || opt_changed
		opt_changed = dead_code_elimination(mut m) || opt_changed
		opt_changed = simplify_phi_nodes(mut m) || opt_changed
	}

	merge_blocks(mut m)
	remove_unreachable_blocks(mut m)

	// 6. Eliminate Phi Nodes (Lower to Copies for Backend)
	// This includes Critical Edge Splitting and Briggs Parallel Copy Resolution
	eliminate_phi_nodes(mut m)

	println('SSA optimization took ${time.since(t)}')
}
