// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa
import time as _

// Set to true to enable verification after each optimization pass (for debugging)
const debug_verify = false

// Optimize Module
pub fn optimize(mut m ssa.Module) {
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

	// 4. Simplify trivial Phi Nodes
	simplify_phi_nodes(mut m)

	// 5. Eliminate Phi Nodes (convert to copies in predecessor blocks)
	eliminate_phi_nodes(mut m)
}
