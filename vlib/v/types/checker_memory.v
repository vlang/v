module types

import v.flat

// release_node_storage releases per-node cache buffers after complete native
// lowering. The checker and its forks must no longer be queried. Pointed-to type
// and name objects stay alive because the lowered module can borrow their text.
@[unsafe]
pub fn (mut tc TypeChecker) release_node_storage() {
	unsafe {
		tc.resolved_call_names.free()
		tc.resolved_call_set.free()
		tc.statement_nodes.free()
		tc.expr_type_values.free()
		tc.expr_type_set.free()
		tc.lexical_smartcast_misses.free()
		tc.checking_nodes.free()
		tc.direct_parent_ids.free()
		tc.preflight_node_ids.free()
		tc.rewritten_parent_ids.free()
		tc.value_used_nodes.free()
		tc.fn_check_costs.free()
	}
	tc.resolved_call_names = []&CachedName{}
	tc.resolved_call_set = []bool{}
	tc.statement_nodes = []bool{}
	tc.expr_type_values = []&Type{}
	tc.expr_type_set = []bool{}
	tc.lexical_smartcast_misses = []bool{}
	tc.checking_nodes = []bool{}
	tc.direct_parent_ids = []flat.NodeId{}
	tc.preflight_node_ids = []i32{}
	tc.rewritten_parent_ids = []flat.NodeId{}
	tc.value_used_nodes = []bool{}
	tc.fn_check_costs = []i32{}
}
