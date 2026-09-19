fn (mut tc TypeChecker) check_mutable_alias_assignment_lhs(id flat.NodeId, rhs_id flat.NodeId) {
	if tc.unsafe_depth > 0 || tc.expr_is_unsafe_reference_alias(rhs_id) || !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .index && node.children_count > 0 {
		base_id := tc.a.child(node, 0)
		base := tc.a.node(base_id)
		mut aliases :=
			(base.kind == .ident && tc.fn_context.immutable_reference_aliases[base.value])
				|| (base.kind == .call && tc.call_immutable_alias_source(base_id) != none)
		if base.kind == .selector {
			if root_id := tc.lvalue_root_ident(base_id) {
				root := tc.a.node(root_id)
				aliases = aliases || tc.fn_context.immutable_reference_aliases[root.value]
			}
		}
		if aliases && !tc.mutable_alias_has_fresh_map_storage(base_id) {
			tc.record_error_at(.assignment_mismatch, '`${tc.source_text_for_node(base_id)}` aliases mutable data from an immutable value, clone it first (or use `unsafe`)', base_id, if base.kind in [
				.ident,
				.selector,
			] {
				tc.node_value_diagnostic_pos(base_id)
			} else {
				base.pos
			})
		}
		return
	}
}
