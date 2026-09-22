	value_context := !tc.is_statement_node(id) && tc.expression_node_used_as_value(id)
	cond_id := tc.a.child(&node, 0)
	condition := tc.a.node(cond_id)
	// Branch hints also use .paren nodes, but their parentheses are required.
	if condition.kind == .paren && condition.value != '__v3_comptime_d'
		&& tc.node_source_starts_with(cond_id, '(') {
		tc.record_warning_at(.condition_mismatch, 'unnecessary `()` in `if` condition, use `if expr {` instead of `if (expr) {`.', cond_id, tc.if_parenthesized_condition_pos(condition))
	}
	saved_mut_local_owners := tc.fn_context.mut_local_owners.clone()
	defer {
		tc.fn_context.mut_local_owners = saved_mut_local_owners.clone()
