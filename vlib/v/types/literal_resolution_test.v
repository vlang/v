module types

import v.flat

fn test_literal_resolution_matches_uncached_with_and_without_body_memo() {
	mut a := flat.FlatAst.new()
	mut ids := []flat.NodeId{}
	for kind in [flat.NodeKind.int_literal, .float_literal, .bool_literal, .char_literal,
		.string_literal, .string_interp, .sizeof_expr, .offsetof_expr, .nil_literal, .none_expr] {
		ids << a.add_val(kind, '1')
	}
	ids << a.add_val(.char_literal, 'c:x')
	ids << a.add_val(.string_literal, '__v3_comptime_zero')
	ids << a.add_val(.string_literal, '__v3_comptime_new')
	mut tc := TypeChecker.new(&a)
	for active in [false, true] {
		if active { tc.arm_body_resolve_memo(0, a.nodes.len - 1) }
		for _ in 0 .. 2 {
			for id in ids {
				assert semantic_types_equal(tc.resolve_type(id), tc.resolve_type_uncached(id))
			}
		}
	}
	// Even when some translated files exist, literals in this ordinary AST keep their type.
	tc.translated_files['translated.c.v'] = true
	for id in ids {
		assert semantic_types_equal(tc.resolve_type(id), tc.resolve_type_uncached(id))
	}
}

fn test_literal_resolution_preserves_checked_and_storage_probe_precedence() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.int_literal, '1')
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.expr_type_values[int(id)] = new_type_reference(builtin_i64_type)
	tc.expr_type_set[int(id)] = true
	tc.trust_checked_expr_types = true
	assert tc.resolve_type(id) == builtin_i64_type
	tc.trust_checked_expr_types = false
	tc.expr_type_set[int(id)] = false
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	tc.body_resolve_memo.types[int(id)] = new_type_reference(builtin_u64_type)
	tc.body_resolve_memo.filled[int(id)] = tc.body_resolve_memo.type_generation
	mut probe := tc.fork_storage_observation_view()
	assert probe.resolve_type(id) == builtin_u64_type
	assert !probe.storage_query_probe.recheck_requested
}
