module types

import v.flat

fn test_resolved_references_keep_checked_storage_and_invalidation() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'item')
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.trust_checked_expr_types = true
	tc.remember_expr_type(id, Type(Array{ elem_type: &Type(int_) }))
	first := tc.resolve_type_reference(id)
	assert voidptr(first) == voidptr(tc.expr_type_values[int(id)])
	assert *first == tc.resolve_type(id)
	tc.invalidate_checked_expr_type(int(id))
	tc.remember_expr_type(id, Type(string_))
	assert *tc.resolve_type_reference(id) == Type(string_)
	assert first.name() == '[]int'
}

fn test_resolved_references_preserve_sparse_and_probe_precedence() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.int_literal, '1')
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.trust_checked_expr_types = true
	tc.remember_expr_type(id, Type(string_))
	tc.parallel_check_sparse = true
	tc.check_range_lo = 1
	tc.check_range_hi = 1
	assert *tc.resolve_type_reference(id) == tc.resolve_type(id)
	assert *tc.resolve_type_reference(id) == Type(int_)
	tc.sparse_expr_type_values[int(id)] = Type(bool_)
	mut probe := tc.fork_storage_observation_view()
	assert *probe.resolve_type_reference(id) == Type(bool_)
	probe.sparse_expr_type_values[int(id)] = Type(u64_)
	assert *probe.resolve_type_reference(id) == probe.resolve_type(id)
	assert *probe.resolve_type_reference(id) == Type(u64_)
}

fn test_resolved_references_keep_integer_widening_and_shift_rules() {
	for op in [flat.Op.plus, .eq, .logical_and, .left_shift, .right_shift] {
		mut a := flat.FlatAst.new()
		left := a.add_val(.ident, 'left')
		right := a.add_val(.ident, 'right')
		a.children << [left, right]
		expr := a.add_node(flat.Node{ kind: .infix, op: op, children_count: 2 })
		mut tc := TypeChecker.new(&a)
		tc.reset_node_caches(a.nodes.len)
		tc.trust_checked_expr_types = true
		tc.remember_expr_type(left, Type(u64_))
		tc.remember_expr_type(right, Type(u128_))
		tc.remember_expr_type(expr, Type(u64_))
		expected := if op == .plus { Type(u128_) } else { Type(u64_) }
		assert *tc.resolve_type_reference(expr) == tc.resolve_type(expr)
		assert *tc.resolve_type_reference(expr) == expected
		if op in [.left_shift, .right_shift] {
			tc.remember_expr_type(left, Type(i128_))
			assert *tc.resolve_type_reference(expr) == Type(i128_)
		}
	}
}

fn test_resolved_references_do_not_publish_provisional_unknowns() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'missing')
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.trust_checked_expr_types = true
	tc.remember_expr_type(id, Type(Array{ elem_type: &Type(unknown_type('T')) }))
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	first := tc.resolve_type_reference(id)
	assert *first == tc.resolve_type(id)
	assert tc.body_resolve_memo.filled[int(id)] == 0
	tc.invalidate_checked_expr_type(int(id))
	tc.cur_scope.insert('missing', Type(bool_))
	assert *tc.resolve_type_reference(id) == Type(bool_)
	assert first.name() != 'bool'
}
