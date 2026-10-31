module types

import v.flat

fn seed_postcheck_widening_type(mut tc TypeChecker, id flat.NodeId, typ Type) {
	// Model the narrow record merged from a worker that could not see a wide
	// operand. remember_expr_type would widen it before the post-check pass runs.
	tc.invalidate_checked_type_id(int(id))
	tc.expr_type_values[int(id)] = tc.intern_type_reference(typ)
	tc.expr_type_set[int(id)] = true
}

fn test_postcheck_widening_updates_recorded_types_and_semantic_ids() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.ident, 'left')
	right := a.add_val(.ident, 'right')
	a.children << [left, right]
	expr := a.add_node(flat.Node{
		kind:           .infix
		op:             .plus
		children_count: 2
	})
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.trust_checked_expr_types = true
	tc.remember_expr_type(left, Type(u64_))
	tc.remember_expr_type(right, Type(u128_))
	for typ in [Type(int_), Type(u64_), Type(usize_), Type(rune_)] {
		seed_postcheck_widening_type(mut tc, expr, typ)
		assert tc.cached_expr_type(expr)? == typ
		before := tc.checked_expr_type_id(expr)?
		tc.widen_mixed_integer_expr_types()
		assert tc.cached_expr_type(expr)? == Type(u128_)
		after := tc.checked_expr_type_id(expr)?
		assert after != before
		assert tc.type_name_by_id(after) == flat.source_type_id('u128')
	}
	// Qualified nominal spellings selected by the existing widening rules retain
	// their identity instead of becoming the builtin with the same short name.
	wide_alias := Type(Alias{
		name_id:   flat.source_type_id('user.i128')
		base_type: &Type(i128_)
	})
	tc.remember_expr_type(left, wide_alias)
	seed_postcheck_widening_type(mut tc, expr, Type(int_))
	assert tc.cached_expr_type(expr)? == Type(int_)
	tc.widen_mixed_integer_expr_types()
	assert tc.cached_expr_type(expr)? == wide_alias
	assert tc.type_name_by_id(tc.checked_expr_type_id(expr)?) == wide_alias.source_type_id()
}

fn test_postcheck_widening_preserves_ineligible_types_and_shift_positions() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.ident, 'left')
	right := a.add_val(.ident, 'right')
	a.children << [left, right]
	expr := a.add_node(flat.Node{
		kind:           .infix
		op:             .plus
		children_count: 2
	})
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.trust_checked_expr_types = true
	tc.remember_expr_type(left, Type(u64_))
	tc.remember_expr_type(right, Type(u128_))
	unchanged := [
		Type(bool_),
		Type(f64_),
		Type(string_),
		Type(u128_),
		Type(Alias{ name_id: flat.source_type_id('user.int'), base_type: &Type(int_) }),
		Type(Array{ elem_type: &Type(int_) }),
		Type(Pointer{ base_type: &Type(int_) }),
		Type(OptionType{ base_type: &Type(int_) }),
		Type(FnType{ return_type: &Type(int_) }),
	]
	for typ in unchanged {
		seed_postcheck_widening_type(mut tc, expr, typ)
		before := tc.checked_expr_type_id(expr)?
		stored := tc.expr_type_values[int(expr)]
		tc.widen_mixed_integer_expr_types()
		assert tc.cached_expr_type(expr)? == typ
		assert tc.checked_expr_type_id(expr)? == before
		assert voidptr(tc.expr_type_values[int(expr)]) == voidptr(stored)
	}
	for op in [flat.Op.eq, .logical_and, .left_shift, .right_shift, .right_shift_unsigned] {
		a.nodes[int(expr)].op = op
		seed_postcheck_widening_type(mut tc, expr, Type(u64_))
		tc.widen_mixed_integer_expr_types()
		assert tc.cached_expr_type(expr)? == Type(u64_)
	}
	a.nodes[int(expr)].op = .left_shift
	tc.remember_expr_type(left, Type(i128_))
	seed_postcheck_widening_type(mut tc, expr, Type(int_))
	assert tc.cached_expr_type(expr)? == Type(int_)
	tc.widen_mixed_integer_expr_types()
	assert tc.cached_expr_type(expr)? == Type(i128_)
}
