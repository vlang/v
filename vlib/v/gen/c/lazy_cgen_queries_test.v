module c

import v.flat
import v.token
import v.types

fn lazy_cgen_node(mut a flat.FlatAst, kind flat.NodeKind, op flat.Op, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		op:             op
		children_start: start
		children_count: children.len
	})
}

fn test_checked_boolean_metadata_keeps_storage_and_unknown_types_on_fallback() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.bool_literal, 'true')
	right := a.add_val(.bool_literal, 'false')
	comparison := lazy_cgen_node(mut a, .infix, .eq, [left, right])
	negation := lazy_cgen_node(mut a, .prefix, .not, [left])
	ident := a.add_val(.ident, 'flag')
	selector := lazy_cgen_node(mut a, .selector, .none, [ident])
	call := lazy_cgen_node(mut a, .call, .none, [ident])
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for id in [left, comparison, negation, ident, selector, call] {
		a.nodes[int(id)].typ = flat.source_type_bool
		assert !g.checked_boolean_operand(id)
		tc.register_synth_type(id, types.Type(types.bool_))
		assert g.checked_boolean_operand(id) == (id == comparison)
	}
	for typ in [types.Type(types.int_), types.Type(types.f32_), types.Type(types.i128_),
		types.Type(types.Unknown{}), types.Type(types.Struct{ name_id: flat.source_type_id('Flag') })] {
		tc.register_synth_type(comparison, typ)
		assert !g.checked_boolean_operand(comparison)
	}
}

fn test_boolean_infix_keeps_parentheses_overrides_and_expected_context() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.bool_literal, 'true')
	right := a.add_val(.bool_literal, 'false')
	comparison := lazy_cgen_node(mut a, .infix, .ne, [left, right])
	choice := lazy_cgen_node(mut a, .infix, .logical_or, [left, right])
	conjunction := lazy_cgen_node(mut a, .infix, .logical_and, [comparison, choice])
	mut tc := types.TypeChecker.new(&a)
	for id in [left, right, comparison, choice, conjunction] {
		tc.register_synth_type(id, types.Type(types.bool_))
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.expected_enum = flat.source_type_id('OuterEnum')
	g.expected_expr_type = types.Type(types.f32_)
	assert g.expr_to_string(conjunction) == '(true != false) && (true || false)'
	g.assert_expr_overrides[int(comparison)] = 'left_override'
	g.callback_target_overrides[int(choice)] = 'right_override'
	assert g.expr_to_string(conjunction) == '(left_override) && (right_override)'
	assert g.expected_enum == flat.source_type_id('OuterEnum')
	assert g.expected_expr_type == types.Type(types.f32_)
	// Checked boolean infixes need no usable-type memo probes.
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	assert g.expr_to_string(conjunction) == '(left_override) && (right_override)'
	for id in [comparison, choice] {
		assert g.usable_expr_type_memo.gens[int(id) & usable_expr_type_memo_mask] != g.usable_expr_type_memo.generation
	}
}

fn test_boolean_infix_resolves_unannotated_identifiers_and_stale_annotations() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.ident, 'left')
	right := a.add_val(.ident, 'right')
	conjunction := lazy_cgen_node(mut a, .infix, .logical_and, [left, right])
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cur_param_types['left'] = types.Type(types.bool_)
	g.cur_param_types['right'] = types.Type(types.bool_)
	assert g.expr_to_string(conjunction) == 'left && right'
	// A flat bool annotation must not override the authoritative checked type.
	first := lazy_cgen_node(mut a, .infix, .eq, [left, right])
	second := lazy_cgen_node(mut a, .infix, .eq, [left, right])
	outer := lazy_cgen_node(mut a, .infix, .logical_and, [first, second])
	a.nodes[int(first)].typ = flat.source_type_bool
	tc.register_synth_type(first, types.Type(types.Struct{ name_id: flat.source_type_id('CustomFlag') }))
	tc.register_synth_type(second, types.Type(types.bool_))
	g.assert_expr_overrides[int(first)] = 'custom_flag'
	g.assert_expr_overrides[int(second)] = 'boolean_flag'
	assert g.expr_to_string(outer) == 'custom_flag && boolean_flag'
}

fn test_boolean_infix_translated_and_unknown_metadata_still_use_normal_resolution() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.bool_literal, 'true')
	right := a.add_val(.bool_literal, 'false')
	conjunction := lazy_cgen_node(mut a, .infix, .logical_and, [left, right])
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('translated_boolean.v', 1)
	a.nodes[int(conjunction)].pos = token.new_pos(1, 0)
	mut tc := types.TypeChecker.new(&a)
	tc.translated_files['translated_boolean.v'] = true
	for id in [left, right, conjunction] {
		tc.register_synth_type(id, types.Type(types.bool_))
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	assert g.expr_to_string(conjunction) == 'true && false'
	for id in [left, right] {
		assert g.usable_expr_type_memo.gens[int(id) & usable_expr_type_memo_mask] == g.usable_expr_type_memo.generation
	}
	g.end_usable_expr_type_memo()
	tc.translated_files.clear()
	tc.register_synth_type(left, types.Type(types.Unknown{}))
	tc.register_synth_type(right, types.Type(types.Unknown{}))
	g.begin_usable_expr_type_memo()
	assert g.expr_to_string(conjunction) == 'true && false'
	for id in [left, right] {
		assert g.usable_expr_type_memo.gens[int(id) & usable_expr_type_memo_mask] == g.usable_expr_type_memo.generation
	}
}

fn test_discarded_infix_handlers_do_not_resolve_the_result_type() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.int_literal, '7')
	right := a.add_val(.int_literal, '2')
	result := lazy_cgen_node(mut a, .infix, .plus, [left, right])
	mut tc := types.TypeChecker.new(&a)
	tc.register_synth_type(result, types.Type(types.int_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	for builtins in [false, true] {
		g.has_builtins = builtins
		assert !g.gen_safe_integer_division(result, a.nodes[int(result)], left, right)
	}
	a.nodes[int(result)].op = .div
	g.static_c_initializer = true
	assert !g.gen_safe_integer_division(result, a.nodes[int(result)], left, right)
	for op in [flat.Op.logical_and, .logical_or, .power, .arrow] {
		a.nodes[int(result)].op = op
		assert !g.gen_int128_infix(result, a.nodes[int(result)], left, right, types.Type(types.int_), types.Type(types.int_))
	}
	assert g.usable_expr_type_memo.gens[int(result) & usable_expr_type_memo_mask] != g.usable_expr_type_memo.generation
}

fn test_lazy_division_and_int128_handlers_keep_supported_operations() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.int_literal, '7')
	right := a.add_val(.int_literal, '2')
	result := lazy_cgen_node(mut a, .infix, .div, [left, right])
	mut tc := types.TypeChecker.new(&a)
	for id in [left, right, result] {
		tc.register_synth_type(id, types.Type(types.int_))
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.has_builtins = true
	division := g.expr_to_string(result)
	assert division.contains('division by zero')
	assert division.contains('v_panic(')
	a.nodes[int(result)].op = .mod
	assert g.expr_to_string(result).contains('modulo by zero')
	for portable in [false, true] {
		g.compile_defines = if portable { ['v3_no_native_int128'] } else { []string{} }
		g.has_builtins = false
		tc.register_synth_type(result, types.Type(types.i128_))
		a.nodes[int(result)].op = .plus
		assert g.expr_to_string(result) == '__v_i128_add(__v_i128_from_i64((i64)(7)), __v_i128_from_i64((i64)(2)))'
		a.nodes[int(result)].op = .div
		assert g.expr_to_string(result) == '__v_i128_div(__v_i128_from_i64((i64)(7)), __v_i128_from_i64((i64)(2)))'
	}
}

fn test_prefix_type_query_keeps_pointer_annotations_and_avoids_unused_child_queries() {
	mut a := flat.FlatAst.new()
	child := a.add_val(.ident, 'value')
	negation := lazy_cgen_node(mut a, .prefix, .not, [child])
	address := lazy_cgen_node(mut a, .prefix, .amp, [child])
	deref := lazy_cgen_node(mut a, .prefix, .mul, [address])
	mut tc := types.TypeChecker.new(&a)
	tc.register_synth_type(negation, types.Type(types.bool_))
	tc.register_synth_type(child, types.Type(types.int_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	assert *g.usable_expr_type(negation) == types.Type(types.bool_)
	assert g.usable_expr_type_memo.gens[int(child) & usable_expr_type_memo_mask] != g.usable_expr_type_memo.generation
	assert g.usable_expr_type(address).name() == '&int'
	assert *g.usable_expr_type(deref) == types.Type(types.int_)
	g.end_usable_expr_type_memo()
	a.nodes[int(deref)].typ = flat.source_type_id('&bool')
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(deref).name() == '&bool'
	assert g.usable_expr_type_memo.gens[int(address) & usable_expr_type_memo_mask] != g.usable_expr_type_memo.generation
}

fn test_ierror_alias_tracking_keeps_array_get_alias_propagation() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'borrowed')
	callee := a.add_val(.ident, 'array__get')
	call := lazy_cgen_node(mut a, .call, .none, [callee, base])
	paren := lazy_cgen_node(mut a, .paren, .none, [call])
	deref := lazy_cgen_node(mut a, .prefix, .mul, [paren])
	lhs := a.add_val(.ident, 'alias')
	mut tc := types.TypeChecker.new(&a)
	tc.cur_scope = types.new_scope(tc.cur_scope)
	tc.cur_scope.insert('borrowed', types.Type(types.int_))
	tc.cur_scope.insert('alias', types.Type(types.int_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.ierror_stack_pointer_aliases << map[string]bool{}
	g.declare_ierror_pointer_alias('borrowed', true)
	for id in [call, paren, deref] {
		assert g.ierror_array_get_pointer_alias_needs_copy(a.nodes[int(id)])
		assert g.ierror_pointer_payload_expr_needs_heap_copy(a.nodes[int(id)])
		g.track_ierror_stack_pointer_alias(a.nodes[int(lhs)], a.nodes[int(id)])
		assert g.ierror_pointer_alias_needs_copy('alias')
		g.assign_ierror_pointer_alias('alias', false)
		g.track_ierror_stack_pointer_alias_assign(a.nodes[int(lhs)], a.nodes[int(id)])
		assert g.ierror_pointer_alias_needs_copy('alias')
	}
}
