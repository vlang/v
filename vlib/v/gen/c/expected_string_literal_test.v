module c

import v.flat
import v.types

fn expected_string_alias() types.Type {
	return types.Alias{
		name_id:   flat.source_type_id('StringAlias')
		base_type: types.new_type_reference(types.Type(types.string_))
	}
}

fn expected_string_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: start
		children_count: children.len
	})
}

fn expected_string_output(mut g FlatGen, id flat.NodeId, expected types.Type, known types.Type, has_known bool) string {
	g.known_expr_type_id = if has_known { int(id) } else { -1 }
	g.known_expr_type = known
	return g.expr_to_string_with_expected_type(id, expected)
}

fn test_expected_string_literal_matches_conversion_fallback_and_restores_context() {
	for raw in [false, true] {
		mut a := flat.FlatAst.new()
		id := a.add_node(flat.Node{
			kind:  .string_literal
			value: 'hello'
			typ:   flat.source_type_id(if raw { 'raw:string' } else { 'string' })
		})
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		old_expected := types.Type(types.Pointer{ base_type: types.Type(types.u8_) })
		old_enum := flat.source_type_id('PreviousEnum')
		g.expected_expr_type = old_expected
		g.expected_enum = old_enum
		for has_known in [false, true] {
			actual := types.Type(types.string_)
			result := expected_string_output(mut g, id, actual, actual, has_known)
			fallback := expected_string_output(mut g, id, expected_string_alias(), actual, has_known)
			assert result == '_str_0'
			assert result == fallback
			assert g.known_expr_type_id == -1
			assert g.expected_expr_type == old_expected
			assert g.expected_enum == old_enum
		}
	}
}

fn test_expected_string_interpolation_and_lowered_join_keep_child_context() {
	for join in [false, true] {
		mut a := flat.FlatAst.new()
		left := a.add_node(flat.Node{ kind: .string_literal, value: 'hello', typ: flat.source_type_id('raw:string') })
		right := a.add_val(.string_literal, 'world')
		id := expected_string_node(mut a, .string_interp, if join { '__v3_string_join' } else { '' }, [
			left,
			right,
		])
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		old_expected := types.Type(types.Pointer{ base_type: types.Type(types.u8_) })
		g.expected_expr_type = old_expected
		for has_known in [false, true] {
			g.tmp_count = 0
			result := expected_string_output(mut g, id, types.Type(types.string_), types.Type(types.string_), has_known)
			g.tmp_count = 0
			fallback := expected_string_output(mut g, id, expected_string_alias(), types.Type(types.string_), has_known)
			assert result == fallback
			assert result.contains('_str_0') && result.contains('_str_1')
			assert !result.contains('"hello"')
			assert g.known_expr_type_id == -1
			assert g.expected_expr_type == old_expected
		}
	}
}

fn test_expected_string_conflicting_known_pointer_keeps_conversion() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.string_literal, 'hello')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	known := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
	result := expected_string_output(mut g, id, types.Type(types.string_), known, true)
	assert result == 'ptr_str(_str_0)'
	assert g.known_expr_type_id == -1
}

fn test_expected_string_preserves_expression_overrides_and_unrelated_known_id() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.string_literal, 'hello')
	other := a.add_val(.int_literal, '7')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.assert_expr_overrides[int(id)] = 'replacement'
	g.known_expr_type_id = int(other)
	g.known_expr_type = types.Type(types.int_)
	assert g.expr_to_string_with_expected_type(id, types.Type(types.string_)) == 'replacement'
	assert g.known_expr_type_id == int(other)
	g.assert_expr_overrides.clear()
	g.callback_target_overrides[int(id)] = 'callback_replacement'
	assert expected_string_output(mut g, id, types.Type(types.string_), types.Type(types.string_), true) == 'callback_replacement'
	assert g.known_expr_type_id == -1
}
