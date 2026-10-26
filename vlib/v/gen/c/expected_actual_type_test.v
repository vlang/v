module c

import v.flat
import v.types

fn expected_actual_node(mut a flat.FlatAst, kind flat.NodeKind, op flat.Op, value string, typ string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		op:             op
		value:          value
		typ:            flat.source_type_id(typ)
		children_start: start
		children_count: children.len
	})
}

fn test_expected_actual_local_storage_overrides_checked_and_known_types() {
	mut a := flat.FlatAst.new()
	id := a.add_node(flat.Node{ kind: .ident, value: 'value', typ: flat.source_type_id('string') })
	mut tc := types.TypeChecker.new(&a)
	tc.file_scope = &types.Scope{}
	tc.cur_scope = &types.Scope{ parent: tc.file_scope }
	int_type := types.Type(types.int_)
	pointer := types.Type(types.Pointer{ base_type: int_type })
	tc.cur_scope.insert('value', pointer)
	tc.register_synth_type(id, types.Type(types.string_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for known in [false, true] {
		assert g.expected_expr_actual_type(id, a.node(id), known, types.Type(types.bool_)) == pointer
		g.known_expr_type_id = if known { int(id) } else { -1 }
		g.known_expr_type = types.Type(types.bool_)
		g.assert_expr_overrides[int(id)] = 'value'
		assert g.expr_to_string_with_expected_type(id, int_type) == '*value'
		assert g.known_expr_type_id == -1
	}
	g.cur_param_types['value'] = types.new_type_reference(int_type)
	assert g.expected_expr_actual_type(id, a.node(id), true, pointer) == int_type
}

fn test_expected_actual_declared_selector_storage_overrides_checked_and_known_types() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'holder')
	field := expected_actual_node(mut a, .selector, .none, 'value', '', [base])
	mut tc := types.TypeChecker.new(&a)
	int_type := types.Type(types.int_)
	pointer := types.Type(types.Pointer{ base_type: int_type })
	tc.structs['Holder'] = [types.StructField{ name: 'value', typ: pointer }]
	tc.register_synth_type(field, types.Type(types.string_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cur_param_types['holder'] = tc.parse_type(flat.source_type_id('Holder'))
	for known in [false, true] {
		assert g.expected_expr_actual_type(field, a.node(field), known, int_type) == pointer
		g.known_expr_type_id = if known { int(field) } else { -1 }
		g.known_expr_type = int_type
		g.assert_expr_overrides[int(field)] = 'holder.value'
		assert g.expr_to_string_with_expected_type(field, int_type) == '*holder.value'
		assert g.known_expr_type_id == -1
	}
}

fn test_expected_actual_optional_selector_annotation_wins_over_storage() {
	for annotation in ['?int', '!int'] {
		mut a := flat.FlatAst.new()
		base := a.add_val(.ident, 'holder')
		field := expected_actual_node(mut a, .selector, .none, 'value', annotation, [base])
		mut tc := types.TypeChecker.new(&a)
		tc.structs['Holder'] = [types.StructField{ name: 'value', typ: types.Type(types.string_) }]
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.cur_param_types['holder'] = tc.parse_type(flat.source_type_id('Holder'))
		expected := tc.parse_resolution_type(flat.source_type_id(annotation))
		for known in [false, true] {
			assert g.expected_expr_actual_type(field, a.node(field), known, types.Type(types.bool_)) == expected
		}
	}
}

fn test_expected_actual_source_mut_pointer_deref_overrides_known_types_through_wrappers() {
	mut a := flat.FlatAst.new()
	param := a.add_val(.ident, 'slot')
	deref := expected_actual_node(mut a, .prefix, .mul, source_mut_pointer_deref_marker, '', [param])
	paren := expected_actual_node(mut a, .paren, .none, '', '', [deref])
	block := expected_actual_node(mut a, .block, .none, '', '', [paren])
	mut tc := types.TypeChecker.new(&a)
	int_type := types.Type(types.int_)
	pointer := types.Type(types.Pointer{ base_type: types.Type(types.Pointer{ base_type: int_type }) })
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cur_param_types['slot'] = types.new_type_reference(pointer)
	g.cur_mut_params['slot'] = true
	g.cur_mut_pointer_params['slot'] = true
	g.cur_mut_param_owners['slot'] = tc.cur_scope.insert_with_owner('slot', pointer)
	for id in [deref, paren, block] {
		for known in [false, true] {
			assert g.expected_expr_actual_type(id, a.node(id), known, types.Type(types.string_)) == int_type
		}
	}
}

fn test_expected_actual_unknown_storage_keeps_known_and_checker_fallbacks() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'unbound')
	base := a.add_val(.ident, 'unknown_base')
	field := expected_actual_node(mut a, .selector, .none, 'field', '', [base])
	mut tc := types.TypeChecker.new(&a)
	int_type := types.Type(types.int_)
	for node_id in [id, field] { tc.register_synth_type(node_id, int_type) }
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for node_id in [id, field] {
		assert g.expected_expr_actual_type(node_id, a.node(node_id), true, types.Type(types.string_)) == types.Type(types.string_)
		assert g.expected_expr_actual_type(node_id, a.node(node_id), false, types.Type(types.string_)) == int_type
	}
	g.known_expr_type_id = int(field)
	g.known_expr_type = types.Type(types.string_)
	g.assert_expr_overrides[int(id)] = 'unbound'
	old_expected := types.Type(types.bool_)
	old_enum := flat.source_type_id('PreviousEnum')
	g.expected_expr_type = old_expected
	g.expected_enum = old_enum
	assert g.expr_to_string_with_expected_type(id, int_type) == 'unbound'
	assert g.known_expr_type_id == int(field)
	assert g.expected_expr_type == old_expected
	assert g.expected_enum == old_enum
}
