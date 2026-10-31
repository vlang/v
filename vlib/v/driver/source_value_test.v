module driver

import v.flat

fn test_incremental_hashes_preserve_source_operand_bytes() {
	legacy := flat.Node{
		kind:  .cast_expr
		value: 'dep.Alias'
		typ:   flat.source_type_int
	}
	canonical := flat.Node{
		kind:         .cast_expr
		source_value: flat.source_type_id('dep.Alias')
		typ:          flat.source_type_int
	}
	different := flat.Node{
		kind:         .cast_expr
		source_value: flat.source_type_id('dep.OtherAlias')
		typ:          flat.source_type_int
	}
	initial := u64(1469598103934665603)
	assert incremental_hash_node_header(initial, &legacy, true) == incremental_hash_node_header(initial,
		&canonical, true)
	assert incremental_hash_node_header(initial, &canonical, true) != incremental_hash_node_header(initial,
		&different, true)
	mut a := flat.FlatAst.new()
	id := a.add_node(canonical)
	canonical_hash := c_hash_monomorph_node(initial, &a, id, []bool{}, map[int]string{})
	a.nodes[int(id)] = legacy
	assert c_hash_monomorph_node(initial, &a, id, []bool{}, map[int]string{}) == canonical_hash
}

fn test_scoped_canonical_nodes_store_source_operands_as_ids() {
	mut a := flat.FlatAst.new()
	legacy := flat.Node{ kind: .cast_expr, value: 'dep.Record', typ: flat.source_type_voidptr }
	canonical := canonical_node_texts(mut a, legacy)
	assert canonical.value == ''
	assert canonical.source_value_id() == flat.source_type_id('dep.Record')
	assert canonical.typ == flat.source_type_voidptr
}

fn test_incremental_hashes_preserve_call_argument_spelling_and_marker_priority() {
	legacy := flat.Node{ kind: .call, value: 'dep.Alias, []int' }
	mut canonical := flat.Node{ kind: .call }
	canonical.set_call_type_args([flat.source_type_id('dep.Alias'), flat.source_type_id('[]int')])
	initial := u64(1469598103934665603)
	assert incremental_hash_node_header(initial, &legacy, true) == incremental_hash_node_header(initial, &canonical, true)
	mut a := flat.FlatAst.new()
	id := a.add_node(canonical)
	canonical_hash := c_hash_monomorph_node(initial, &a, id, []bool{}, map[int]string{})
	a.nodes[int(id)] = legacy
	assert c_hash_monomorph_node(initial, &a, id, []bool{}, map[int]string{}) == canonical_hash
	canonical.value = flat.method_value_borrow_receiver_marker
	marker := flat.Node{ kind: .call, value: flat.method_value_borrow_receiver_marker }
	assert incremental_hash_node_header(initial, &marker, true) == incremental_hash_node_header(initial, &canonical, true)
}

fn test_incremental_hashes_preserve_lowered_type_marker_spelling_layouts() {
	fn_type := flat.source_type_id('fn (dep.Record) []int')
	variant := flat.source_type_id('dep.Variant')
	view := flat.source_type_id('?&dep.Record')
	legacy_params := ['before', '__v3_comptime_method_selector',
		'__v3_comptime_method_selector_fn_type:' + flat.source_type_name(fn_type),
		'__v3_generated_variant_access', flat.source_type_name(variant), '__v3_mut_optional_arg',
		flat.source_type_name(view), 'after']
	legacy := flat.Node{ kind: .selector, value: 'method', typ: flat.source_type_int, payload: flat.node_payload(legacy_params) }
	mut canonical := flat.Node{ kind: .selector, value: 'method', typ: flat.source_type_int }
	canonical.set_generic_params(['before', '__v3_comptime_method_selector',
		'__v3_generated_variant_access', '__v3_mut_optional_arg', 'after'])
	canonical.set_fn_value_type_id(fn_type)
	canonical.set_generated_variant_type_id(variant)
	canonical.set_mut_optional_arg_view_type_id(view)
	assert incremental_legacy_node_params(&canonical) == legacy_params
	initial := u64(1469598103934665603)
	assert incremental_hash_node_header(initial, &legacy, true) == incremental_hash_node_header(initial, &canonical, true)
	mut ast := flat.FlatAst.new()
	id := ast.add_node(canonical)
	canonical_hash := c_hash_monomorph_node(initial, &ast, id, []bool{}, map[int]string{})
	ast.nodes[int(id)] = legacy
	assert c_hash_monomorph_node(initial, &ast, id, []bool{}, map[int]string{}) == canonical_hash
	canonical.set_generated_variant_type_id(flat.source_type_id('dep.OtherVariant'))
	assert incremental_hash_node_header(initial, &canonical, true) != incremental_hash_node_header(initial, &legacy, true)
	canonical.set_generic_params([]string{})
	assert incremental_legacy_node_params(&canonical).len == 6
}

fn test_incremental_hashes_distinguish_missing_lowered_recognition_markers() {
	markers := ['__v3_comptime_method_selector', '__v3_generated_variant_access',
		'__v3_mut_optional_arg']
	for i, marker in markers {
		mut unmarked := flat.Node{ kind: .selector, value: 'method' }
		match i {
			0 { unmarked.set_fn_value_type_id(flat.source_type_id('fn () int')) }
			1 { unmarked.set_generated_variant_type_id(flat.source_type_id('dep.Variant')) }
			else {
				unmarked.kind = .prefix
				unmarked.op = .amp
				unmarked.set_mut_optional_arg_view_type_id(flat.source_type_id('?&dep.Record'))
			}
		}
		mut marked := unmarked
		marked.set_generic_params([marker])
		assert incremental_legacy_node_params(&unmarked) != incremental_legacy_node_params(&marked)
		initial := u64(1469598103934665603)
		assert incremental_hash_node_header(initial, &unmarked, true) != incremental_hash_node_header(initial,
			&marked, true)
		mut ast := flat.FlatAst.new()
		id := ast.add_node(unmarked)
		unmarked_hash := c_hash_monomorph_node(initial, &ast, id, []bool{}, map[int]string{})
		ast.nodes[int(id)] = marked
		assert c_hash_monomorph_node(initial, &ast, id, []bool{}, map[int]string{}) != unmarked_hash
	}
}
