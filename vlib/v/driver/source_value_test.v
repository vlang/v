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
