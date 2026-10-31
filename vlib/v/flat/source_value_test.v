module flat

import v.token

fn test_source_value_preserves_original_operand_independently_of_annotation() {
	mut ast := FlatAst.new()
	original := ' &pkg.Alias[T] '
	id := ast.add_node(Node{
		kind:  .cast_expr
		value: original
		typ:   source_type_id('&pkg.Resolved[int]')
	})
	node := ast.node(id)
	assert node.value == ''
	assert node.source_value_id() == source_type_id(original)
	assert node.value_text() == original
	assert node.typ_text() == '&pkg.Resolved[int]'
	assert node.source_value_id() != node.typ
	if sizeof(voidptr) == 8 {
		assert sizeof(Node) == 64
	}
}

fn test_source_value_canonicalization_covers_operand_kinds_and_internal_markers() {
	mut ast := FlatAst.new()
	for kind in [NodeKind.struct_init, .assoc, .cast_expr, .as_expr, .is_expr, .map_init, .array_init,
		.array_literal, .sizeof_expr, .typeof_expr, .offsetof_expr] {
		id := ast.add_val(kind, 'pkg.Original[T]')
		node := ast.node(id)
		assert node.value == ''
		assert node.source_value == source_type_id('pkg.Original[T]')
		assert node.value_text() == 'pkg.Original[T]'
	}
	array_id := ast.add_val_id(int(NodeKind.array_init), '__v3_comptime_type_array')
	typeof_id := ast.add_val(.typeof_expr, '__v3_generic_type_name:T')
	assert ast.node(array_id).value == ''
	assert ast.node(array_id).value_text() == '__v3_comptime_type_array'
	assert ast.node(typeof_id).value == ''
	assert ast.node(typeof_id).value_text() == '__v3_generic_type_name:T'
}

fn test_source_value_setters_preserve_type_only_identifiers_and_clear_both_representations() {
	mut node := Node{
		kind:  .ident
		value: 'ordinary_name'
	}
	node.set_value_text('another_name')
	assert node.value == 'another_name'
	assert node.source_value == 0
	node.set_source_value_id(source_type_id('[]T'))
	assert node.value == ''
	assert node.value_text() == '[]T'
	node.set_value_text('[]int')
	assert node.value == ''
	assert node.source_value_id() == source_type_id('[]int')
	node.set_value_text('')
	assert node.value == ''
	assert node.source_value_id() == 0
	assert node.value_text() == ''
	node.set_source_value_id(source_type_int)
	node.set_source_value_id(0)
	assert node.value_text() == ''
}

fn test_source_value_raw_rewrites_take_priority_until_canonicalized() {
	mut node := Node{
		kind:         .struct_init
		value:        'pkg.New'
		source_value: source_type_id('pkg.Old')
	}
	assert node.value_text() == 'pkg.New'
	assert node.source_value_id() == source_type_id('pkg.New')
	node.canonicalize_source_value()
	assert node.value == ''
	assert node.source_value_id() == source_type_id('pkg.New')
	legacy := Node{ kind: .sizeof_expr, value: 'pkg.SizeOperand', typ: source_type_usize }
	assert legacy.value_text() == 'pkg.SizeOperand'
	assert legacy.source_value_id() == source_type_id('pkg.SizeOperand')
}

fn test_source_value_copies_preserve_operand_and_independent_managed_payload() {
	mut node := Node{
		kind:           .struct_init
		typ:            source_type_id('pkg.Refined[int]')
		children_start: 5
		children_count: 2
		pos:            token.new_pos(1, 20)
	}
	node.set_source_value_id(source_type_id('pkg.Original[T]'))
	node.set_generic_params(['T'])
	shifted := node.with_shifted_children(7)
	repositioned := node.with_pos(token.new_pos(2, 40))
	cloned := node.clone_owned()
	for copied in [shifted, repositioned, cloned] {
		assert copied.value == ''
		assert copied.source_value == node.source_value
		assert copied.value_text() == 'pkg.Original[T]'
		assert copied.typ == node.typ
		assert copied.generic_params() == ['T']
	}
	assert shifted.children_start == 12
	assert repositioned.pos.id == 2
	node.set_generic_params(['Changed'])
	assert cloned.generic_params() == ['T']
}

fn test_source_value_intern_scans_canonicalize_raw_slots_without_ordinary_text_entries() {
	mut ast := FlatAst.new()
	ast.nodes << Node{ kind: .map_init, value: 'map[string]pkg.Value' }
	ast.intern_node_texts_range(0, 1)
	assert ast.nodes[0].value == ''
	assert ast.nodes[0].source_value_id() == source_type_id('map[string]pkg.Value')
	assert ast.text_count() == 0
	ast.nodes << Node{ kind: .typeof_expr, value: '__v3_generic_type_name:T' }
	ast.intern_node_texts_range_wide(1, 2)
	assert ast.nodes[1].value == ''
	assert ast.nodes[1].value_text() == '__v3_generic_type_name:T'
	assert ast.text_count() == 0
}

struct SourceValueAppendCase {
	node           Node
	expected_value string
	expected_id    u32
}

fn test_add_node_preserves_immutable_and_legacy_operands_with_and_without_capacity() {
	original_id := source_type_id('pkg.Original[T]')
	rewritten_id := source_type_id('pkg.Rewritten[int]')
	marker := '__v3_generic_type_name:T'
	cases := [
		SourceValueAppendCase{
			node:           Node{ kind: .ident, value: 'ordinary_name' }
			expected_value: 'ordinary_name'
		},
		SourceValueAppendCase{
			node:        Node{ kind: .ident, source_value: original_id }
			expected_id: original_id
		},
		SourceValueAppendCase{
			node:        Node{ kind: .cast_expr, value: 'pkg.Original[T]' }
			expected_id: original_id
		},
		SourceValueAppendCase{
			node:        Node{ kind: .ident, value: 'pkg.Rewritten[int]', source_value: original_id }
			expected_id: rewritten_id
		},
		SourceValueAppendCase{
			node:        Node{ kind: .typeof_expr, value: marker, source_value: original_id }
			expected_id: source_type_id(marker)
		},
		SourceValueAppendCase{
			node:        Node{ kind: .struct_init, source_value: original_id }
			expected_id: original_id
		},
		SourceValueAppendCase{
			node: Node{ kind: .cast_expr }
		},
		SourceValueAppendCase{
			node:           Node{ kind: .call, value: method_value_borrow_receiver_marker }
			expected_value: method_value_borrow_receiver_marker
		},
	]
	for force_growth in [false, true] {
		for fixture in cases {
			mut ast := FlatAst.new()
			if force_growth {
				ast.nodes = []Node{}
			}
			mut node := fixture.node
			node.typ = source_type_id('pkg.Refined')
			node.children_start = 7
			node.children_count = 2
			node.pos = token.new_pos(3, 40)
			node.is_mut = true
			node.flags = node_flags(true, true)
			node.set_generic_params(['T'])
			node.set_call_type_args([source_type_int, source_type_string])
			id := ast.add_node(node)
			stored := ast.node(id)
			assert id == NodeId(0)
			assert ast.nodes.len == 1
			assert stored.value == fixture.expected_value
			assert stored.source_value == fixture.expected_id
			assert stored.typ == node.typ
			assert stored.kind == node.kind
			assert stored.op == node.op
			assert stored.children_start == node.children_start
			assert stored.children_count == node.children_count
			assert stored.pos == node.pos
			assert stored.is_mut == node.is_mut
			assert stored.flags == node.flags
			assert stored.payload == node.payload
			assert stored.generic_params() == ['T']
			assert stored.call_type_args() == [source_type_int, source_type_string]
			assert node.value == fixture.node.value
			assert node.source_value == fixture.node.source_value
		}
	}
}
