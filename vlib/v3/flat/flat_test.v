module flat

fn discard_capacity_on_worker() bool {
	test_discard_unused_capacity_preserves_live_nodes_and_later_appends()
	$if prealloc {
		// Releasing the original arena must also work after its unused pages
		// have been replaced, including the root arena's libc-backed blocks.
		unsafe { prealloc_thread_cleanup() }
	}
	return true
}

fn test_discard_unused_capacity_allows_arena_teardown() {
	$if prealloc {
		handle := spawn discard_capacity_on_worker()
		assert handle.wait()
	}
}

fn test_discard_unused_capacity_preserves_live_nodes_and_later_appends() {
	mut a := FlatAst.new()
	// Dirty the full backing, then discard the former worker append regions.
	a.nodes = []Node{len: 50_000, init: Node{ kind: .ident, value: 'live', typ: 'int' }}
	a.children = []NodeId{len: 400_000, init: NodeId(17)}
	a.nodes.trim(73)
	a.children.trim(113)
	neighbor := []u8{len: 40_000, init: 0xab}
	node_data := a.nodes.data
	child_data := a.children.data
	node_cap := a.nodes.cap
	child_cap := a.children.cap
	unsafe { a.discard_unused_capacity() }
	assert a.nodes.len == 73
	assert a.children.len == 113
	for node in a.nodes {
		assert node.kind == .ident
		assert node.value == 'live'
		assert node.typ == 'int'
	}
	for child in a.children {
		assert child == NodeId(17)
	}
	for byte in neighbor {
		assert byte == 0xab
	}
	for i in 0 .. 30_000 {
		a.nodes << Node{ kind: .int_literal, value: '42' }
		a.children << NodeId(i)
	}
	assert a.nodes.data == node_data
	assert a.children.data == child_data
	assert a.nodes.cap == node_cap
	assert a.children.cap == child_cap
	for i in 0 .. 30_000 {
		assert a.nodes[73 + i].kind == .int_literal
		assert a.nodes[73 + i].value == '42'
		assert a.children[113 + i] == NodeId(i)
	}
}

fn test_node_kind_has_one_canonical_representation() {
	mut ast := FlatAst.new()
	id := ast.add_node(Node{
		kind: .call
	})
	assert ast.nodes[int(id)].kind == .call
}

fn test_child_count_supports_large_generated_nodes() {
	assert child_count(50_000) == 50_000
}

fn test_node_uses_compact_header_and_uncommon_payload() {
	empty := Node{}
	assert empty.generic_params().len == 0
	mut with_params := Node{}
	with_params.set_generic_params(['T', 'U'])
	assert with_params.generic_params() == ['T', 'U']
	assert sizeof(NodeKind) == 1
	assert sizeof(Op) == 1
	// The former always-present []string field made Node 96 bytes on 64-bit.
	// `int` is 64-bit now, so the two `string` headers alone are 48 bytes; the
	// rest of the header (payload pointer, child range, flags and Pos) is 40.
	assert sizeof(Node) <= 88
	assert sizeof(NodeId) == 4
}

fn test_node_owned_clone_preserves_semantic_flags_and_payload() {
	node := Node{
		value: 'value'
		typ: '[]string'
		payload: node_payload(['T'])
		children_start: 12
		children_count: 3
		kind: .for_stmt
		op: .plus
		is_mut: true
		flags: node_flags(true, true)
	}
	cloned := node.clone_owned()
	assert cloned.value == node.value
	assert cloned.typ == node.typ
	assert cloned.generic_params() == ['T']
	assert cloned.children_start == node.children_start
	assert cloned.children_count == node.children_count
	assert cloned.kind == node.kind
	assert cloned.op == node.op
	assert cloned.is_mut
	assert cloned.skip_ownership_drops()
	assert cloned.is_static_type_method()
}

fn test_static_type_method_name_round_trip_with_marker_in_both_parts() {
	encoded := encode_static_type_method_name('models.Cache__static__State', 'reset__static__now')
	assert encode_static_type_method_name('int', 'tag') != 'int__static__tag__static__3'
	receiver, method := decode_static_type_method_name(encoded) or { panic('invalid encoding') }
	assert receiver == 'models.Cache__static__State'
	assert method == 'reset__static__now'
	if _, _ := decode_static_type_method_name('cache__static__reset') {
		assert false
	}
}

fn test_clone_text_table_owned_detaches_scoped_storage() {
	$if prealloc {
		mut ast := FlatAst.new()
		scope := unsafe { prealloc_scope_begin() }
		for i in 0 .. 256 {
			ast.intern_text('scoped_text_${i}')
		}
		unsafe { prealloc_scope_leave(scope) }

		values, ids := ast.clone_text_table_owned()
		assert !unsafe { prealloc_scope_owns(scope, values.data) }
		for idx in [0, 128, 255] {
			assert unsafe { prealloc_scope_owns(scope, ast.text_values[idx].str) }
			assert !unsafe { prealloc_scope_owns(scope, values[idx].str) }
			assert ids[values[idx]] == TextId(idx + 1)
		}
		unsafe { prealloc_scope_free_after(scope) }

		assert values[0] == 'scoped_text_0'
		assert values[255] == 'scoped_text_255'
		assert ids['scoped_text_128'] == TextId(129)
	}
}

fn test_text_intern_passes_detach_reused_source_storage() {
	mut ast := FlatAst.new()
	mut source := []u8{len: 5}
	// Simulate parser scratch being reused between independent intern passes.
	borrowed := unsafe { tos(source.data, source.len) }
	names := ['alpha', 'bravo', 'cider']
	for i, name in names {
		for j in 0 .. source.len {
			source[j] = name[j]
		}
		ast.add_node(Node{
			value: borrowed
			typ: borrowed
			payload: node_payload([borrowed])
		})
		if i % 2 == 0 {
			ast.intern_node_texts_range(i, i + 1)
		} else {
			ast.intern_node_texts_at([i])
		}
	}
	for i, name in names {
		node := ast.nodes[i]
		assert node.value == name
		assert node.typ == name
		assert node.generic_params() == [name]
		assert ast.text(TextId(node.type_text_id())) == name
		assert node.value.str == node.typ.str
	}
}

fn test_promote_transform_texts_rebuilds_scoped_table_growth() {
	$if prealloc {
		mut ast := FlatAst.new()
		ast.reserve_transform_texts(4)
		scope := unsafe { prealloc_scope_begin() }
		for i in 0 .. 256 {
			ast.intern_text('promoted_text_${i}')
		}
		assert unsafe { prealloc_scope_owns(scope, ast.text_values.data) }
		unsafe { prealloc_scope_leave(scope) }

		ast.promote_transform_texts_from(0, scope)
		// After promotion the values array must be detached from the scope, and
		// the lookup map must remain usable once the scope is freed — which it
		// cannot be if any of the map's backing were still owned by the scope.
		assert !unsafe { prealloc_scope_owns(scope, ast.text_values.data) }
		for idx in [0, 128, 255] {
			assert !unsafe { prealloc_scope_owns(scope, ast.text_values[idx].str) }
		}
		unsafe { prealloc_scope_free_after(scope) }

		for idx in [0, 128, 255] {
			value := 'promoted_text_${idx}'
			id, canonical := ast.intern_text(value)
			assert id == TextId(idx + 1)
			assert canonical == value
		}
	}
}

fn test_ast_accessors_preserve_bounds_validation() {
	mut a := FlatAst.new()
	id := a.add_val(.ident, 'valid')
	a.children << id
	parent := Node{ children_count: 1 }
	assert a.child(&parent, 0) == id
	assert a.child_node(&parent, 0).value == 'valid'
	assert a.node(id).value == 'valid'
	assert a.child(&parent, -1) == empty_node
	assert a.child(&parent, 1) == empty_node
	assert a.child_node(&parent, -1).kind == .empty
	assert a.child_node(&parent, 1).kind == .empty
	assert a.node(empty_node).kind == .empty
	assert a.node(NodeId(a.nodes.len)).kind == .empty
	outside := Node{ children_start: 4, children_count: 1 }
	assert a.child(&outside, 0) == empty_node
	assert a.child_node(&outside, 0).kind == .empty
	a.children[0] = NodeId(a.nodes.len)
	assert a.child_node(&parent, 0).kind == .empty
}
