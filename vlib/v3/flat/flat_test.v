module flat

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
	assert sizeof(Node) <= 72
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
