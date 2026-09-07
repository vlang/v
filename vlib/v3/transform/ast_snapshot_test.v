module transform

import v3.flat
import v3.types

fn test_ast_snapshot_is_independent_and_can_grow() {
	$if macos {
		mut a := flat.FlatAst.new()
		for _ in 0 .. 2048 {
			id := a.add_node(flat.Node{ kind: .int_literal, value: 'source' })
			a.add_child(id)
		}
		mut tc := types.TypeChecker.new(&a)
		t := new_transformer(mut a, &tc, map[string]bool{})
		mut first, first_storage := t.snapshot_ast_base(a.nodes.len, a.children.len) or {
			panic('failed to snapshot AST')
		}
		mut second, second_storage := t.snapshot_ast_base(a.nodes.len, a.children.len) or {
			panic('failed to snapshot AST')
		}
		defer {
			release_transform_clone_storage(&first_storage)
			release_transform_clone_storage(&second_storage)
		}
		a.nodes[0].value = 'master'
		first.nodes[1].value = 'first'
		second.nodes[2].value = 'second'
		a.children[0] = flat.NodeId(3)
		first.children[1] = flat.NodeId(4)
		second.children[2] = flat.NodeId(5)
		assert first.nodes[0].value == 'source'
		assert second.nodes[0].value == 'source'
		assert a.nodes[1].value == 'source'
		assert second.nodes[1].value == 'source'
		assert a.nodes[2].value == 'source'
		assert first.nodes[2].value == 'source'
		assert first.children[0] == flat.NodeId(0)
		assert a.children[1] == flat.NodeId(1)
		assert second.children[1] == flat.NodeId(1)
		assert first.children[2] == flat.NodeId(2)
		// Appends first use the private mapping, then grow into ordinary array
		// storage. Neither path may change another view or free the mapping.
		for _ in 0 .. 4096 {
			id := first.add_node(flat.Node{ kind: .int_literal, value: 'appended' })
			first.add_child(id)
		}
		assert first.nodes.len == 6144
		assert first.nodes[1].value == 'first'
		assert first.nodes.last().value == 'appended'
		assert first.children.last() == flat.NodeId(6143)
		assert a.nodes.len == 2048
		assert second.nodes.len == 2048
		assert second.nodes[2].value == 'second'
	}
}

fn test_ast_snapshot_rejects_empty_or_inverted_ranges() {
	assert snapshot_ast_buffer(unsafe { nil }, 0, 0) == none
	value := u64(0)
	assert snapshot_ast_buffer(&value, 8, 4) == none
}
