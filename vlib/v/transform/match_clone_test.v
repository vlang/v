module transform

import v.flat
import v.types

fn test_match_variant_clones_remain_independent_across_storage_growth() {
	mut a := flat.FlatAst.new()
	leaf := a.add_node(flat.Node{
		kind:  .sizeof_expr
		value: 'subject'
		flags: 3
	})
	a.children << leaf
	branch := a.add_node(flat.Node{
		kind:           .block
		children_count: 1
	})
	start := a.children.len
	for _ in 0 .. 257 {
		a.children << branch
	}
	root := a.add_node(flat.Node{
		kind:           .block
		children_start: start
		children_count: 257
	})
	tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	first := t.instantiate_match_variant_body(root, 'subject', '', 'First', true)
	second := t.instantiate_match_variant_body(root, 'subject', '', 'Second', true)
	roots := [root, first, second]
	expected := ['subject', 'First', 'Second']
	mut seen := map[flat.NodeId]bool{}
	for i, id in roots {
		for child in a.children_of(a.node(id)) {
			copied := a.child(a.node(child), 0)
			assert a.node(copied).value == expected[i]
			assert a.node(copied).flags == 3
			if i == 0 {
				continue
			}
			assert copied !in seen
			seen[copied] = true
		}
	}
	assert a.node(leaf).typ == ''
	node_count := a.nodes.len
	child_count := a.children.len
	last := t.instantiate_match_variant_body(root, 'subject', '', 'Last', false)
	assert last == root
	assert a.nodes.len == node_count
	assert a.children.len == child_count
	assert a.node(leaf).value == 'Last'
	assert a.node(leaf).typ == 'usize'
	first_branch := a.child_node(a.node(first), 0)
	second_branch := a.child_node(a.node(second), 0)
	assert a.child_node(first_branch, 0).value == 'First'
	assert a.child_node(second_branch, 0).value == 'Second'
}
