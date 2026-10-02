module transform

import v.flat
import v.types

fn remaining_match_walk_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: start
		children_count: children.len
	})
}

fn remaining_match_walk_match(mut a flat.FlatAst, body []flat.NodeId) flat.NodeId {
	subject := a.add_val(.int_literal, '1')
	branch := remaining_match_walk_node(mut a, .match_branch, 'else', body)
	return remaining_match_walk_node(mut a, .match_stmt, '', [subject, branch])
}

fn remaining_match_walk_reachable_values(a &flat.FlatAst, root flat.NodeId) []string {
	mut values := []string{}
	mut seen := map[int]bool{}
	mut pending := [root]
	for pending.len > 0 {
		id := pending.pop()
		if seen[int(id)] {
			continue
		}
		seen[int(id)] = true
		node := a.node(id)
		assert node.kind != .match_stmt
		if node.kind == .int_literal {
			values << node.value
		}
		for child in a.children_of(node) {
			pending << child
		}
	}
	return values
}

fn test_remaining_match_walk_lowers_siblings_and_nested_matches_after_arena_growth() {
	mut a := flat.FlatAst.new()
	inner_value := a.add_val(.int_literal, '7')
	inner := remaining_match_walk_match(mut a, [inner_value])
	first := remaining_match_walk_match(mut a, [inner])
	second_value := a.add_val(.int_literal, '9')
	second := remaining_match_walk_match(mut a, [second_value])
	root := remaining_match_walk_node(mut a, .block, '', [first, second])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	initial_nodes := a.nodes.len
	mut visited := []u32{len: initial_nodes}
	t.lower_remaining_match_subtree(root, mut visited, 1)
	assert a.nodes.len > initial_nodes
	assert a.node(first).kind != .match_stmt
	assert a.node(second).kind != .match_stmt
	values := remaining_match_walk_reachable_values(&a, root)
	assert '7' in values
	assert '9' in values
}

fn test_remaining_match_walk_revisits_shared_nodes_in_the_next_function_epoch() {
	mut a := flat.FlatAst.new()
	leaf := a.add_val(.int_literal, '3')
	shared := remaining_match_walk_node(mut a, .block, '', [leaf])
	root := remaining_match_walk_node(mut a, .block, '', [shared, shared])
	a.nodes[int(root)].children_count++
	a.add_child(root)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	initial_nodes := a.nodes.len
	mut visited := []u32{len: initial_nodes}
	t.lower_remaining_match_subtree(root, mut visited, 1)
	assert a.nodes.len == initial_nodes
	value := a.add_val(.int_literal, '11')
	late_match := remaining_match_walk_match(mut a, [value])
	a.nodes[int(shared)].children_start = a.children.len
	a.nodes[int(shared)].children_count = 1
	a.add_child(late_match)
	t.lower_remaining_match_subtree(root, mut visited, 2)
	assert a.node(late_match).kind != .match_stmt
	values := remaining_match_walk_reachable_values(&a, root)
	assert '11' in values
}
