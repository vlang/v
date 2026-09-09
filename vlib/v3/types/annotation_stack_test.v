module types

import v3.flat

fn test_annotation_walk_handles_deep_single_child_ast_iteratively() {
	mut a := flat.FlatAst.new()
	mut current := a.add_node(flat.Node{
		kind:  .int_literal
		value: '1'
	})
	for _ in 0 .. 20_000 {
		children_start := a.begin_children()
		a.add_child(current)
		current = a.add_node(flat.Node{
			kind:           .paren
			children_start: children_start
			children_count: 1
		})
	}

	mut tc := TypeChecker.new(&a)
	tc.extend_node_caches(a.nodes.len)
	mut memo := &BodyResolveMemo{}
	memo.begin(0, a.nodes.len - 1)
	for i in 0 .. a.nodes.len {
		memo.types[i] = Type(int_)
		memo.filled[i] = 1
	}
	tc.body_resolve_memo = memo
	tc.annotate_node(current)
	assert true
}

fn test_annotation_walk_preserves_pending_siblings_after_call() {
	mut a := flat.FlatAst.new()
	first := a.add_node(flat.Node{
		kind: .call
	})
	sibling := a.add_node(flat.Node{
		kind: .paren
	})
	children_start := a.begin_children()
	a.add_child(first)
	a.add_child(sibling)
	root := a.add_node(flat.Node{
		kind:           .infix
		op:             .plus
		children_start: children_start
		children_count: 2
	})

	mut tc := TypeChecker.new(&a)
	tc.extend_node_caches(a.nodes.len)
	mut memo := &BodyResolveMemo{}
	memo.begin(0, a.nodes.len - 1)
	for i in 0 .. a.nodes.len {
		memo.types[i] = Type(int_)
		memo.filled[i] = 1
	}
	tc.body_resolve_memo = memo
	tc.annotate_node(root)
	assert tc.expr_type_set[int(sibling)]
}
