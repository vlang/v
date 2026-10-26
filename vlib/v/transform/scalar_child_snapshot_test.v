module transform

import v.flat
import v.types

fn test_scalar_child_lookup_preserves_empty_sentinels_and_snapshot_after_growth() {
	mut a := flat.FlatAst.new()
	child := a.add_node(flat.Node{ kind: .ident, value: 'saved' })
	a.children << child
	snapshot := flat.Node{ kind: .call, children_start: 0, children_count: 1 }
	for i in [-1, 0, 1] {
		assert a.node(a.child(&snapshot, i)).kind == a.child_node(&snapshot, i).kind
		assert a.node(a.child(&snapshot, i)).value == a.child_node(&snapshot, i).value
	}
	for start in [-1, a.children.len, a.children.len + 100] {
		invalid := flat.Node{ kind: .call, children_start: start, children_count: 1 }
		assert a.node(a.child(&invalid, 0)).kind == a.child_node(&invalid, 0).kind
	}
	for id in [flat.NodeId(-1), flat.NodeId(a.nodes.len + 100)] {
		start := a.children.len
		a.children << id
		invalid := flat.Node{ kind: .call, children_start: start, children_count: 1 }
		assert a.node(a.child(&invalid, 0)).kind == a.child_node(&invalid, 0).kind
	}
	count := a.nodes.cap + 100
	for _ in 0 .. count {
		a.add_node(flat.Node{ kind: .ident, value: 'growth' })
		a.children << flat.NodeId(-1)
	}
	assert a.node(a.child(&snapshot, 0)).value == 'saved'
	assert snapshot.children_count == 1
}

fn test_call_type_queries_use_stable_snapshot_after_ast_growth() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{ kind: .ident, value: 'known' })
	start := a.children.len
	a.children << callee
	call := a.add_node(flat.Node{ kind: .call, children_start: start, children_count: 1 })
	snapshot := a.nodes[int(call)]
	mut tc := types.TypeChecker.new(&a)
	tc.fn_ret_types['known'] = types.Type(types.bool_)
	tc.fn_param_types['known'] = [types.Type(types.String{})]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	count := a.nodes.cap + 100
	for _ in 0 .. count {
		a.add_node(flat.Node{ kind: .ident, value: 'growth' })
		a.children << flat.NodeId(-1)
	}
	assert t.get_call_return_type(call, snapshot) == flat.source_type_bool
	params := t.call_param_types_for_node('known', snapshot)
	assert params.len == 1
	assert params[0] is types.String
}
