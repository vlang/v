module c

import v.flat
import v.token
import v.types

fn add_snapshot_query_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn grow_snapshot_query_arena(mut a flat.FlatAst) {
	for _ in 0 .. 2048 {
		a.add_val(.int_literal, '0')
	}
}

fn test_snapshot_closure_search_keeps_first_nested_match_after_arena_growth() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.ident, 'main.__anon_fn_first')
	second := a.add_val(.ident, 'main.__anon_fn_second')
	nested := add_snapshot_query_node(mut a, .paren, '', [first])
	root := add_snapshot_query_node(mut a, .call, '', [nested, second])
	snapshot := a.nodes[int(root)]
	grow_snapshot_query_arena(mut a)
	a.nodes[int(root)].children_count = 0
	mut g := FlatGen.new()
	g.a = &a
	assert g.runtime_closure_lifted_c_name(snapshot) or { '' } == g.cname('main.__anon_fn_first')
	assert g.runtime_closure_lifted_c_name(a.node(root)) == none
}

fn test_snapshot_const_selector_keeps_owning_file_import_and_local_shadow() {
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('owner.v', 1)
	base := a.add_val(.ident, 'dep')
	selector := add_snapshot_query_node(mut a, .selector, 'limit', [base])
	a.nodes[int(selector)].pos = token.new_pos(1, 0)
	snapshot := a.nodes[int(selector)]
	grow_snapshot_query_arena(mut a)
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.cur_file = 'other.v'
	tc.file_imports['owner.v\ndep'] = 'library'
	tc.file_imports['other.v\ndep'] = 'other'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['library.limit'] = flat.NodeId(0)
	g.const_vals['other.limit'] = flat.NodeId(0)
	assert g.const_ref_name_from_node(snapshot) == 'library.limit'
	g.cur_param_types['dep'] = &types.Type(types.int_)
	assert g.const_ref_name_from_node(snapshot) == ''
	mut cache := map[string]string{}
	assert g.const_ref_name_from_node_cached_for_collect(snapshot, map[string]string{}, mut cache) == ''
}

fn test_snapshot_source_file_walk_keeps_descendant_order_and_fallback() {
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('first.v', 1)
	a.source_files[2] = files.add_file('second.v', 1)
	first := a.add_node(flat.Node{ kind: .ident, pos: token.new_pos(1, 0) })
	second := a.add_node(flat.Node{ kind: .ident, pos: token.new_pos(2, 0) })
	nested := add_snapshot_query_node(mut a, .paren, '', [second])
	root := add_snapshot_query_node(mut a, .call, '', [first, nested])
	snapshot := a.nodes[int(root)]
	grow_snapshot_query_arena(mut a)
	a.nodes[int(root)].children_count = 0
	mut tc := types.TypeChecker.new(&a)
	tc.cur_file = 'checker.v'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cur_fn_source_file = 'function.v'
	assert g.node_source_file(&snapshot) == 'second.v'
	assert g.node_source_file(a.node(root)) == 'function.v'
}

fn test_fixed_size_query_keeps_annotation_and_rejects_ordinary_calls() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'values')
	selector := add_snapshot_query_node(mut a, .selector, 'to_fixed_size', [base])
	call := add_snapshot_query_node(mut a, .call, '', [selector])
	a.nodes[int(call)].typ = flat.source_type_id('[2]int')
	ordinary_callee := a.add_val(.ident, 'ordinary')
	ordinary := add_snapshot_query_node(mut a, .call, '', [ordinary_callee])
	grow_snapshot_query_arena(mut a)
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	fixed := g.to_fixed_size_call_fixed_type(call) or { panic('missing fixed array') }
	assert fixed.len == 2
	assert fixed.elem_type.name() == 'int'
	assert g.to_fixed_size_call_fixed_type(ordinary) == none
}
