module c

import v.flat
import v.types

fn test_borrowed_call_helpers_keep_stack_snapshot_across_arena_growth_and_nested_calls() {
	mut a := flat.FlatAst.new()
	inner_callee := a.add_val(.ident, 'inner')
	a.children << inner_callee
	inner := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 1 })
	outer_callee := a.add_val(.ident, 'outer')
	a.children << outer_callee
	a.children << inner
	outer := a.add_node(flat.Node{ kind: .call, children_start: 1, children_count: 2 })
	call_snapshot := a.nodes[int(outer)]
	callee_snapshot := a.nodes[int(outer_callee)]
	for i in 0 .. 2048 {
		a.add_val(.ident, 'padding_${i}')
	}
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.fn_param_types['inner'] = []types.Type{}
	tc.fn_param_types['outer'] = [types.Type(types.int_)]
	tc.fn_ret_types['inner'] = types.Type(types.int_)
	tc.fn_ret_types['outer'] = types.Type(types.int_)
	tc.register_synth_type(inner, types.Type(types.int_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.direct_call_name_for_call_node(outer, &call_snapshot, 'outer') == 'outer'
	assert g.call_target_key(&callee_snapshot) == 'outer'
	assert !g.call_callee_is_module_selector(&call_snapshot)
	g.gen_call(outer, &call_snapshot)
	assert g.sb.str() == 'outer(inner())'
	assert call_snapshot == a.nodes[int(outer)]
	assert callee_snapshot == a.nodes[int(outer_callee)]
}

fn test_borrowed_runtime_map_call_uses_stack_child_snapshots() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'new_map')
	key_size := a.add_val(.sizeof_expr, 'int')
	value_size := a.add_val(.sizeof_expr, 'string')
	a.children << callee
	a.children << key_size
	a.children << value_size
	call := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 3 })
	call_snapshot := a.nodes[int(call)]
	callee_snapshot := a.nodes[int(callee)]
	for i in 0 .. 2048 {
		a.add_val(.ident, 'padding_${i}')
	}
	mut g := FlatGen.new()
	g.a = &a
	assert g.is_runtime_new_map_call(&call_snapshot)
	assert g.main_runtime_shadow_call_c_name(&call_snapshot, &callee_snapshot) == none
	assert call_snapshot == a.nodes[int(call)]
}
