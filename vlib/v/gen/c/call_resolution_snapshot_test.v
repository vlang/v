module c

import v.flat
import v.token
import v.types

fn test_call_resolution_snapshot_keeps_absent_and_present_empty_names() {
	for present in [false, true] {
		mut a := flat.FlatAst.new()
		callee := a.add_val(.ident, 'ordinary')
		a.children << callee
		call := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 1 })
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.fn_ret_types['ordinary'] = types.Type(types.void_)
		tc.resolved_call_names = []&types.CachedName{len: a.nodes.len}
		tc.resolved_call_set = []bool{len: a.nodes.len}
		if present {
			tc.resolved_call_names[int(call)] = types.cached_name('')
			tc.resolved_call_set[int(call)] = true
		}
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.gen_call(call, a.node(call))
		assert g.sb.str() == 'ordinary()'
		assert (tc.resolved_call_name(call) != none) == present
	}
}

fn test_nested_call_resolution_snapshots_keep_each_selector_target() {
	for sparse in [false, true] {
		mut a := flat.FlatAst.new()
		base := a.add_val(.ident, 'receiver')
		a.children << base
		inner_selector := a.add_node(flat.Node{ kind: .selector, value: 'inner', children_start: 0, children_count: 1 })
		a.children << inner_selector
		inner := a.add_node(flat.Node{ kind: .call, children_start: 1, children_count: 1 })
		a.children << base
		outer_selector := a.add_node(flat.Node{ kind: .selector, value: 'outer', children_start: 2, children_count: 1 })
		a.children << outer_selector
		a.children << inner
		outer := a.add_node(flat.Node{ kind: .call, children_start: 3, children_count: 2 })
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.cur_scope.insert('receiver', types.Type(types.int_))
		tc.fn_param_types['impl.inner'] = []types.Type{}
		tc.fn_param_types['impl.outer'] = [types.Type(types.int_)]
		tc.fn_ret_types['impl.inner'] = types.Type(types.int_)
		tc.fn_ret_types['impl.outer'] = types.Type(types.int_)
		tc.register_synth_type(inner, types.Type(types.int_))
		if sparse {
			tc.parallel_check_sparse = true
			tc.sparse_resolved_call_names[int(inner)] = 'impl.inner'
			tc.sparse_resolved_call_names[int(outer)] = 'impl.outer'
		} else {
			tc.resolved_call_names = []&types.CachedName{len: a.nodes.len}
			tc.resolved_call_set = []bool{len: a.nodes.len}
			tc.resolved_call_names[int(inner)] = types.cached_name('impl.inner')
			tc.resolved_call_names[int(outer)] = types.cached_name('impl.outer')
			tc.resolved_call_set[int(inner)] = true
			tc.resolved_call_set[int(outer)] = true
		}
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.gen_call(outer, a.node(outer))
		assert g.sb.str() == 'impl__outer(impl__inner())'
		assert tc.resolved_call_name(outer) or { '' } == 'impl.outer'
		assert tc.resolved_call_name(inner) or { '' } == 'impl.inner'
	}
}

fn test_script_local_call_keeps_checker_resolution_unchanged() {
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('commands.vsh', 1)
	callee := a.add_val(.ident, 'ordinary')
	a.children << callee
	call := a.add_node(flat.Node{ kind: .call, pos: token.new_pos(1, 0), children_start: 0, children_count: 1 })
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.cur_file = 'commands.vsh'
	tc.fn_type_files['ordinary'] = 'commands.vsh'
	tc.fn_ret_types['ordinary'] = types.Type(types.void_)
	tc.resolved_call_names = []&types.CachedName{len: a.nodes.len}
	tc.resolved_call_set = []bool{len: a.nodes.len}
	tc.resolved_call_names[int(call)] = types.cached_name('other.ordinary')
	tc.resolved_call_set[int(call)] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.gen_call(call, a.node(call))
	assert g.sb.str() == 'ordinary()'
	assert tc.resolved_call_name(call) or { '' } == 'other.ordinary'
}
