module transform

import v.flat
import v.types

fn test_helper_merge_releases_bookkeeping_and_preserves_published_text() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.begin_sparse_transform_node_caches(0)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.retain_worker_results = true
	master.used_fns_log_active = true
	mut helper := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	helper.merge_scratch_scope = transform_worker_scope_begin(true)
	scope := helper.merge_scratch_scope
	helper.used_fns['main.generated'.clone()] = true
	helper.sum_eq_types['main.Sum'.clone()] = SumEqRequest{
		sum_name: 'main.Sum'.clone()
		module: 'main'.clone()
		file: 'main.v'.clone()
		helper_module: 'main'.clone()
	}
	text := helper.promote_scoped_result_text('main.resolved'.clone())
	assert !transform_scope_owns(scope, text.str)
	helper.tc.fork_overlay.resolved_call_names[10] = text
	helper.generic_call_spec_cache[12] = GenericCallSpec{
		decl_key: 'main.generic'.clone()
		args: ['[]int'.clone()]
	}
	transform_worker_scope_leave(scope)
	master.merge_worker_used_fns(helper)
	assert !transform_scope_owns(scope, master.used_fns_log[0].str)
	assert !transform_scope_owns(scope, master.sum_eq_types['main.Sum'].sum_name.str)
	master.merge_worker(helper, []FnWorkItem{}, 0, 0, false)
	assert helper.merge_scratch_scope == unsafe { nil }
	assert master.used_fns_log == ['main.generated']
	assert master.sum_eq_types['main.Sum'].file == 'main.v'
	assert tc.sparse_resolved_call_names[10] == 'main.resolved'
	assert master.generic_call_spec_cache[12].decl_key == 'main.generic'
	assert master.generic_call_spec_cache[12].args == ['[]int']
}

fn test_transform_fork_reads_and_merges_source_fn_values() {
	mut a := flat.FlatAst.new()
	for _ in 0 .. 8 {
		a.add_node(flat.Node{
			kind: .ident
		})
	}
	mut tc := types.TypeChecker.new(&a)
	tc.begin_sparse_transform_node_caches(a.nodes.len)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.set_resolved_fn_value_entry(3, 'main.callback')
	master.set_resolved_fn_value_entry(4, 'main.stale')
	master.set_resolved_fn_value_entry(6, 'main.removed_by_master')
	mut helper := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	mut untouched := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	// The fork reads the master's source-node entries.
	assert helper.tc.resolved_fn_value_name(3)? == 'main.callback'
	// Its own clears and discoveries stay private until the merge.
	helper.tc.clear_resolved_fn_value(4)
	helper.set_resolved_fn_value_entry(5, 'main.discovered')
	assert helper.tc.resolved_fn_value_name(4) == none
	assert helper.tc.resolved_fn_value_name(5)? == 'main.discovered'
	assert tc.resolved_fn_value_name(4)? == 'main.stale'
	assert tc.resolved_fn_value_name(5) == none
	// A batch forked from the helper sees the helper's writes and clears.
	batch_tc := helper.tc.fork_for_parallel_transform(&a)
	assert batch_tc.resolved_fn_value_name(4) == none
	assert batch_tc.resolved_fn_value_name(5)? == 'main.discovered'
	// The master clears an entry after the forks took their snapshots.
	tc.clear_resolved_fn_value(6)
	master.merge_worker(helper, []FnWorkItem{}, 0, 0, false)
	assert tc.resolved_fn_value_name(3)? == 'main.callback'
	assert tc.resolved_fn_value_name(4) == none
	assert tc.resolved_fn_value_name(5)? == 'main.discovered'
	// An untouched fork replays nothing, so it cannot restore the stale entry.
	master.merge_worker(untouched, []FnWorkItem{}, 0, 0, false)
	assert tc.resolved_fn_value_name(6) == none
	assert tc.resolved_fn_value_name(4) == none
}
