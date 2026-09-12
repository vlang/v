module transform

import v3.flat
import v3.types

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
