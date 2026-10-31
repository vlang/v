module types

import v.flat

fn test_fn_value_filter_snapshot_hits_misses_collisions_and_empty_names() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_resolved_fn_value(3, 'main.callback')
	tc.set_resolved_fn_value(5, '')
	forked := tc.fork_for_parallel_codegen()
	assert forked.sparse_fn_value_filter_ready
	assert forked.resolved_fn_value_name(3)? == 'main.callback'
	assert forked.resolved_fn_value_name(5)? == ''
	assert forked.resolved_fn_value_name(4) == none
	// 258 hashes to the same bit as 3; the final map lookup rejects it.
	assert sparse_fn_value_filter_bit(258) == sparse_fn_value_filter_bit(3)
	assert forked.resolved_fn_value_name(258) == none
	assert forked.resolved_fn_value_name(-1) == none
}

fn test_fn_value_filter_stays_disabled_on_mutable_master_direct_writes() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert !tc.sparse_fn_value_filter_ready
	tc.sparse_resolved_fn_values[3] = 'main.first'
	assert tc.resolved_fn_value_name(3)? == 'main.first'
	tc.sparse_resolved_fn_values.delete(3)
	tc.sparse_resolved_fn_values[4] = 'main.replacement'
	assert tc.resolved_fn_value_name(3) == none
	assert tc.resolved_fn_value_name(4)? == 'main.replacement'
}

fn test_fn_value_filter_codegen_set_clear_and_reset_keep_exact_membership() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_resolved_fn_value(3, 'main.original')
	mut forked := tc.fork_for_parallel_codegen()
	assert forked.resolved_fn_value_name(4) == none
	forked.set_resolved_fn_value(4, 'main.later')
	assert forked.resolved_fn_value_name(4)? == 'main.later'
	forked.clear_resolved_fn_value(4)
	assert forked.resolved_fn_value_name(4) == none
	forked.set_resolved_fn_value(4, 'main.again')
	assert forked.resolved_fn_value_name(4)? == 'main.again'
	forked.reset_sparse_fn_values()
	assert !forked.sparse_fn_value_filter_ready
	assert forked.resolved_fn_value_name(3) == none
	assert forked.resolved_fn_value_name(4) == none
	forked.sparse_resolved_fn_values[7] = 'main.new_ast'
	assert forked.resolved_fn_value_name(7)? == 'main.new_ast'
	assert tc.resolved_fn_value_name(3)? == 'main.original'
	assert tc.resolved_fn_value_name(4) == none
}

fn test_fn_value_filter_transform_writes_tombstones_and_nested_snapshots() {
	mut a := flat.FlatAst.new()
	for _ in 0 .. 8 {
		a.add(.ident)
	}
	mut tc := TypeChecker.new(&a)
	tc.set_resolved_fn_value(3, 'main.original')
	mut forked := tc.fork_for_parallel_transform(&a)
	assert forked.sparse_fn_value_filter_ready
	assert forked.resolved_fn_value_name(3)? == 'main.original'
	forked.set_resolved_fn_value(4, 'main.private')
	assert forked.resolved_fn_value_name(4)? == 'main.private'
	forked.clear_resolved_fn_value(3)
	assert forked.resolved_fn_value_name(3) == none
	// The nested snapshot must include private writes and omit tombstones,
	// even when the parent's snapshot filter did not contain the new key.
	nested := forked.fork_for_parallel_transform(&a)
	assert nested.sparse_fn_value_filter_ready
	assert nested.resolved_fn_value_name(3) == none
	assert nested.resolved_fn_value_name(4)? == 'main.private'
	forked.fork_overlay.resolved_fn_values[9] = 'main.generated'
	assert forked.resolved_fn_value_name(9)? == 'main.generated'
	assert tc.resolved_fn_value_name(3)? == 'main.original'
	assert tc.resolved_fn_value_name(4) == none
}

fn test_fn_value_filter_parallel_worker_merge_records_new_snapshot_keys() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	mut forked := tc.fork_for_parallel_codegen()
	mut worker := tc.fork_for_parallel_check()
	assert !worker.sparse_fn_value_filter_ready
	worker.sparse_resolved_fn_values[7] = 'main.worker'
	forked.merge_parallel_check_worker_scoped(worker, true)
	assert forked.sparse_fn_value_filter_ready
	assert forked.resolved_fn_value_name(7)? == 'main.worker'
}

fn test_fn_value_filter_storage_clones_and_nested_observers_stay_unfiltered() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_resolved_fn_value(3, 'main.original')
	forked := tc.fork_for_parallel_codegen()
	mut exact := forked.fork_storage_query_view()
	assert !exact.sparse_fn_value_filter_ready
	exact.sparse_resolved_fn_values.delete(3)
	exact.sparse_resolved_fn_values[4] = 'main.private_exact'
	assert exact.resolved_fn_value_name(3) == none
	assert exact.resolved_fn_value_name(4)? == 'main.private_exact'
	mut outer := forked.fork_storage_observation_view()
	assert !outer.sparse_fn_value_filter_ready
	assert outer.resolved_fn_value_name(3)? == 'main.original'
	outer.sparse_resolved_fn_values[4] = 'main.private_observer'
	assert outer.resolved_fn_value_name(4)? == 'main.private_observer'
	mut nested := outer.fork_storage_observation_view()
	assert !nested.sparse_fn_value_filter_ready
	assert nested.resolved_fn_value_name(3)? == 'main.original'
	assert nested.resolved_fn_value_name(4)? == 'main.private_observer'
	nested.clear_resolved_fn_value(3)
	assert nested.resolved_fn_value_name(3) == none
	assert outer.resolved_fn_value_name(3)? == 'main.original'
	assert forked.resolved_fn_value_name(4) == none
}
