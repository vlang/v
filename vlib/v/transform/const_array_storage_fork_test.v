module transform

import v.flat
import v.types

fn test_lazy_const_array_storage_lookup_does_not_escape_worker_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	// Allocate the parent's buckets before the fork: copying an empty map would
	// let a first worker insertion allocate independent buckets by accident.
	assert !master.const_array_literal_requires_fixed_storage('main.parent')
	scope := transform_worker_scope_begin(true)
	mut worker := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	assert !worker.const_array_literal_requires_fixed_storage('main.worker_only'.clone())
	transform_worker_scope_leave(scope)
	defer {
		transform_worker_scope_free(scope)
	}
	assert 'main.worker_only' !in master.const_array_fixed_storage_cache
	assert !master.const_array_literal_requires_fixed_storage('main.parent')
}

fn test_ready_const_array_storage_lookup_keeps_shared_cache_unchanged() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.int_literal, '7')
	array_children := a.begin_children()
	a.add_child(value)
	literal := a.add_node(flat.Node{
		kind:           .array_literal
		children_start: array_children
		children_count: 1
	})
	base := a.add_val(.ident, 'ready')
	index_value := a.add_val(.int_literal, '0')
	index_children := a.begin_children()
	a.add_child(base)
	a.add_child(index_value)
	a.add_node(flat.Node{
		kind:           .index
		children_start: index_children
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.const_exprs['ready'] = literal
	tc.const_types['ready'] = types.Array{ elem_type: types.int_ }
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.precompute_const_array_fixed_storage()
	assert master.const_array_literal_requires_fixed_storage('ready')
	mut worker := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	assert worker.const_array_literal_requires_fixed_storage('ready')
	assert !worker.const_array_literal_requires_fixed_storage('main.missing')
	assert 'main.missing' !in worker.const_array_fixed_storage_cache
	assert 'main.missing' !in master.const_array_fixed_storage_cache
	assert master.const_array_fixed_storage_cache.len == 1
	assert worker.const_array_fixed_storage_cache.len == 1
	assert master.const_array_literal_requires_fixed_storage('ready')
}
