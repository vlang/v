module transform

import v.flat
import v.types

fn test_scoped_overlay_names_survive_retiring_batch_storage() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	mut helper := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	parent_call := 'main.parent_call'.clone()
	parent_value := 'main.parent_value'.clone()
	scope := transform_worker_scope_begin(true)
	mut batch := helper.fork_scoped_batch_worker(&a, helper.tc.fork_for_parallel_transform(&a))
	batch.tc.fork_overlay.resolved_call_names[10] = parent_call
	batch.tc.fork_overlay.resolved_fn_values[11] = parent_value
	batch.tc.fork_overlay.resolved_call_names[12] = 'main.batch_call'.clone()
	batch.tc.fork_overlay.resolved_fn_values[13] = 'main.batch_value'.clone()
	transform_worker_scope_leave(scope)
	helper.absorb_scoped_batch(batch, scope, a.nodes.len)
	for _, name in helper.tc.fork_overlay.resolved_call_names {
		assert !transform_scope_owns(scope, name.str)
	}
	for _, name in helper.tc.fork_overlay.resolved_fn_values {
		assert !transform_scope_owns(scope, name.str)
	}
	transform_worker_scope_free(scope)
	assert helper.tc.fork_overlay.resolved_call_names[10] == parent_call
	assert helper.tc.fork_overlay.resolved_fn_values[11] == parent_value
	assert helper.tc.fork_overlay.resolved_call_names[12] == 'main.batch_call'
	assert helper.tc.fork_overlay.resolved_fn_values[13] == 'main.batch_value'
}

fn test_scoped_overlay_later_batch_replaces_existing_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	mut helper := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	helper.tc.fork_overlay.resolved_call_names[10] = 'main.old_call'
	helper.tc.fork_overlay.resolved_fn_values[11] = 'main.old_value'
	for i in 0 .. 3 {
		scope := transform_worker_scope_begin(true)
		mut batch := helper.fork_scoped_batch_worker(&a, helper.tc.fork_for_parallel_transform(&a))
		batch.tc.fork_overlay.resolved_call_names[10] = 'main.call_${i}'
		batch.tc.fork_overlay.resolved_fn_values[11] = 'main.value_${i}'
		transform_worker_scope_leave(scope)
		helper.absorb_scoped_batch(batch, scope, a.nodes.len)
		transform_worker_scope_free(scope)
		assert helper.tc.fork_overlay.resolved_call_names[10] == 'main.call_${i}'
		assert helper.tc.fork_overlay.resolved_fn_values[11] == 'main.value_${i}'
	}
}

fn test_scoped_overlay_names_publish_to_dense_and_sparse_master_caches() {
	for sparse in [false, true] {
		mut a := flat.FlatAst.new()
		for _ in 0 .. 4 {
			a.add_node(flat.Node{ kind: .ident })
		}
		mut tc := types.TypeChecker.new(&a)
		if sparse {
			tc.begin_sparse_transform_node_caches(0)
		}
		mut master := new_transformer(mut a, &tc, map[string]bool{})
		scope := transform_worker_scope_begin(true)
		mut batch := master.fork_scoped_batch_worker(&a, tc.fork_for_parallel_transform(&a))
		batch.tc.fork_overlay.resolved_call_names[2] = 'main.generated_call'.clone()
		batch.tc.fork_overlay.resolved_fn_values[3] = 'main.generated_value'.clone()
		transform_worker_scope_leave(scope)
		master.absorb_scoped_batch(batch, scope, a.nodes.len)
		transform_worker_scope_free(scope)
		assert tc.resolved_call_name(2)? == 'main.generated_call'
		assert tc.resolved_fn_value_name(3)? == 'main.generated_value'
	}
}
