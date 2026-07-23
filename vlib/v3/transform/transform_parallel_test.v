module transform

import v3.flat
import v3.types

fn test_deferred_worker_node_clone_preserves_skip_ownership_drops() {
	$if !v3_no_parallel ? {
		mut t := Transformer{
			deferred_base_writes:  [
				DeferredBaseWrite{
					idx:  7
					kind: 2
					node: flat.Node{
						kind:                 .for_stmt
						skip_ownership_drops: true
					}
				},
			]
			scoped_promoted_texts: map[string]string{}
		}
		t.clone_deferred_worker_writes_from(0)
		cloned := t.deferred_base_writes[0].node
		assert cloned.kind == .for_stmt
		assert cloned.skip_ownership_drops
	}
}

fn test_merge_worker_shifts_private_specialization_metadata() {
	mut a := flat.FlatAst.new()
	base_id := a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'base_specialization'
	})
	a.specialized_fn_nodes[int(base_id)] = true
	a.specialized_fn_modules[int(base_id)] = 'base_module'
	a.specialized_fn_files[int(base_id)] = 'base.v'
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	base_nodes := master.a.nodes.len
	base_children := master.a.children.len

	worker_ast := master.clone_ast_base(base_nodes, base_children)
	worker_tc := tc.fork_for_parallel_transform(worker_ast)
	mut worker := master.fork_worker(worker_ast, worker_tc)
	assert worker.a.specialized_fn_modules[int(base_id)] == 'base_module'
	worker_id := worker.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'worker_specialization'
	})
	worker.a.specialized_fn_nodes[int(worker_id)] = true
	worker.a.specialized_fn_modules[int(worker_id)] = 'worker_module'
	worker.a.specialized_fn_files[int(worker_id)] = 'worker.v'
	assert int(worker_id) == base_nodes
	assert int(worker_id) !in master.a.specialized_fn_nodes

	master.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'earlier_master_append'
	})
	shifted_id := master.a.nodes.len
	master.merge_worker(worker, []FnWorkItem{}, base_nodes, base_children, false)

	assert master.a.nodes[shifted_id].value == 'worker_specialization'
	assert master.a.specialized_fn_nodes[shifted_id]
	assert master.a.specialized_fn_modules[shifted_id] == 'worker_module'
	assert master.a.specialized_fn_files[shifted_id] == 'worker.v'
	assert int(worker_id) !in master.a.specialized_fn_nodes
	assert master.a.specialized_fn_modules[int(base_id)] == 'base_module'
	assert master.a.specialized_fn_files[int(base_id)] == 'base.v'
}
