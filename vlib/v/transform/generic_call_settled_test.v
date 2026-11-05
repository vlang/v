module transform

import v.flat
import v.types

fn test_settled_calls_are_private_to_each_worker_view() {
	mut a := flat.FlatAst.new()
	base_id := a.add_node(flat.Node{ kind: .call })
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	assert isnil(master.generic_call_spec_settled)
	master.mark_generic_call_settled(int(base_id))
	mut view := master.fork_program_view(&a, &tc, map[string]bool{}, false)
	mut scan := master.fork_scan_worker(&tc)
	mut worker := master.fork_worker_with_state(&a, &tc, false, false)
	for fork in [&view, scan, worker] {
		assert isnil(fork.generic_call_spec_settled)
	}
	view.mark_generic_call_settled(101)
	scan.mark_generic_call_settled(102)
	worker.mark_generic_call_settled(103)
	assert master.generic_call_spec_settled.entries.keys() == [int(base_id)]
	assert view.generic_call_spec_settled.entries.keys() == [101]
	assert scan.generic_call_spec_settled.entries.keys() == [102]
	assert worker.generic_call_spec_settled.entries.keys() == [103]
}

fn test_worker_merge_relocates_settled_calls_without_sharing_storage() {
	mut a := flat.FlatAst.new()
	base_id := a.add_node(flat.Node{ kind: .call })
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	base_nodes := master.a.nodes.len
	base_children := master.a.children.len
	mut worker_ast := master.clone_ast_base(base_nodes, base_children)
	mut worker_tc := tc.fork_for_parallel_transform(worker_ast)
	mut worker := master.fork_worker(worker_ast, worker_tc)
	worker_id := worker_ast.add_node(flat.Node{ kind: .call, value: 'worker_call' })
	worker.mark_generic_call_settled(int(base_id))
	worker.mark_generic_call_settled(int(worker_id))
	worker.generic_call_spec_misses[int(worker_id)] = true
	master.a.add_node(flat.Node{ kind: .call, value: 'earlier_master_call' })
	shifted_id := master.a.nodes.len
	master.generic_call_spec_misses[shifted_id] = true
	master.merge_worker(worker, []FnWorkItem{}, base_nodes, base_children, false)
	assert master.a.nodes[shifted_id].value == 'worker_call'
	assert master.generic_call_spec_settled.entries[int(base_id)]
	assert master.generic_call_spec_settled.entries[shifted_id]
	assert !master.generic_call_spec_settled.entries[int(worker_id)]
	assert !master.generic_call_spec_misses[shifted_id]
	worker.generic_call_spec_settled.entries.delete(int(base_id))
	worker.generic_call_spec_settled.entries.delete(int(worker_id))
	assert master.generic_call_spec_settled.entries[int(base_id)]
	assert master.generic_call_spec_settled.entries[shifted_id]
}
