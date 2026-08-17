module transform

import v3.flat
import v3.token
import v3.types

fn test_monomorph_job_count_does_not_start_empty_workers() {
	$if !v3_no_parallel ? {
		assert monomorph_job_count(16, 1) == 1
		assert monomorph_job_count(16, 3) == 3
		assert monomorph_job_count(2, 8) == 2
	}
}

fn test_generated_calls_publish_exact_resolution_except_cgen_intrinsics() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	call_id := t.make_call('main.helper', []flat.NodeId{})
	assert tc.resolved_call_name(call_id)? == 'main.helper'
	intrinsic_id := t.make_call('__v3_clone_owned_ierror', []flat.NodeId{})
	assert tc.resolved_call_name(intrinsic_id) == none
}

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

fn test_merge_worker_signatures_updates_checker_method_suffix_index() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})

	worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	mut worker_tc := tc.fork_for_parallel_transform(worker_ast)
	worker_tc.ensure_private_transform_signatures()
	worker_tc.fn_ret_types['widgets.Box.open'] = types.Type(types.bool_)
	params := [types.Type(types.int_)]
	worker_tc.register_generated_fn_param_types('widgets.Box.open', params)
	worker := master.fork_worker(worker_ast, worker_tc)

	assert 'widgets.Box.open' !in master.tc.fn_ret_types
	assert 'widgets.Box.open' !in master.tc.fn_param_types
	assert 'Box.open' !in master.tc.receiver_method_suffix_index
	assert worker.tc.receiver_method_suffix_index['Box.open'] == 'widgets.Box.open'
	master.merge_worker_signatures(worker)

	assert master.tc.fn_param_types_for_name('widgets.Box.open') == params
	assert master.tc.receiver_method_suffix_index['Box.open'] == 'widgets.Box.open'
	assert master.tc.fn_param_types_for_name('Box.open') == params
	assert master.tc.fn_param_types_for_name('open') == params
}

fn test_parallel_master_detaches_metadata_maps_before_writing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.fn_ret_types['main.base'] = 'int'
	tc.fn_param_types['main.base'] = [types.Type(types.int_)]
	master.structs['main.Base'] = StructInfo{
		name: 'main.Base'
	}
	tc.structs['main.Base'] = []types.StructField{}

	worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	worker_tc := tc.fork_for_parallel_transform(worker_ast)
	worker := master.fork_worker(worker_ast, worker_tc)
	master.mark_parallel_worker_maps_shared()

	master.set_fn_ret_type('main.master_generated', 'bool')
	mut master_tc := unsafe { &types.TypeChecker(voidptr(master.tc)) }
	master_tc.ensure_private_transform_signatures()
	master_tc.register_generated_fn_param_types('main.master_generated', [
		types.Type(types.bool_),
	])

	assert master.fn_ret_types['main.master_generated'] == 'bool'
	assert 'main.master_generated' !in worker.fn_ret_types
	assert 'main.master_generated' in master.tc.fn_param_types
	assert 'main.master_generated' !in worker.tc.fn_param_types

	master.add_fn_literal_capture_context('CaptureContext', 'main', []string{}, map[string]string{})
	assert 'CaptureContext' in master.structs
	assert 'CaptureContext' !in worker.structs
	assert 'CaptureContext' in master.tc.structs
	assert 'CaptureContext' !in worker.tc.structs
}

fn test_transform_ast_clone_preserves_template_metadata() {
	mut a := flat.FlatAst.new()
	a.template_call_sites[7] = token.new_pos(3, 11)
	a.template_actions[7] = 'render_page'
	mut tc := types.TypeChecker.new(&a)
	master := new_transformer(mut a, &tc, map[string]bool{})
	worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	assert worker_ast.template_call_sites[7] == master.a.template_call_sites[7]
	assert worker_ast.template_actions[7] == 'render_page'
}

fn test_transform_worker_records_struct_operators_in_private_map() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.used_struct_operator_fns['main.Box.+'] = true

	worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	worker_tc := tc.fork_for_parallel_transform(worker_ast)
	mut worker := master.fork_worker(worker_ast, worker_tc)
	worker.mark_struct_operator_used_name('main.Point.==')

	assert 'main.Point.==' !in master.used_struct_operator_fns
	master.merge_worker_used_fns(worker)
	assert master.used_struct_operator_fns['main.Box.+']
	assert master.used_struct_operator_fns['main.Point.==']
}

fn test_skipped_literal_decl_does_not_hide_later_closure() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{
		kind: .fn_literal
	})
	a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'dead'
	})
	a.add_node(flat.Node{
		kind: .fn_literal
	})
	main_idx := int(a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'main'
	}))
	helper_idx := int(a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'helper'
	}))
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, {
		'main':   true
		'helper': true
	})
	t.skip_generics = true
	t.transformed_fns = []bool{len: t.a.nodes.len}

	literal_decls := t.collect_literal_fn_decls(t.a.nodes.len)
	assert literal_decls == [1, main_idx]
	pure := t.transform_serial_then_collect_pure(literal_decls)
	assert t.transformed_fns[main_idx]
	assert !t.transformed_fns[helper_idx]
	assert pure.len == 1
	assert pure[0].fn_idx == helper_idx
}
