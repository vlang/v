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
		sum_name:      'main.Sum'.clone()
		module:        'main'.clone()
		file:          'main.v'.clone()
		helper_module: 'main'.clone()
	}
	helper.sum_eq_types['main.Node'.clone()] = SumEqRequest{
		struct_name:   'main.Node'.clone()
		module:        'main'.clone()
		file:          'main.v'.clone()
		helper_module: 'main'.clone()
	}
	text := helper.promote_scoped_result_text('main.resolved'.clone())
	assert !transform_scope_owns(scope, text.str)
	helper.tc.fork_overlay.resolved_call_names[10] = text
	helper.generic_call_spec_cache[12] = GenericCallSpec{
		decl_key: 'main.generic'.clone()
		args:     ['[]int'.clone()]
	}
	transform_worker_scope_leave(scope)
	master.merge_worker_used_fns(helper)
	assert !transform_scope_owns(scope, master.used_fns_log[0].str)
	assert !transform_scope_owns(scope, master.sum_eq_types['main.Sum'].sum_name.str)
	assert !transform_scope_owns(scope, master.sum_eq_types['main.Node'].struct_name.str)
	master.merge_worker(helper, []FnWorkItem{}, 0, 0, false)
	assert helper.merge_scratch_scope == unsafe { nil }
	assert master.used_fns_log == ['main.generated']
	assert master.sum_eq_types['main.Sum'].file == 'main.v'
	assert master.sum_eq_types['main.Node'].struct_name == 'main.Node'
	assert tc.sparse_resolved_call_names[10] == 'main.resolved'
	assert master.generic_call_spec_cache[12].decl_key == 'main.generic'
	assert master.generic_call_spec_cache[12].args == ['[]int']
}

fn test_scoped_monomorph_specialization_args_are_deep_cloned() {
	scope := transform_worker_scope_begin(true)
	scoped_args := ['cloud.Body'.clone(), '[]string'.clone()]
	transform_worker_scope_leave(scope)

	owned_args := clone_monomorph_specialization_args(scoped_args)
	assert owned_args == ['cloud.Body', '[]string']
	for arg in owned_args {
		assert !transform_scope_owns(scope, arg.str)
	}
	transform_worker_scope_free(scope)
	assert owned_args == ['cloud.Body', '[]string']
}

fn test_scoped_batch_specialization_names_outlive_scratch_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.a.specialized_fn_modules[3] = 'builtin'
	t.a.specialized_fn_files[3] = 'builtin.v'
	nodes_len := t.a.specialized_fn_nodes.len
	modules_len := t.a.specialized_fn_modules.len
	files_len := t.a.specialized_fn_files.len

	// A scoped batch records the module/file names of the functions it
	// specializes, and those names can live in the batch's scratch arena
	// (vlang/v#28897).
	scope := transform_worker_scope_begin(true)
	t.a.specialized_fn_nodes[7] = true
	t.a.specialized_fn_modules[7] = 'main'.clone()
	t.a.specialized_fn_files[7] = 'main.v'.clone()
	transform_worker_scope_leave(scope)

	t.promote_scoped_specialization_maps(scope, nodes_len, modules_len, files_len)
	assert !transform_scope_owns(scope, t.a.specialized_fn_modules[7].str)
	assert !transform_scope_owns(scope, t.a.specialized_fn_files[7].str)
	transform_worker_scope_free(scope)
	assert t.a.specialized_fn_nodes[7]
	assert t.a.specialized_fn_modules[7] == 'main'
	assert t.a.specialized_fn_files[7] == 'main.v'
	assert t.a.specialized_fn_modules[3] == 'builtin'
	assert t.a.specialized_fn_files[3] == 'builtin.v'
}

fn test_generic_unresolved_cache_owns_scoped_module() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.generic_unresolved_cache = &GenericUnresolvedCache{}

	scope := transform_worker_scope_begin(true)
	scoped_module := 'cloud'.clone()
	transform_worker_scope_leave(scope)
	t.cur_module = scoped_module
	assert !t.generic_arg_is_unresolved('int')
	assert !transform_scope_owns(scope, t.generic_unresolved_cache.module.str)

	t.cur_module = 'main'
	transform_worker_scope_free(scope)
	assert !t.generic_arg_is_unresolved('int')
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

fn test_serial_scoped_monomorphization_preserves_lifted_callback_signatures() {
	mut a := flat.FlatAst.new()
	param_id := a.add_node(flat.Node{
		kind:  .param
		value: 'item'
		typ:   'T'
	})
	item_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'item'
		typ:   'T'
	})
	return_children := a.begin_children()
	a.add_child(item_id)
	return_id := a.add_node(flat.Node{
		kind:           .return_stmt
		typ:            'T'
		children_start: return_children
		children_count: 1
	})
	literal_children := a.begin_children()
	a.add_child(param_id)
	a.add_child(return_id)
	literal_id := a.add_node(flat.Node{
		kind:           .fn_literal
		typ:            'T'
		children_start: literal_children
		children_count: 2
	})
	outer_return_children := a.begin_children()
	a.add_child(literal_id)
	outer_return_id := a.add_node(flat.Node{
		kind:           .return_stmt
		typ:            'fn (T) T'
		children_start: outer_return_children
		children_count: 1
	})
	fn_children := a.begin_children()
	a.add_child(outer_return_id)
	mut fn_node := flat.Node{
		kind:           .fn_decl
		value:          'make_callback'
		typ:            'fn (T) T'
		children_start: fn_children
		children_count: 1
	}
	fn_node.set_generic_params(['T'])
	fn_id := a.add_node(fn_node)
	decl := GenericFnDecl{
		id:     fn_id
		node:   fn_node
		file:   'callbacks.v'
		module: 'callbacks'
		key:    'callbacks.make_callback'
	}
	mut tc := types.TypeChecker.new(&a)
	tc.begin_sparse_transform_node_caches(a.nodes.len)
	tc.file_modules['callbacks.v'] = 'callbacks'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare()
	t.generic_fn_decls_cache[decl.key] = decl
	t.generic_fn_decls_ready = true
	t.parallel_monomorphize = false
	t.scope_parallel_workers = true
	t.scoped_monomorphize = true
	t.defer_nested_generic_emissions = true
	for arg in ['int', 'string'] {
		t.request_generic_fn_specialization(decl, [arg])
	}
	mut emitted := map[string]bool{}
	mut generated := []string{}
	assert t.drain_pending_generic_fn_specs(map[string]GenericStructDecl{}, map[string]GenericSumDecl{}, mut emitted, mut generated)
	assert isnil(a.worker_pool)
	assert t.pending_generic_fn_specs.len == 0
	for arg in ['int', 'string'] {
		spec_key := generic_fn_spec_key(decl.key, [arg])
		assert emitted[spec_key]
		root := t.generic_fn_spec_nodes[spec_key] or { panic('missing ${spec_key}') }
		name := transform_qualified_fn_name(decl.module, a.node(root).value)
		return_type := tc.fn_ret_types[name] or { panic('missing signature ${name}') }
		assert return_type is types.FnType
		assert (return_type as types.FnType).return_type.name() == arg
		assert (return_type as types.FnType).params[0].name() == arg
	}
	mut lifted := 0
	for idx in 0 .. a.nodes.len {
		node := a.node(flat.NodeId(idx))
		if node.kind != .fn_decl || !node.value.starts_with('__anon_fn_') {
			continue
		}
		lifted++
		assert node.typ in ['int', 'string']
		for name in [node.value, transform_qualified_fn_name(decl.module, node.value)] {
			assert t.fn_ret_types[name] == node.typ
			assert (tc.fn_ret_types[name] or { panic('missing lifted return ${name}') }).name() == node.typ
			params := tc.fn_param_types[name] or { panic('missing lifted params ${name}') }
			assert params.len == 1
			assert params[0].name() == node.typ
		}
	}
	assert lifted == 2
}

fn test_scoped_worker_keeps_extended_call_param_index_private() {
	mut a := flat.FlatAst.new()
	fn_id := a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'source'
	})
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.prepare_parallel_call_param_types()
	index_count := master.call_param_types_decl_index.len
	scope := transform_worker_scope_begin(true)
	mut worker := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	worker.add_call_param_types_decl_key('scope.only'.clone(), int(fn_id), 'scope.v'.clone(), 'scope'.clone())
	assert 'scope.only' in worker.call_param_types_decl_index
	transform_worker_scope_leave(scope)
	assert 'scope.only' !in master.call_param_types_decl_index
	assert 'scope__only' !in master.call_param_types_decl_index
	assert master.call_param_types_decl_index.len == index_count
	transform_worker_scope_free(scope)
	assert (master.call_param_types_from_decl('source') or { panic('missing source') }).len == 0
}
