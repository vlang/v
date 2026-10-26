module transform

import os
import v.flat
import v.types

fn test_clone_scoped_worker_node_publishes_every_payload_kind() {
	$if prealloc {
		mut a := flat.FlatAst.new()
		for kind in [flat.NodeKind.call, .debugger_stmt, .enum_decl, .ident, .ident, .ident, .fn_decl,
			.fn_decl] {
			a.add_node(flat.Node{ kind: kind })
		}
		mut tc := types.TypeChecker.new(&a)
		mut master := new_transformer(mut a, &tc, map[string]bool{})
		type_id := flat.source_type_id('string')
		constraint_id := flat.source_type_id('Named')
		scope := transform_worker_scope_begin(true)
		a.nodes[0].set_call_type_args([type_id])
		a.nodes[1].set_debugger_value_type_ids([type_id])
		a.nodes[2].set_enum_backing_type_id(type_id)
		a.nodes[3].set_fn_value_type_id(type_id)
		a.nodes[4].set_generated_variant_type_id(type_id)
		a.nodes[5].set_mut_optional_arg_view_type_id(type_id)
		a.nodes[6].set_generic_params_and_constraint_ids(['T'.clone()], [constraint_id])
		a.nodes[6].set_call_type_args([type_id])
		a.nodes[6].set_debugger_value_type_ids([type_id])
		a.nodes[6].set_enum_backing_type_id(type_id)
		a.nodes[6].set_fn_value_type_id(type_id)
		a.nodes[6].set_generated_variant_type_id(type_id)
		a.nodes[6].set_mut_optional_arg_view_type_id(type_id)
		a.nodes[7].set_generic_params_and_constraint_ids([]string{}, [constraint_id])
		transform_worker_scope_leave(scope)
		for idx in 0 .. a.nodes.len {
			assert transform_scope_owns(scope, a.nodes[idx].payload_ptr())
			master.clone_scoped_worker_node(idx, scope)
			payload := a.nodes[idx].payload_ptr()
			assert !transform_scope_owns(scope, payload), 'payload ${idx}'
			assert !transform_scope_owns(scope, payload.generic_params.data)
			for param in payload.generic_params {
				assert !transform_scope_owns(scope, param.str)
			}
			assert !transform_scope_owns(scope, payload.generic_constraints.ids.data)
			assert !transform_scope_owns(scope, payload.call_type_args.ids.data)
			assert !transform_scope_owns(scope, payload.debugger_value_types.ids.data)
		}
		transform_worker_scope_free(scope)
		assert a.nodes[0].call_type_args() == [type_id]
		assert a.nodes[1].debugger_value_type_ids() == [type_id]
		assert a.nodes[2].enum_backing_type_id() == type_id
		assert a.nodes[3].fn_value_type_id() == type_id
		assert a.nodes[4].generated_variant_type_id() == type_id
		assert a.nodes[5].mut_optional_arg_view_type_id() == type_id
		payload := a.nodes[6].payload_ptr()
		assert payload.generic_params == ['T']
		assert payload.generic_constraints.ids == [constraint_id]
		assert payload.call_type_args.ids == [type_id]
		assert payload.debugger_value_types.ids == [type_id]
		assert payload.enum_backing_type == type_id
		assert payload.fn_value_type == type_id
		assert payload.generated_variant_type == type_id
		assert payload.mut_optional_arg_view_type == type_id
		assert a.nodes[7].generic_params().len == 0
		assert a.nodes[7].generic_constraint_ids() == [constraint_id]
	}
}

fn test_clone_scoped_worker_node_publishes_indirect_generic_names() {
	$if prealloc {
		mut a := flat.FlatAst.new()
		a.add_node(flat.Node{ kind: .fn_decl })
		mut tc := types.TypeChecker.new(&a)
		mut master := new_transformer(mut a, &tc, map[string]bool{})
		mut params := ['unscoped']
		scope := transform_worker_scope_begin(true)
		params[0] = 'scoped parameter'.clone()
		transform_worker_scope_leave(scope)
		a.nodes[0].set_generic_params(params)
		assert !transform_scope_owns(scope, a.nodes[0].payload_ptr())
		assert !transform_scope_owns(scope, a.nodes[0].generic_params().data)
		assert transform_scope_owns(scope, a.nodes[0].generic_params()[0].str)
		master.clone_scoped_worker_node(0, scope)
		assert !transform_scope_owns(scope, a.nodes[0].generic_params()[0].str)
		transform_worker_scope_free(scope)
		assert a.nodes[0].generic_params() == ['scoped parameter']
	}
}

fn test_scoped_text_scan_finds_borrowed_text_in_unscoped_payloads() {
	$if prealloc {
		mut node := flat.Node{ kind: .fn_decl, value: 'unscoped' }
		scope := transform_worker_scope_begin(true)
		params := ['scoped parameter'.clone()]
		transform_worker_scope_leave(scope)
		node.set_generic_params(params)
		lo, hi := scopes_address_range([scope])
		assert hi > lo
		assert !transform_scope_owns(scope, node.value.str)
		assert !transform_scope_owns(scope, node.payload_ptr())
		assert node_has_any_scoped_text(&node, [scope], lo, hi)
		plain := flat.Node{ kind: .ident, value: 'unscoped' }
		assert !node_has_any_scoped_text(&plain, [scope], lo, hi)
		assert !text_pointer_in_scopes(unsafe { nil }, [scope], lo, hi)
		transform_worker_scope_free(scope)
	}
}

fn test_generic_batch_publishes_indirect_base_writes_and_new_nodes() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{ kind: .ident, value: 'outside_current_body' })
	a.add_node(flat.Node{ kind: .ident, value: 'previous_batch' })
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.scope_parallel_workers = true
	assert !master.skip_generics
	new_node_start := a.nodes.len
	scope := transform_worker_scope_begin(true)
	mut batch := master.fork_scoped_batch_worker(&a, tc.fork_for_parallel_transform(&a))
	batch.scoped_base_nodes = new_node_start
	batch.item_range_lo = 1
	batch.item_range_hi = 1
	batch.set_node_value(0, 'indirect_rewrite'.clone())
	batch.set_node_typ(1, flat.source_type_id('previous_batch_type'.clone()))
	batch.set_node_generic_params(0, ['T'.clone()])
	a.nodes[0].set_generic_params_and_constraints(['T'.clone()], ['retiring constraint'.clone()])
	added := a.add_node(flat.Node{ kind: .ident, value: 'appended'.clone(), typ: flat.source_type_id('new_type'.clone()) })
	transform_worker_scope_leave(scope)
	master.absorb_scoped_batch(batch, scope, new_node_start)
	for id in [0, 1, int(added)] {
		node := a.nodes[id]
		assert !transform_scope_owns(scope, node.value.str)
		assert !transform_scope_owns(scope, node.typ_text().str)
		if node.generic_params().len > 0 {
			assert !transform_scope_owns(scope, node.payload_ptr())
			assert !transform_scope_owns(scope, node.generic_params().data)
			assert !transform_scope_owns(scope, node.generic_params()[0].str)
			assert !transform_scope_owns(scope, node.generic_constraints().data)
			assert !transform_scope_owns(scope, node.generic_constraints()[0].str)
		}
	}
	transform_worker_scope_free(scope)
	assert a.nodes[0].value == 'indirect_rewrite'
	assert a.nodes[0].generic_params() == ['T']
	assert a.nodes[0].generic_constraints() == ['retiring constraint']
	assert a.nodes[1].typ_text() == 'previous_batch_type'
	assert a.nodes[int(added)].value == 'appended'
}

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
		sum_name:      flat.source_type_id('main.Sum'.clone())
		module:        'main'.clone()
		file:          'main.v'.clone()
		helper_module: 'main'.clone()
	}
	helper.sum_eq_types['main.Node'.clone()] = SumEqRequest{
		struct_name:   flat.source_type_id('main.Node'.clone())
		module:        'main'.clone()
		file:          'main.v'.clone()
		helper_module: 'main'.clone()
	}
	text := helper.promote_scoped_result_text('main.resolved'.clone())
	assert !transform_scope_owns(scope, text.str)
	helper.tc.fork_overlay.resolved_call_names[10] = text
	helper.generic_call_spec_cache[12] = GenericCallSpec{
		decl_key: 'main.generic'.clone()
		args:     flat.TypeTextList.from_names(['[]int'.clone()])
	}
	transform_worker_scope_leave(scope)
	master.merge_worker_used_fns(helper)
	assert !transform_scope_owns(scope, master.used_fns_log[0].str)
	assert !transform_scope_owns(scope, master.sum_eq_types['main.Sum'].sum_name_text().str)
	assert !transform_scope_owns(scope, master.sum_eq_types['main.Node'].struct_name_text().str)
	master.merge_worker(helper, []FnWorkItem{}, 0, 0, false)
	assert helper.merge_scratch_scope == unsafe { nil }
	assert master.used_fns_log == ['main.generated']
	assert master.sum_eq_types['main.Sum'].file == 'main.v'
	assert master.sum_eq_types['main.Node'].struct_name_text() == 'main.Node'
	assert tc.sparse_resolved_call_names[10] == 'main.resolved'
	assert master.generic_call_spec_cache[12].decl_key == 'main.generic'
	assert master.generic_call_spec_cache[12].args_text() == ['[]int']
}

fn test_scoped_monomorph_specialization_args_are_deep_cloned() {
	mut master := flat.TypeTextListBindings{}
	scope := transform_worker_scope_begin(true)
	mut worker := flat.TypeTextListBindings{}
	scoped_name := 'decode_cloud_Body'.clone()
	worker.set(scoped_name, ['cloud.Body'.clone(), '[]string'.clone()])
	scoped_args := worker.type_ids(scoped_name) or { panic('missing worker args') }
	transform_worker_scope_leave(scope)

	// Mirrors the scoped and parallel monomorphize merges, which publish the
	// worker's arena-backed argument IDs before its scratch scope is freed.
	master.set_ids(scoped_name.clone(), scoped_args)
	owned_args := master.type_ids('decode_cloud_Body') or { panic('missing master args') }
	assert !transform_scope_owns(scope, owned_args.data)
	assert !transform_scope_owns(scope, master.keys()[0].str)
	for arg in owned_args {
		assert !transform_scope_owns(scope, flat.source_type_name(arg).str)
	}
	assert master.value('decode_cloud_Body') == ['cloud.Body', '[]string']
	transform_worker_scope_free(scope)
	assert master.value('decode_cloud_Body') == ['cloud.Body', '[]string']
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

fn test_monomorph_release_preserves_cross_worker_specialization_contexts() {
	$if prealloc {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		a.specialized_fn_modules[0] = 'builtin'
		a.specialized_fn_files[0] = 'builtin.v'
		mut module_ptrs := [3]voidptr{}
		mut file_ptrs := [3]voidptr{}
		for idx in 1 .. 4 {
			scope := transform_worker_scope_begin(true)
			module_name := 'worker_${idx}'.clone()
			file := 'worker_${idx}.v'.clone()
			assert transform_scope_owns(scope, module_name.str)
			assert transform_scope_owns(scope, file.str)
			module_ptrs[idx - 1] = module_name.str
			file_ptrs[idx - 1] = file.str
			transform_worker_scope_leave(scope)
			// A nested declaration can be discovered by one worker and emitted
			// by another, including the master. Its context is not a node payload.
			a.specialized_fn_nodes[idx] = true
			a.specialized_fn_modules[idx] = module_name
			a.specialized_fn_files[idx] = file
			t.monomorph_worker_scopes << scope
		}
		t.release_monomorph_worker_scopes()
		assert t.monomorph_worker_scopes.len == 0
		for idx in 1 .. 4 {
			// Small arenas may be recycled instead of unmapped immediately.
			// Check ownership before dereferencing potentially released strings.
			assert voidptr(a.specialized_fn_modules[idx].str) != module_ptrs[idx - 1]
			assert voidptr(a.specialized_fn_files[idx].str) != file_ptrs[idx - 1]
			assert a.specialized_fn_nodes[idx]
			assert a.specialized_fn_modules[idx] == 'worker_${idx}'
			assert a.specialized_fn_files[idx] == 'worker_${idx}.v'
		}
		assert a.specialized_fn_modules[0] == 'builtin'
		assert a.specialized_fn_files[0] == 'builtin.v'
	}
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
	assert !t.generic_arg_is_unresolved(flat.source_type_int)
	assert !transform_scope_owns(scope, t.generic_unresolved_cache.module.str)

	t.cur_module = 'main'
	transform_worker_scope_free(scope)
	assert !t.generic_arg_is_unresolved(flat.source_type_int)
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
	old_jobs := os.getenv('VJOBS')
	defer { os.setenv('VJOBS', old_jobs, true) }
	os.setenv('VJOBS', '2', true)
	mut a := flat.FlatAst.new()
	param_id := a.add_node(flat.Node{
		kind:  .param
		value: 'item'
		typ:   flat.source_type_id('T')
	})
	item_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'item'
		typ:   flat.source_type_id('T')
	})
	return_children := a.begin_children()
	a.add_child(item_id)
	return_id := a.add_node(flat.Node{
		kind:           .return_stmt
		typ:            flat.source_type_id('T')
		children_start: return_children
		children_count: 1
	})
	literal_children := a.begin_children()
	a.add_child(param_id)
	a.add_child(return_id)
	literal_id := a.add_node(flat.Node{
		kind:           .fn_literal
		typ:            flat.source_type_id('T')
		children_start: literal_children
		children_count: 2
	})
	outer_return_children := a.begin_children()
	a.add_child(literal_id)
	outer_return_id := a.add_node(flat.Node{
		kind:           .return_stmt
		typ:            flat.source_type_id('fn (T) T')
		children_start: outer_return_children
		children_count: 1
	})
	fn_children := a.begin_children()
	a.add_child(outer_return_id)
	mut fn_node := flat.Node{
		kind:           .fn_decl
		value:          'make_callback'
		typ:            flat.source_type_id('fn (T) T')
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
	// Exhaust direct emission so the drain exercises its scoped batch path.
	t.direct_monomorph_specs = direct_monomorph_spec_budget
	t.defer_nested_generic_emissions = true
	for arg in ['int', 'string'] {
		t.request_generic_fn_specialization(decl, flat.TypeTextList.from_names([arg]).ids)
	}
	mut emitted := GenericFnSpecBoolMap{}
	mut generated := []string{}
	assert t.drain_pending_generic_fn_specs(map[string]GenericStructDecl{}, map[string]GenericSumDecl{}, mut emitted, mut generated)
	assert isnil(a.worker_pool)
	assert t.pending_generic_fn_specs.len == 0
	for arg in ['int', 'string'] {
		spec_key := generic_fn_spec_key(decl.key, flat.TypeTextList.from_names([arg]).ids)
		assert emitted.contains(spec_key)
		root := t.generic_fn_spec_nodes.get(spec_key) or { panic('missing ${spec_key.legacy_text()}') }
		name := transform_qualified_fn_name(decl.module, a.node(root).value)
		return_type := tc.fn_ret_types[name] or { panic('missing signature ${name}') }
		assert return_type is types.FnType
		assert (return_type as types.FnType).return_type.name() == arg
		assert (return_type as types.FnType).params[0].typ.name() == arg
	}
	mut lifted := 0
	for idx in 0 .. a.nodes.len {
		node := a.node(flat.NodeId(idx))
		if node.kind != .fn_decl || !node.value.starts_with('__anon_fn_') {
			continue
		}
		lifted++
		assert node.typ_text() in ['int', 'string']
		for name in [node.value, transform_qualified_fn_name(decl.module, node.value)] {
			assert flat.source_type_name(t.fn_ret_types.value_id(name)) == node.typ_text()
			assert (tc.fn_ret_types[name] or { panic('missing lifted return ${name}') }).name() == node.typ_text()
			params := tc.fn_param_types[name] or { panic('missing lifted params ${name}') }
			assert params.len == 1
			assert params[0].name() == node.typ_text()
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
