module types

import v.flat

fn global_store_query_checker() &TypeChecker {
	mut a := flat.FlatAst.new()
	names := ['root', 'cycle', 'sink', 'clean', 'leaf', 'translated']
	for name in names {
		a.add_node(flat.Node{ kind: .fn_decl, value: name })
	}
	mut tc := TypeChecker.new(&a)
	for i, name in names {
		tc.register_visible_mutation_fn_decl(i, 'main', name, name)
		tc.fn_type_files[name] = '${name}.v'
		tc.fn_type_modules[name] = 'main'
	}
	tc.has_globals_files['sink.v'] = true
	tc.translated_files['translated.v'] = true
	root_id, _ := tc.intern_symbol('root')
	cycle_id, _ := tc.intern_symbol('cycle')
	sink_id, _ := tc.intern_symbol('sink')
	leaf_id, _ := tc.intern_symbol('leaf')
	tc.direct_dependencies_by_fn[0] = [cycle_id, sink_id]
	tc.direct_dependencies_by_fn[1] = [root_id]
	tc.direct_dependencies_by_fn[3] = [leaf_id]
	tc.visible_mutation_cache.decl_index_ready = true
	tc.remember_resolved_call(flat.NodeId(0), 'root')
	tc.set_resolved_fn_value(1, 'cycle')
	return tc
}

fn test_frozen_global_store_queries_keep_recursive_roots_independent() {
	tc := global_store_query_checker()
	forked := tc.fork_for_parallel_transform(tc.a)
	// root -> cycle -> root and root -> sink. A negative answer reached while
	// root is visiting must not become the answer to a later cycle root query.
	for _ in 0 .. 3 {
		assert forked.resolved_call_may_store_globally(flat.NodeId(0))
		assert forked.fn_value_may_store_globally(flat.NodeId(1))
		assert !forked.cached_fn_may_store_globally('clean')
		assert forked.cached_fn_may_store_globally('translated')
	}
	assert forked.fn_value_may_store_globally(flat.NodeId(-1))
	assert !forked.resolved_call_may_store_globally(flat.NodeId(-1))
}

fn test_frozen_global_store_cache_is_private_and_invalidated_by_signature_changes() {
	tc := global_store_query_checker()
	mut first := tc.fork_for_parallel_transform(tc.a)
	second := tc.fork_for_parallel_transform(tc.a)
	assert !first.cached_fn_may_store_globally('clean')
	assert !second.cached_fn_may_store_globally('clean')

	first.ensure_private_transform_signatures()
	first.fn_type_files['leaf'] = 'sink.v'
	assert first.cached_fn_may_store_globally('clean')
	assert !second.cached_fn_may_store_globally('clean')

	// Later edits must invalidate as well as the initial detachment.
	first.ensure_private_transform_signatures()
	first.fn_type_files['leaf'] = 'leaf.v'
	assert !first.cached_fn_may_store_globally('clean')
	assert !second.cached_fn_may_store_globally('clean')

	nested := first.fork_for_parallel_transform(first.a)
	assert !nested.cached_fn_may_store_globally('clean')
}

fn test_live_global_store_queries_do_not_reuse_post_check_results() {
	mut tc := global_store_query_checker()
	assert !tc.cached_fn_may_store_globally('clean')
	tc.fn_type_files['leaf'] = 'sink.v'
	assert tc.cached_fn_may_store_globally('clean')
	tc.fn_type_files['leaf'] = 'leaf.v'
	assert !tc.cached_fn_may_store_globally('clean')
}

fn test_frozen_global_store_declaration_index_preserves_module_priority() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.register_visible_mutation_fn_decl_with_lowered(10, 'alpha', 'alpha.helper', 'helper',
		'alpha__helper', 'helper')
	tc.register_visible_mutation_fn_decl_with_lowered(20, 'beta', 'beta.helper', 'helper',
		'beta__helper', 'helper')
	tc.register_visible_mutation_fn_decl_with_lowered(30, 'alpha', 'alpha.Box[T].method',
		'Box[T].method', 'alpha__Box_T__method', 'Box_T__method')
	tc.visible_mutation_cache.decl_index_ready = true
	forked := tc.fork_for_parallel_transform(&a)
	for module_name, expected in {
		'alpha': 10
		'beta':  20
		'':      10
	} {
		decl := forked.global_store_fn_decl('helper', module_name) or { panic('missing helper') }
		assert decl.idx == expected
	}
	assert forked.global_store_fn_decl('helper', 'missing') == none
	for name in ['Box[int].method', 'alpha.Box[string].method', 'Box.method'] {
		decl := forked.global_store_fn_decl(name, 'alpha') or { panic('missing generic method') }
		assert decl.idx == 30
	}
	alias_decl := forked.global_store_fn_decl('beta__helper', 'beta') or { panic('missing alias') }
	assert alias_decl.idx == 20
}

fn test_frozen_global_store_declaration_misses_fall_back_to_current_ast() {
	mut a := flat.FlatAst.new()
	module_id := a.add_node(flat.Node{ kind: .module_decl, value: 'main' })
	generated_id := a.add_node(flat.Node{ kind: .fn_decl, value: 'generated' })
	mut tc := TypeChecker.new(&a)
	tc.top_level_idx = [i32(module_id), i32(generated_id)]
	tc.visible_mutation_cache.decl_index_ready = true
	forked := tc.fork_for_parallel_transform(&a)
	decl := forked.global_store_fn_decl('generated', 'main') or { panic('missing generated declaration') }
	assert decl.idx == int(generated_id)
	assert decl.mod == 'main'
}
