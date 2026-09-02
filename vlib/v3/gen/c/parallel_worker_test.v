module c

import v3.flat
import v3.pref
import v3.types

fn parallel_worker_test_gen(scoped bool) (&FlatGen, &types.TypeChecker) {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.scope_parallel_workers = scoped
	return &g, &tc
}

fn test_parallel_dispatch_worker_owns_checker_outside_scoped_batching() {
	g, tc := parallel_worker_test_gen(false)
	w := g.new_parallel_dispatch_worker(1)
	assert w.tc != tc
}

fn test_parallel_dispatch_worker_shares_checker_as_scoped_accumulator() {
	g, tc := parallel_worker_test_gen(true)
	w := g.new_parallel_dispatch_worker(1)
	assert w.tc == tc
}

fn test_scoped_parallel_dispatch_worker_owns_string_snapshot() {
	mut g, _ := parallel_worker_test_gen(true)
	assert g.intern_string('source') == 0
	mut w := g.new_parallel_dispatch_worker(1)
	assert !w.str_lits_shared
	assert w.intern_string('worker generated') == 1
	assert g.str_lits == ['source']
	assert g.intern_string('master generated') == 1
	assert w.str_lits == ['source', 'worker generated']
	assert g.str_lits == ['source', 'master generated']
}

fn test_scoped_parallel_worker_reuses_preselected_functions_and_c_extern_refs() {
	mut g, _ := parallel_worker_test_gen(true)
	g.fn_gen_items = [FlatFnGenItem{
		c_name: 'main__run'
	}]
	g.c_extern_refs['puts'] = true
	g.c_extern_refs_ready = true

	w := g.new_parallel_worker(1)
	assert w.fn_gen_items.len == 1
	assert w.fn_gen_items[0].c_name == 'main__run'
	assert w.c_extern_refs == {
		'puts': true
	}
	assert w.c_extern_refs_ready
}

fn test_parallel_worker_shares_precomputed_const_short_index() {
	mut g, _ := parallel_worker_test_gen(true)
	g.const_vals = {
		'moda.only':   flat.NodeId(0)
		'moda.answer': flat.NodeId(1)
		'modb.answer': flat.NodeId(2)
	}
	g.precompute_const_short_index()
	assert g.const_short_index.built
	assert g.const_short_index.entries['only'] == 'moda.only'
	assert g.const_short_index.entries['answer'] == ''

	w := g.new_parallel_worker(1)
	assert w.const_short_index == g.const_short_index
	assert w.unique_const_ref_name('only') or { '' } == 'moda.only'
	assert w.unique_const_ref_name('answer') == none
}

fn test_parallel_worker_preserves_test_assertion_stats_mode() {
	mut g, _ := parallel_worker_test_gen(true)
	g.show_test_stats = true
	w := g.new_parallel_worker(1)
	assert w.show_test_stats
}

fn test_parallel_worker_preserves_target() {
	mut g, _ := parallel_worker_test_gen(true)
	g.target = pref.target_from('linux', 'x86') or { panic(err) }
	w := g.new_parallel_worker(1)
	assert w.target == g.target
}

fn test_windows_filelock_method_preseeds_parallel_compat_helpers() {
	mut g, _ := parallel_worker_test_gen(true)
	mut used := {
		'filelock.FileLock.lock_handle': true
	}
	g.used_fns = &used
	// Isolate used-function reachability from the later function-body reference scan.
	g.c_extern_refs_ready = true
	g.preseed_libc_compat_fns()
	assert g.libc_compat_fns['filelock']

	g.libc_compat_fns.delete('filelock')
	g.filelock_compat_decls()
	c_code := g.sb.str()
	assert c_code.contains('static inline int v_filelock_lock(HANDLE handle'), c_code
	assert c_code.contains('static inline int v_filelock_unlock(HANDLE handle'), c_code
}

fn test_builtin_gettid_preseeds_parallel_compat_helper() {
	mut g, _ := parallel_worker_test_gen(true)
	mut used := {
		'v_gettid': true
	}
	g.used_fns = &used
	// Isolate used-function reachability from the later function-body reference scan.
	g.c_extern_refs_ready = true
	g.preseed_libc_compat_fns()
	assert g.libc_compat_fns['gettid']
}

fn test_parallel_tail_worker_preserves_runtime_init_module_order() {
	mut g, _ := parallel_worker_test_gen(true)
	g.const_runtime_inits = ['\tmoda__runtime_const = moda__make_const();']
	g.const_runtime_init_modules = ['moda']
	g.runtime_inits = ['\tmoda__runtime_global = moda__make_global();']
	g.runtime_init_modules = ['moda']
	g.module_init_fns = ['moda__init']
	g.module_init_fn_modules['moda__init'] = 'moda'

	mut tail := g.new_parallel_tail_worker(max_flat_cgen_jobs + 1)
	tail.gen_vinit()
	output := tail.sb.str()
	const_pos := output.index('moda__runtime_const = moda__make_const();') or { -1 }
	global_pos := output.index('moda__runtime_global = moda__make_global();') or { -1 }
	init_pos := output.index('moda__init();') or { -1 }
	assert const_pos >= 0
	assert global_pos > const_pos
	assert init_pos > global_pos
}

fn test_parallel_tail_worker_preserves_shared_cleanup_mode() {
	mut g, _ := parallel_worker_test_gen(true)
	g.is_shared = true
	assert g.module_cleanup_fns.len == 0

	mut tail := g.new_parallel_tail_worker(max_flat_cgen_jobs + 1)
	tail.gen_vcleanup()
	output := tail.sb.str()
	assert tail.is_shared
	assert output.contains('void _vcleanup(void) {')
}

fn test_parallel_checker_clone_preserves_sparse_transform_caches() {
	g, mut tc := parallel_worker_test_gen(false)
	tc.a.nodes = [flat.Node{
		kind: .ident
	}, flat.Node{
		kind: .ident
	}]
	tc.resolved_call_names = ['source_call']
	tc.resolved_call_set = [true]
	tc.expr_type_values = [types.Type(types.int_)]
	tc.expr_type_set = [true]
	tc.begin_sparse_transform_node_caches(1)
	tc.sparse_resolved_call_names[1] = 'transformed_call'
	tc.sparse_expr_type_values[1] = types.Type(types.String{})

	w := g.clone_parallel_type_checker()
	assert w.parallel_check_sparse
	assert w.check_range_lo == 0
	assert w.check_range_hi == 0
	assert w.resolved_call_name(flat.NodeId(0)) or { '' } == 'source_call'
	assert w.resolved_call_name(flat.NodeId(1)) or { '' } == 'transformed_call'
	assert w.expr_type(flat.NodeId(0)) or { types.Type(types.void_) } is types.Primitive
	assert w.expr_type(flat.NodeId(1)) or { types.Type(types.void_) } is types.String
	assert g.parallel_cached_expr_type(flat.NodeId(0), tc.a.nodes[0]) or { types.Type(types.void_) } is types.Primitive
	assert g.parallel_cached_expr_type(flat.NodeId(1), tc.a.nodes[1]) or { types.Type(types.void_) } is types.String
}

fn test_parallel_checker_clone_keeps_checked_file_scope_identity() {
	g, mut tc := parallel_worker_test_gen(true)
	tc.file_scope.insert('file_value', types.Type(types.int_))
	tc.resolution_type_mode = true
	w := g.clone_parallel_type_checker()
	assert w.file_scope == tc.file_scope
	assert w.cur_scope != w.file_scope
	assert w.resolution_type_mode
	owner := w.cur_scope.lookup_owner('file_value') or { panic('missing file binding') }
	assert owner.belongs_to_scope(w.file_scope)
}

fn test_scoped_cgen_batch_preserves_worker_interned_literals() {
	mut g, _ := parallel_worker_test_gen(true)
	assert g.intern_string('source') == 0
	for generated in ['generated_a', 'generated_b'] {
		mut batch := g.new_parallel_worker(0)
		generated_id := batch.intern_string(generated)
		assert generated_id == g.str_lits.len
		g.absorb_scoped_cgen_batch(batch, false)
		assert g.str_lits[generated_id] == generated
		assert g.str_lit_ids[generated] == generated_id
	}
	assert g.str_lits == ['source', 'generated_a', 'generated_b']
}

fn test_scoped_cgen_worker_merge_publishes_generated_literals() {
	mut g, _ := parallel_worker_test_gen(true)
	assert g.intern_string('source') == 0

	mut helper := g.new_parallel_dispatch_worker(1)
	mut helper_batch := helper.new_parallel_worker(0)
	assert helper_batch.intern_string('helper generated') == 1
	helper_batch.sb.write_string('helper(_str_1); "_str_1"; /* _str_1 */')
	helper_batch.add_spawn_wrapper_def('spawn_helper(_str_1);')
	helper.absorb_scoped_cgen_batch(helper_batch, false)

	mut master_batch := g.new_parallel_worker(0)
	assert master_batch.intern_string('master generated') == 1
	master_batch.sb.write_string('master(_str_1);')
	g.absorb_scoped_cgen_batch(master_batch, false)
	g.merge_parallel_worker(helper)

	assert g.str_lits == ['source', 'master generated', 'helper generated']
	assert g.str_lit_ids['helper generated'] == 2
	assert g.fn_segs == ['master(_str_1);', 'helper(_str_2); "_str_1"; /* _str_1 */']
	assert g.spawn_wrapper_defs == ['spawn_helper(_str_2);']
}

fn test_scoped_cgen_string_remap_preserves_user_c_identifiers() {
	mut g, _ := parallel_worker_test_gen(true)
	g.c_extern_refs['_str_999'] = true
	g.c_extern_refs_ready = true
	user_c_symbols := g.cache_user_c_string_symbols()
	remap := {
		1:   2
		999: 1000
	}
	source := 'helper(_str_1); _str_999(); "_str_1"; /* _str_999 */'
	assert remap_scoped_worker_string_symbols(source, remap, user_c_symbols) == 'helper(_str_2); _str_999(); "_str_1"; /* _str_999 */'
}

fn test_fused_parallel_prep_interns_body_string_literals() {
	mut g, _ := parallel_worker_test_gen(false)
	g.a.nodes = [
		flat.Node{
			kind:           .fn_decl
			children_start: 0
			children_count: 1
		},
		flat.Node{
			kind:  .string_literal
			value: 'worker literal'
		},
	]
	g.a.children = [flat.NodeId(1)]
	mut stack := []flat.NodeId{}
	mut type_text_cache := map[string]bool{}
	g.fn_item_cost_and_prep(0, mut stack, mut type_text_cache)
	assert g.str_lits == ['worker literal']
	assert g.str_lit_ids['worker literal'] == 0
}

fn test_serial_prep_interns_ast_string_literals_in_source_order() {
	mut g, _ := parallel_worker_test_gen(false)
	g.ast_string_literals = ['first', 'second']
	g.ast_string_literals_ready = true
	g.prepare_serial_fn_tables()
	assert g.str_lits == ['first', 'second']
	assert g.str_lit_ids['first'] == 0
	assert g.str_lit_ids['second'] == 1
}

fn test_scoped_pre_dispatch_preserves_direct_array_access_flag() {
	mut g, _ := parallel_worker_test_gen(true)
	fn_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'unchecked_index'
	})
	g.fn_gen_items = [
		FlatFnGenItem{
			node_id:             fn_id
			file:                'direct_array_access.v'
			module:              'main'
			c_name:              'main__unchecked_index'
			cost:                1
			direct_array_access: true
			ignore_overflow:     true
		},
	]
	g.prepare_pre_dispatch_master()
	assert g.fn_gen_items.len == 1
	assert g.fn_gen_items[0].direct_array_access
	assert g.fn_gen_items[0].ignore_overflow
	g.release_scoped_fn_items()
}

fn test_parallel_generic_app_cache_uses_frozen_base_and_private_overlays() {
	mut g, _ := parallel_worker_test_gen(true)
	base, args, ok := g.shared_generic_app_parts('Frozen[int]')
	assert ok
	assert base == 'Frozen'
	assert args == ['int']
	frozen := g.generic_app_cache

	g.freeze_parallel_lookup_caches()
	assert voidptr(g.generic_app_cache) != voidptr(frozen)
	assert voidptr(g.generic_app_cache.base) == voidptr(frozen)

	mut dispatcher := g.new_parallel_dispatch_worker(1)
	assert voidptr(dispatcher.generic_app_cache) != voidptr(g.generic_app_cache)
	assert voidptr(dispatcher.generic_app_cache.base) == voidptr(frozen)
	mut batch := dispatcher.new_parallel_worker(0)
	assert voidptr(batch.generic_app_cache) != voidptr(dispatcher.generic_app_cache)
	assert voidptr(batch.generic_app_cache.base) == voidptr(frozen)

	g.shared_generic_app_parts('Shared[string]')
	dispatcher.shared_generic_app_parts('Shared[string]')
	batch.shared_generic_app_parts('Shared[string]')
	assert 'Shared[string]' in g.generic_app_cache.entries
	assert 'Shared[string]' in dispatcher.generic_app_cache.entries
	assert 'Shared[string]' in batch.generic_app_cache.entries
	assert 'Shared[string]' !in frozen.entries
}

fn test_open_generic_receiver_template_bypasses_stale_generic_app_cache() {
	mut g, _ := parallel_worker_test_gen(true)
	mut cache := g.generic_app_cache
	cache.entries['AtomicVal[T]'] = GenericAppInfo{}
	node := flat.Node{
		kind:  .fn_decl
		value: 'AtomicVal[T].load'
	}
	assert g.fn_node_is_open_generic_template(node, 'stdatomic')
	assert !g.should_emit_fn_node_in_module_known(node, 'stdatomic', 'atomic.v',
		'stdatomic__AtomicVal_T__load', true)
}

fn test_parallel_type_declarations_include_body_discovered_fn_ptr_types() {
	mut g, _ := parallel_worker_test_gen(true)
	g.parallel_type_decls = '/* precomputed type declarations */\n'.clone()
	encoded := 'fn_ptr:int|int'
	name := g.resolve_fn_ptr_type(encoded)

	g.write_type_declaration_block()
	source := g.sb.str()
	precomputed_idx := source.index('/* precomputed type declarations */') or { -1 }
	typedef_idx := source.index('typedef int (*${name})(int);') or { -1 }
	assert precomputed_idx >= 0
	assert typedef_idx > precomputed_idx
	assert g.emitted_fn_ptr_typedefs[encoded]
}

fn test_dynamic_parallel_merge_preserves_chunk_order() {
	mut g, _ := parallel_worker_test_gen(true)
	mut first := g.new_parallel_dispatch_worker(1)
	first.fn_segs = ['chunk-2;', 'chunk-0;']
	first.fn_seg_chunk_indexes = [2, 0]
	mut second := g.new_parallel_dispatch_worker(2)
	second.fn_segs = ['chunk-3;', 'chunk-1;']
	second.fn_seg_chunk_indexes = [3, 1]
	mut ordered := []string{len: 4}
	mut ordered_wrapper_defs := []ParallelChunkWrapperDefs{len: 4}

	g.merge_parallel_worker_ordered(first, mut ordered, mut ordered_wrapper_defs)
	g.merge_parallel_worker_ordered(second, mut ordered, mut ordered_wrapper_defs)
	assert ordered == ['chunk-0;', 'chunk-1;', 'chunk-2;', 'chunk-3;']
}

fn test_dynamic_parallel_merge_replays_wrapper_defs_in_chunk_order() {
	mut g, _ := parallel_worker_test_gen(true)
	mut high := g.new_parallel_dispatch_worker(1)
	high.fn_segs = ['chunk-3;', 'chunk-2;']
	high.fn_seg_chunk_indexes = [3, 2]
	high.parallel_chunk_wrapper_defs = [
		ParallelChunkWrapperDefs{
			chunk_idx: 3
			spawn:     ['spawn-3-typedef;', 'spawn-3-trampoline;', 'spawn-shared;']
			callback:  ['callback-3;']
		},
		ParallelChunkWrapperDefs{
			chunk_idx: 2
			spawn:     ['spawn-2-typedef;', 'spawn-2-trampoline;']
			callback:  ['callback-2;']
		},
	]

	mut low := g.new_parallel_dispatch_worker(2)
	low.fn_segs = ['chunk-1;', 'chunk-0;']
	low.fn_seg_chunk_indexes = [1, 0]
	low.parallel_chunk_wrapper_defs = [
		ParallelChunkWrapperDefs{
			chunk_idx: 1
			spawn:     ['spawn-1-typedef;', 'spawn-1-trampoline;']
			callback:  ['callback-1;']
		},
		ParallelChunkWrapperDefs{
			chunk_idx: 0
			spawn:     ['spawn-0-typedef;', 'spawn-0-trampoline;', 'spawn-shared;']
			callback:  ['callback-0;']
		},
	]

	mut ordered := []string{len: 4}
	mut ordered_wrapper_defs := []ParallelChunkWrapperDefs{len: 4}
	g.merge_parallel_worker_ordered(high, mut ordered, mut ordered_wrapper_defs)
	g.merge_parallel_worker_ordered(low, mut ordered, mut ordered_wrapper_defs)
	g.replay_ordered_parallel_wrapper_defs(ordered_wrapper_defs)

	assert ordered == ['chunk-0;', 'chunk-1;', 'chunk-2;', 'chunk-3;']
	assert g.spawn_wrapper_defs == ['spawn-0-typedef;', 'spawn-0-trampoline;', 'spawn-shared;',
		'spawn-1-typedef;', 'spawn-1-trampoline;', 'spawn-2-typedef;', 'spawn-2-trampoline;',
		'spawn-3-typedef;', 'spawn-3-trampoline;']
	assert g.callback_wrapper_defs == ['callback-0;', 'callback-1;', 'callback-2;', 'callback-3;']
}

fn test_dynamic_parallel_chunk_capture_keeps_deduplicated_wrapper_attempts() {
	mut g, _ := parallel_worker_test_gen(true)
	g.parallel_chunk_wrapper_defs << ParallelChunkWrapperDefs{
		chunk_idx: 2
	}
	g.parallel_chunk_wrapper_capture = 0
	g.add_spawn_wrapper_def('spawn-shared;')
	g.add_spawn_wrapper_def('spawn-shared;')
	g.add_callback_wrapper_def('callback-shared;')
	g.add_callback_wrapper_def('callback-shared;')
	g.parallel_chunk_wrapper_capture = -1

	assert g.spawn_wrapper_defs == ['spawn-shared;']
	assert g.callback_wrapper_defs == ['callback-shared;']
	assert g.parallel_chunk_wrapper_defs[0].spawn == ['spawn-shared;', 'spawn-shared;']
	assert g.parallel_chunk_wrapper_defs[0].callback == ['callback-shared;', 'callback-shared;']
}
