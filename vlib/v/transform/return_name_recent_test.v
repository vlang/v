module transform

import v.flat
import v.types

fn test_return_name_recent_reuses_composite_ids_and_equivalent_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	name := 'composite_result'.clone()
	tc.register_generated_fn_return_type(name, tc.parse_type(flat.source_type_id('![]map[string]int')))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.fn_return_name_cache = new_fn_return_name_cache()
	for query in [name, name.clone(), name] {
		assert t.checker_fn_return_type_name(query) or { panic('missing return type') } == flat.source_type_id('![]map[string]int')
	}
	t.memo_semantic_type_names = true
	assert t.checker_fn_return_type_name(name) or { panic('missing return type') } == flat.source_type_id('![]map[string]int')
}

fn test_return_name_recent_invalidates_same_count_signature_changes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	name := 'target'.clone()
	later := 'later'.clone()
	tc.register_generated_fn_return_type(name, types.Type(types.string_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.fn_return_name_cache = new_fn_return_name_cache()
	assert t.checker_fn_return_type_name(name) or { panic('missing return type') } == flat.source_type_string
	assert t.checker_fn_return_type_name(later) == none
	tc.register_generated_fn_return_type(name, types.Type(types.bool_))
	assert t.checker_fn_return_type_name(name) or { panic('stale return type') } == flat.source_type_bool
	tc.unregister_generated_fn_return_type(name)
	tc.register_generated_fn_return_type(later, types.Type(types.int_))
	assert t.checker_fn_return_type_name(name) == none
	assert t.checker_fn_return_type_name(later) or { panic('stale negative answer') } == flat.source_type_int
}

fn test_return_name_recent_retains_unknown_void_and_nil_checker_semantics() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.register_generated_fn_return_type('unknown_result', types.Type(types.Unknown{}))
	tc.register_generated_fn_return_type('void_result', types.Type(types.void_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.fn_return_name_cache = new_fn_return_name_cache()
	for _ in 0 .. 2 {
		assert t.checker_fn_return_type_name('unknown_result') or { panic('lost unknown type') } == flat.source_type_unknown
		assert t.checker_fn_return_type_name('void_result') or { panic('lost void type') } == flat.source_type_void
		assert t.checker_fn_return_type_name('absent') == none
	}
	nil_checker := Transformer{ a: &a }
	assert nil_checker.checker_fn_return_type_name('anything') == none
}

fn test_return_name_recent_unprepared_transformer_observes_raw_map_overwrites() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_ret_types['target'] = types.Type(types.string_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert isnil(t.fn_return_name_cache)
	assert t.checker_fn_return_type_name('target') or { panic('missing return type') } == flat.source_type_string
	tc.fn_ret_types['target'] = types.Type(types.bool_)
	assert t.checker_fn_return_type_name('target') or { panic('stale uncached return type') } == flat.source_type_bool
}

fn test_return_name_recent_worker_private_storage_and_merge_clear() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	name := 'target'.clone()
	tc.register_generated_fn_return_type(name, types.Type(types.string_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.fn_return_name_cache = new_fn_return_name_cache()
	assert t.checker_fn_return_type_name(name) or { panic('missing return type') } == flat.source_type_string
	wtc := tc.fork_for_parallel_transform(&a)
	w := t.fork_scoped_batch_worker(&a, wtc)
	$if !autofree ? {
		assert voidptr(w.fn_return_name_cache) != voidptr(t.fn_return_name_cache)
	}
	t.merge_worker_signatures(w)
	$if !autofree ? {
		for i in 0 .. 128 {
			assert t.fn_return_name_cache.names[i].len == 0
			assert t.fn_return_name_cache.states[i] == 0
		}
	}
	assert t.checker_fn_return_type_name(name) or { panic('missing return after clear') } == flat.source_type_string
}
