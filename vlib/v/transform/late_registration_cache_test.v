module transform

import v.flat
import v.types

// These tests cover the lifecycle of the memoized name lookups: a cached
// negative answer must not survive a later registration, whether that
// registration lands in the transformer table, only in the checker, or in a
// scoped batch worker that is merged back into the master transformer.

fn new_late_registration_transformer(mut a flat.FlatAst, tc &types.TypeChecker) &Transformer {
	mut t := new_transformer(mut a, tc, map[string]bool{})
	t.fn_return_name_cache = new_fn_return_name_cache()
	t.receiver_method_cache = &ReceiverMethodCache{}
	t.cur_module = 'main'
	return t
}

// register_checker_only_signature mirrors generated specializations that are
// published to the checker and the signature log, but not to fn_ret_types.
fn register_checker_only_signature(mut t Transformer, mut tc types.TypeChecker, name string) {
	tc.ensure_private_transform_signatures()
	tc.register_generated_fn_return_type(name, types.Type(types.int_))
	tc.register_generated_fn_param_types(name, []types.Type{})
	t.tc_signature_names_log << name
}

fn test_late_registration_clears_cached_negative_fn_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_late_registration_transformer(mut a, &tc)
	for name in ['late_local', 'late_ret', 'late_params'] {
		for _ in 0 .. 2 {
			assert !t.is_known_fn_name(name), name
		}
	}
	t.set_fn_ret_type('late_local', flat.source_type_int)
	tc.register_generated_fn_return_type('late_ret', types.Type(types.int_))
	tc.register_generated_fn_param_types('late_params', []types.Type{})
	for name in ['late_local', 'late_ret', 'late_params'] {
		for _ in 0 .. 2 {
			assert t.is_known_fn_name(name), name
		}
	}
}

fn test_late_registration_clears_cached_receiver_method_misses() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_late_registration_transformer(mut a, &tc)
	foo := flat.source_type_id('Foo')
	for method in ['local', 'checker'] {
		for _ in 0 .. 2 {
			assert t.resolve_receiver_method_for_type(foo, method) == none, method
		}
	}
	// A checker-only registration leaves fn_ret_types untouched, so it must
	// invalidate the cached miss on its own.
	register_checker_only_signature(mut t, mut tc, 'Foo.checker')
	for _ in 0 .. 2 {
		assert t.resolve_receiver_method_for_type(foo, 'checker') or {
			panic('stale miss for checker')
		} == 'Foo.checker'
	}
	t.set_fn_ret_type('Foo.local', flat.source_type_int)
	for method in ['local', 'checker'] {
		for _ in 0 .. 2 {
			assert t.resolve_receiver_method_for_type(foo, method) or { panic('stale miss for ${method}') } == 'Foo.${method}'
		}
	}
}

fn test_late_registration_in_scoped_batch_reaches_master_caches() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_late_registration_transformer(mut a, &tc)
	foo := flat.source_type_id('Foo')
	fn_names := ['batch_local', 'batch_checker']
	methods := ['local', 'checker']
	// The master caches negative answers before the batch runs.
	for _ in 0 .. 2 {
		for name in fn_names {
			assert !t.is_known_fn_name(name), name
		}
		for method in methods {
			assert t.resolve_receiver_method_for_type(foo, method) == none, method
		}
	}
	mut wtc := tc.fork_for_parallel_transform(&a)
	mut w := t.fork_scoped_batch_worker(&a, wtc)
	$if !autofree ? {
		assert voidptr(w.fn_return_name_cache) != voidptr(t.fn_return_name_cache)
		assert voidptr(w.receiver_method_cache) != voidptr(t.receiver_method_cache)
	}
	w.cur_module = 'main'
	// The worker caches the same misses, then registers the names mid-batch.
	for _ in 0 .. 2 {
		for name in fn_names {
			assert !w.is_known_fn_name(name), name
		}
		for method in methods {
			assert w.resolve_receiver_method_for_type(foo, method) == none, method
		}
	}
	w.set_fn_ret_type('batch_local', flat.source_type_int)
	w.set_fn_ret_type('Foo.local', flat.source_type_int)
	register_checker_only_signature(mut w, mut wtc, 'batch_checker')
	register_checker_only_signature(mut w, mut wtc, 'Foo.checker')
	for name in fn_names {
		assert w.is_known_fn_name(name), name
	}
	for method in methods {
		assert w.resolve_receiver_method_for_type(foo, method) or { panic('worker stale miss for ${method}') } == 'Foo.${method}'
	}
	// Until the batch is merged, the master keeps its own view.
	for name in fn_names {
		assert !t.is_known_fn_name(name), name
	}
	t.merge_worker_signatures(w)
	for _ in 0 .. 2 {
		for name in fn_names {
			assert t.is_known_fn_name(name), name
		}
		for method in methods {
			assert t.resolve_receiver_method_for_type(foo, method) or { panic('master stale miss for ${method}') } == 'Foo.${method}'
		}
	}
	// A worker of the next batch starts from the merged signatures.
	wtc2 := tc.fork_for_parallel_transform(&a)
	mut w2 := t.fork_scoped_batch_worker(&a, wtc2)
	w2.cur_module = 'main'
	for name in fn_names {
		assert w2.is_known_fn_name(name), name
	}
	for method in methods {
		assert w2.resolve_receiver_method_for_type(foo, method) or { panic('next batch miss for ${method}') } == 'Foo.${method}'
	}
}

fn test_late_checker_only_registration_in_scoped_batch_reaches_master_caches() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_late_registration_transformer(mut a, &tc)
	foo := flat.source_type_id('Foo')
	for _ in 0 .. 2 {
		assert !t.is_known_fn_name('batch_only_checker')
		assert t.resolve_receiver_method_for_type(foo, 'only_checker') == none
	}
	// The worker publishes through the checker only, so neither the worker nor
	// the merge grows fn_ret_types.
	mut wtc := tc.fork_for_parallel_transform(&a)
	mut w := t.fork_scoped_batch_worker(&a, wtc)
	w.cur_module = 'main'
	assert w.resolve_receiver_method_for_type(foo, 'only_checker') == none
	fn_count := t.fn_ret_types.ids.len
	register_checker_only_signature(mut w, mut wtc, 'batch_only_checker')
	register_checker_only_signature(mut w, mut wtc, 'Foo.only_checker')
	assert w.fn_ret_types.ids.len == fn_count
	assert w.resolve_receiver_method_for_type(foo, 'only_checker') or {
		panic('worker stale miss for only_checker')
	} == 'Foo.only_checker'
	t.merge_worker_signatures(w)
	assert t.fn_ret_types.ids.len == fn_count
	for _ in 0 .. 2 {
		assert t.is_known_fn_name('batch_only_checker')
		assert t.resolve_receiver_method_for_type(foo, 'only_checker') or {
			panic('master stale miss for only_checker')
		} == 'Foo.only_checker'
	}
}

fn test_late_registration_cached_answers_follow_module_switches() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_late_registration_transformer(mut a, &tc)
	foo := flat.source_type_id('Foo')
	assert t.resolve_receiver_method_for_type(foo, 'switch') == none
	t.cur_module = 'other'
	assert t.resolve_receiver_method_for_type(foo, 'switch') == none
	t.set_fn_ret_type('Foo.switch', flat.source_type_int)
	t.cur_module = 'main'
	assert t.resolve_receiver_method_for_type(foo, 'switch') or { panic('stale miss after module switch') } == 'Foo.switch'
	// clear_fn_return_name_cache drops the direct-mapped front, but positive
	// answers must still be served from the backing entries afterwards.
	t.clear_fn_return_name_cache()
	assert t.resolve_receiver_method_for_type(foo, 'switch') or { panic('lost entry after clear') } == 'Foo.switch'
	assert t.is_known_fn_name('Foo.switch')
}
