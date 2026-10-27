module transform

import v.flat
import v.types

fn test_scoped_signature_writes_detach_prepared_bindings_and_survive_release() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.fn_ret_types.set_id('existing', flat.source_type_int)
	master.globals.set_id('global_value', flat.source_type_u64)
	master.qualified_types.set_id('Record', flat.source_type_id('sample.Record'))
	mut helper := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	peer := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))

	scope := transform_worker_scope_begin(true)
	batch := helper.fork_scoped_batch_worker(&a, helper.tc.fork_for_parallel_transform(&a))
	mut nested := batch.fork_scoped_batch_worker(&a, batch.tc.fork_for_parallel_transform(&a))
	assert voidptr(nested.fn_ret_types.texts) != voidptr(helper.fn_ret_types.texts)
	assert voidptr(nested.globals.texts) != voidptr(master.globals.texts)
	assert voidptr(nested.qualified_types.texts) != voidptr(master.qualified_types.texts)
	// Inserting enough generated signatures to grow the map must not rehash a
	// parent's or peer's prepared binding storage, even through nested forks.
	for i in 0 .. 256 {
		nested.set_fn_ret_type('generated_${i}', flat.source_type_string)
	}
	nested.set_fn_ret_type('existing', flat.source_type_bool)
	assert batch.fn_ret_types.value_id('existing') == flat.source_type_int
	assert helper.fn_ret_types.ids.len == 1
	assert master.fn_ret_types.ids.len == 1
	assert peer.fn_ret_types.ids.len == 1
	transform_worker_scope_leave(scope)
	helper.absorb_scoped_batch(nested, scope, a.nodes.len)
	transform_worker_scope_free(scope)

	// Publication owns the generated keys; a following batch can use them after
	// the producing arena has been retired, without changing a parallel peer.
	for i in 0 .. 256 {
		assert helper.fn_ret_types.value_id('generated_${i}') == flat.source_type_string
		assert !master.fn_ret_types.contains('generated_${i}')
		assert !peer.fn_ret_types.contains('generated_${i}')
	}
	next := helper.fork_scoped_batch_worker(&a, helper.tc.fork_for_parallel_transform(&a))
	assert next.fn_ret_types.value_id('generated_255') == flat.source_type_string
	assert next.globals.value_id('global_value') == flat.source_type_u64
	assert flat.source_type_name(next.qualified_types.value_id('Record')) == 'sample.Record'
	assert master.fn_ret_types.value_id('existing') == flat.source_type_int
	assert peer.fn_ret_types.value_id('existing') == flat.source_type_int
}

fn test_alias_cache_rebinds_equal_content_type_text_owners() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	alias_id := flat.source_type_id('CacheOwnerAlias')
	tc.type_aliases.set_id(flat.source_type_id('CacheOwnerAlias'), flat.source_type_int)
	parent := &flat.TypeTextTable{}
	fork := parent.fork()
	assert voidptr(parent) != voidptr(fork)
	// Both tables have identical empty lookup caches. Their global spelling IDs
	// agree, but switching owners must discard the old owner's disposable entry.
	mut cache := &AliasCache{
		texts: parent
	}
	cache.entries[alias_id] = flat.source_type_string
	t := Transformer{
		tc:             &tc
		var_type_texts: fork
		alias_cache:    cache
	}
	assert t.normalize_type_alias_cached(alias_id) == flat.source_type_int
	assert voidptr(cache.texts) == voidptr(fork)
	assert cache.entries[alias_id] == flat.source_type_int
}
