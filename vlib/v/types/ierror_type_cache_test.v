module types

import v.flat

fn test_ierror_cache_owner_and_scoped_payload_context() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'first.v'
	tc.cur_module = 'pkg'
	tc.structs[flat.source_type_id('pkg.Payload')] = []StructField{}
	payload := tc.ierror_compat_cache_key(flat.source_type_id('Payload'))
	assert payload.owner_id == flat.source_type_id('pkg.Payload')
	assert payload.scope.len == 0
	first := tc.ierror_compat_cache_key(flat.source_type_id('Error'))
	assert first.owner_id == flat.source_type_id('Error')
	tc.cur_file = 'second.v'
	assert tc.ierror_compat_cache_key(flat.source_type_id('Error')).scope != first.scope
	tc.cur_file = 'first.v'
	tc.cur_module = 'other'
	assert tc.ierror_compat_cache_key(flat.source_type_id('Error')).scope != first.scope
	assert tc.ierror_compat_cache_key(flat.source_type_id('pkg.Error')).scope.len == 0
	tc.cur_file = ''
	tc.cur_module = ''
	assert tc.ierror_compat_cache_key(flat.source_type_id('MessageError')).scope.len > 0
}

fn test_ierror_cache_base_priority_and_overlay_merge_own_scoped_entries() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'query.v'
	tc.cur_module = 'main'
	error_id := flat.source_type_id('Error')
	payload_id := flat.source_type_id('Payload')
	error_key := tc.ierror_compat_cache_key(error_id)
	payload_key := tc.ierror_compat_cache_key(payload_id)
	mut base := new_type_cache(false)
	base.remember_ierror_compat_entry(error_key, -1)
	base.remember_ierror_compat_entry(payload_key, 1)
	tc.type_cache = new_type_cache_with_base(false, base)
	mut overlay := tc.type_cache
	overlay.remember_ierror_compat_entry(error_key, 1)
	overlay.remember_ierror_compat_entry(payload_key, -1)
	assert !tc.named_type_compatible_with_ierror(error_id)
	assert tc.named_type_compatible_with_ierror(payload_id)
	tc.restore_type_cache_base()
	assert tc.named_type_compatible_with_ierror(error_id)
	assert !tc.named_type_compatible_with_ierror(payload_id)
	// Clearing the old overlay must not release the parent's merged context table.
	overlay.clear_ierror_compat_entries()
	assert tc.named_type_compatible_with_ierror(error_id)
}

fn test_source_error_embed_keys_keep_qualified_and_file_scope_rules() {
	owner_id := flat.source_type_id('Holder')
	local := source_error_embed_lookup_key(owner_id, 'holder.v', '')
	assert local == source_error_embed_entry_key(owner_id, 'holder.v', '')
	assert source_error_embed_lookup_key(owner_id, 'holder.v', 'main') == source_error_embed_entry_key(owner_id, '', '')
	assert source_error_embed_lookup_key(flat.source_type_id('main.Holder'), 'other.v', 'other') == source_error_embed_entry_key(owner_id, '', '')
	assert source_error_embed_lookup_key(flat.source_type_id('pkg.Holder'), 'holder.v', 'other') == source_error_embed_entry_key(owner_id, '', 'pkg')
	assert source_error_embed_lookup_key(owner_id, 'holder.v', 'builtin') == source_error_embed_entry_key(owner_id, '', 'builtin')
}

fn test_source_error_embed_worker_cleanup_keeps_frozen_owner_tables() {
	owner_id := flat.source_type_id('Holder')
	key := source_error_embed_entry_key(owner_id, 'holder.v', '')
	mut frozen := new_type_cache(false)
	remember_type_query_scope_entry(mut frozen.source_error_embed_entries, key, 1)
	frozen.source_error_embed_indexed = true
	mut worker := new_type_cache(false)
	worker.source_error_embed_entries = frozen.source_error_embed_entries
	worker.source_error_embed_indexed = true
	worker.source_error_embed_shared = true
	worker.free_source_error_embed_entries()
	assert (type_query_scope_entry(frozen.source_error_embed_entries, key) or { 0 }) == 1
	assert !worker.source_error_embed_indexed
}

fn test_struct_error_shadow_flags_use_exact_source_identity() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'pkg'
	shadow_id := flat.source_type_id('pkg.Shadowed')
	other_id := flat.source_type_id('other.Shadowed')
	tc.set_struct_fields(shadow_id, []StructField{}, []string{}, []string{}, map[string]string{}, true)
	tc.set_struct_fields(other_id, []StructField{}, []string{}, []string{}, map[string]string{}, false)
	assert tc.struct_error_embeds_shadow_builtin[shadow_id]
	assert !tc.struct_error_embeds_shadow_builtin[other_id]
	assert !tc.struct_error_embeds_shadow_builtin[flat.source_type_id('&pkg.Shadowed')]
	assert tc.named_type_has_non_builtin_error_embed(shadow_id)
	// Qualification must probe the resolved spelling ID, retaining the same
	// module lookup that the previous string-keyed metadata used.
	assert tc.named_type_has_non_builtin_error_embed(flat.source_type_id('Shadowed'))
	assert !tc.named_type_has_non_builtin_error_embed(other_id)
	worker := tc.fork_for_parallel_codegen()
	assert worker.struct_error_embeds_shadow_builtin[shadow_id]
	assert worker.named_type_has_non_builtin_error_embed(shadow_id)
	// Numeric keys and scalar values need only a map clone when promoting the
	// side metadata out of an arena; its payload contains no borrowed strings.
	cloned := tc.struct_error_embeds_shadow_builtin.clone()
	tc.struct_error_embeds_shadow_builtin.clear()
	assert cloned[shadow_id]
	assert !cloned[other_id]
}
