module types

import v.flat

fn test_recent_sum_pattern_context_is_invalidated_by_clear() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'main.v'
	tc.cur_module = 'main'
	tc.sum_types.set(flat.source_type_id('Choice'), ['string'])
	sum_id := flat.source_type_id('Choice')
	variant_id := flat.source_type_int
	assert tc.sum_variant_type_for_pattern(sum_id, variant_id) == none
	assert !isnil(tc.type_cache.sum_variant_pattern_recent)
	tc.sum_types.set(flat.source_type_id('Choice'), ['int', 'string'])
	tc.type_cache.sum_variant_pattern_entries.clear()
	assert tc.sum_variant_type_for_pattern(sum_id, variant_id)? == variant_id
	assert tc.type_cache.sum_variant_pattern_recent_count == 1
}

fn test_recent_sum_pattern_context_clear_and_repopulate_preserves_other_files() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.sum_types.set(flat.source_type_id('Choice'), ['string'])
	sum_id := flat.source_type_id('Choice')
	variant_id := flat.source_type_int
	tc.cur_file = 'first.v'
	assert tc.sum_variant_type_for_pattern(sum_id, variant_id) == none
	tc.sum_types.set(flat.source_type_id('Choice'), ['int', 'string'])
	tc.type_cache.sum_variant_pattern_entries.clear()
	tc.cur_file = 'second.v'
	assert tc.sum_variant_type_for_pattern(sum_id, variant_id)? == variant_id
	assert tc.type_cache.sum_variant_pattern_entries.len == 1
	tc.cur_file = 'first.v'
	assert tc.sum_variant_type_for_pattern(sum_id, variant_id)? == variant_id
	assert tc.type_cache.sum_variant_pattern_entries.len == 2
}

fn test_recent_sum_pattern_context_owns_file_and_module_guard_strings() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	mut file := 'first.v'.clone()
	mut mod_name := 'one'.clone()
	tc.cur_file = file
	tc.cur_module = mod_name
	tc.structs[flat.source_type_id('one.Item')] = []StructField{}
	tc.structs[flat.source_type_id('two.Item')] = []StructField{}
	tc.sum_types.set(flat.source_type_id('one.Choice'), ['one.Item'])
	tc.sum_types.set(flat.source_type_id('two.Choice'), ['two.Item'])
	sum_id := flat.source_type_id('Choice')
	variant_id := flat.source_type_id('Item')
	assert tc.sum_variant_type_for_pattern(sum_id, variant_id)? == flat.source_type_id('one.Item')
	// The directory owns the guard strings independently of the caller's spelling storage.
	unsafe {
		file.str[0] = `s`
		mod_name.str[0] = `t`
		mod_name.str[1] = `w`
		mod_name.str[2] = `o`
	}
	assert tc.sum_variant_type_for_pattern(sum_id, variant_id)? == flat.source_type_id('two.Item')
	assert tc.type_cache.sum_variant_pattern_entries.len == 2
	assert tc.type_cache.sum_variant_pattern_recent.file == 'sirst.v'
	assert tc.type_cache.sum_variant_pattern_recent.module == 'two'
}
