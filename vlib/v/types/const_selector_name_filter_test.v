module types

import v.flat

fn assert_const_selector_filter_matches_tables(tc &TypeChecker, names []string) {
	for name in names {
		expected := tc.const_suffix_key_count == tc.const_types.len
			&& name !in tc.const_suffixes && name !in tc.const_types
		assert tc.selector_names_no_const(name) == expected, name
	}
}

fn test_const_selector_filter_preserves_bare_qualified_and_ambiguous_names() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for name in ['bare', 'one.limit', 'two.limit', 'deep.module.width', 'éclair', '_hidden'] {
		tc.const_types[name] = builtin_int_type
	}
	// Existing suffix entries must remain visible even when not derived from this collection.
	tc.const_suffixes['old_suffix'] = 'prior.old_suffix'
	tc.build_const_suffixes()
	assert tc.const_suffixes['limit'] == ''
	assert !tc.selector_names_no_const('old_suffix')
	for name, _ in tc.const_types {
		assert !tc.selector_names_no_const(name), name
	}
	for name, _ in tc.const_suffixes {
		assert !tc.selector_names_no_const(name), name
	}
	mut names := ['bare', 'limit', 'width', 'module.width', 'deep.module.width', 'one.limit',
		'old_suffix', '', 'éclair', '_hidden', 'runtime_local']
	for i in 0 .. 1000 {
		names << 'absent_${i}'
	}
	assert_const_selector_filter_matches_tables(tc, names)
	fork := tc.fork_for_parallel_codegen()
	assert fork.const_selector_bloom == tc.const_selector_bloom
	assert fork.const_selector_suffix_count == tc.const_suffixes.len
	assert_const_selector_filter_matches_tables(fork, names)
}

fn test_const_selector_filter_falls_back_after_collection_changes() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.const_types['pkg.first'] = builtin_int_type
	tc.build_const_suffixes()
	tc.const_suffixes['added_suffix'] = 'pkg.added_suffix'
	assert !tc.selector_names_no_const('added_suffix')
	assert_const_selector_filter_matches_tables(tc, ['first', 'added_suffix', 'missing'])
	tc.const_types['pkg.second'] = builtin_int_type
	assert !tc.selector_names_no_const('second')
	assert !tc.selector_names_no_const('missing')
	tc.build_const_suffixes()
	assert_const_selector_filter_matches_tables(tc, ['first', 'second', 'added_suffix', 'missing'])
	// Value changes preserve the key set and may continue to use the snapshot.
	tc.const_types['pkg.first'] = builtin_string_type
	assert_const_selector_filter_matches_tables(tc, ['first', 'second', 'missing'])
	tc.const_types.delete('pkg.second')
	assert !tc.selector_names_no_const('missing')
	tc.build_const_suffixes()
	assert_const_selector_filter_matches_tables(tc, ['first', 'second', 'added_suffix', 'missing'])
}

fn test_const_selector_filter_rejects_most_absent_names_in_large_tables() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for i in 0 .. 4000 {
		tc.const_types['mod_${i % 50}.const_${i}'] = builtin_int_type
	}
	tc.build_const_suffixes()
	mut names := []string{}
	for i in 0 .. 1000 {
		names << 'field_${i}'
	}
	names << ['const_7', 'mod_3.const_103', 'const_3999', 'mod_1.const_3999']
	assert_const_selector_filter_matches_tables(tc, names)
	mut rejected := 0
	for i in 0 .. 1000 {
		if tc.const_selector_bloom.excludes('field_${i}', tc.const_types.len) {
			rejected++
		}
	}
	assert rejected > 900, 'rejected only ${rejected} of 1000 absent names'
}
