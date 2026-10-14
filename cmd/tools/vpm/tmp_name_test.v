module main

import os

// These mirror what version_range_test.v asserts about version_tmp_name, run
// here so the clang-dependent parts of that suite cannot hide a regression.

fn test_exact_refs_pass_through_unchanged() {
	for v in ['v1.0.0', '1.0.0', 'v0.1.50', 'v1.2.3-beta.1', 'main', 'HEAD'] {
		assert version_tmp_name(v) == v, 'exact ref `${v}` was rewritten to `${version_tmp_name(v)}`'
	}
}

fn test_range_components_hold_no_range_operators() {
	for c in ['^1.0', '~1.2', '>=1.0 <2.0', '*', 'x', 'X', '1.x', '1.X', '1.2.x', '1.2.X'] {
		tmp := version_tmp_name(c)
		assert !tmp.contains_any('<>^~|*/\\ \t'), 'constraint `${c}` produced `${tmp}`'
		assert tmp.len > 'range-'.len, 'constraint `${c}` produced an empty digest'
	}
}

fn test_range_components_are_bounded_in_length() {
	// The reason for the change: a long component pushes the temp path over MAX_PATH.
	for c in ['^1.0', '>=1.0 <2.0', '1.2.X'] {
		tmp := version_tmp_name(c)
		assert tmp.len <= 6 + tmp_name_length, 'constraint `${c}` produced a ${tmp.len}-character component `${tmp}`'
	}
}

fn test_distinct_ranges_still_get_distinct_components() {
	assert version_tmp_name('^1') != version_tmp_name('^2')
	assert version_tmp_name('^1.0') != version_tmp_name('~1.0')
	assert version_tmp_name('>=1.0 <2.0') != version_tmp_name('>=1.0 <3.0')
}

fn test_a_long_hex_ref_is_bounded() {
	long := '0769d565c503c3885f4b48b7a7e87648b660bf66aae632430b74e13efd5b60c4'
	tmp := version_tmp_name(long)
	assert tmp.len == tmp_name_length, 'a 64-character SHA produced a ${tmp.len}-character component'
	assert tmp == version_tmp_name(long)
}

fn test_long_hex_refs_with_the_same_prefix_have_distinct_components() {
	assert version_tmp_name('deadbeef0000aaaa') != version_tmp_name('deadbeef0000bbbb')
}

fn test_the_digest_is_stable() {
	assert version_tmp_name('^1.0') == version_tmp_name('^1.0')
}

fn test_resolver_namespaces_keep_randomness_after_a_shared_timestamp() {
	a := '01ARZ3NDEK0000000000000000'
	b := '01ARZ3NDEK0000000000000001'
	assert a[..tmp_name_length] == b[..tmp_name_length]
	first := resolver_tmp_namespace(a)
	second := resolver_tmp_namespace(b)
	assert first != second
	assert os.file_name(first).len == tmp_name_length
	assert os.file_name(second).len == tmp_name_length
	assert os.dir(first) == 'resolver'
}
