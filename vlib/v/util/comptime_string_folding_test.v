module util

fn must_scalar(receiver string, method string, args []string) ComptimeStringScalar {
	scalar := comptime_string_scalar(receiver, method, args) or { return ComptimeStringScalar{} }
	assert scalar.typ_text() in ['string', 'bool', 'int']
	return scalar
}

fn folded(receiver string, method string, args []string) string {
	return must_scalar(receiver, method, args).value
}

// The result of a pure string operation is folded by name, so a method that is
// not in the table has to come back as none rather than a wrong value.
fn test_comptime_string_scalar_folds_the_zero_argument_methods() {
	scalar := must_scalar('  Ab  ', 'trim_space', [])
	assert scalar.typ_text() == 'string'
	assert scalar.value == 'Ab'
	assert folded('Ab', 'to_lower', []) == 'ab'
	assert folded('Ab', 'to_upper', []) == 'AB'
	assert comptime_string_scalar('Ab', 'len', []) == none
	assert comptime_string_scalar('Ab', '', []) == none
}

fn test_comptime_string_scalar_folds_the_predicate_methods_as_bool() {
	scalar := must_scalar('abc', 'contains', ['b'])
	assert scalar.typ_text() == 'bool'
	assert scalar.value == 'true'
	assert folded('abc', 'starts_with', ['a']) == 'true'
	assert folded('abc', 'ends_with', ['c']) == 'true'
	assert folded('abc', 'starts_with', ['b']) == 'false'
	assert folded('abc', 'ends_with', ['b']) == 'false'
	assert folded('abc', 'contains', ['d']) == 'false'
}

fn test_comptime_string_scalar_folds_replace_and_count() {
	scalar := must_scalar('a.b.c', 'replace', ['.', '/'])
	assert scalar.typ_text() == 'string'
	assert scalar.value == 'a/b/c'
	count := must_scalar('a.b.c', 'count', ['.'])
	assert count.typ_text() == 'int'
	assert count.value == '2'
	assert folded('abc', 'count', ['x']) == '0'
	// `replace` is the one two-argument method; anything else with two is none.
	assert comptime_string_scalar('a', 'count', ['a', 'b']) == none
	assert comptime_string_scalar('a', 'replace', ['a']) == none
	assert comptime_string_scalar('a', 'replace', ['a', 'b', 'c']) == none
	assert comptime_string_scalar('a', 'unknown', ['b']) == none
}

fn test_comptime_string_scalar_folds_the_substring_methods() {
	expected := {
		'all_before':        'a'
		'all_after':         'b.c'
		'all_before_last':   'a.b'
		'all_after_last':    'c'
		'trim':              'a.b.c'
		'trim_left':         'a.b.c'
		'trim_right':        'a.b.c'
		'trim_string_left':  'a.b.c'
		'trim_string_right': 'a.b.c'
	}
	for method, value in expected {
		assert folded('a.b.c', method, ['.']) == value
	}
	assert comptime_string_scalar('a.b.c', 'substr', ['.']) == none
	assert folded('a.b.c', 'trim_string_left', ['a.']) == 'b.c'
	assert folded('a.b.c', 'trim_string_right', ['.c']) == 'a.b'
	assert folded('a.b.c', 'all_before', ['x']) == 'a.b.c'
}

// A bound that does not name a whole integer must not be silently truncated to
// int, which is what a slice with a 64-bit bound would otherwise do.
fn test_comptime_string_bound_accepts_only_exact_integers() {
	assert comptime_string_bound('12') or { -1 } == 12
	assert comptime_string_bound('1_000') or { -1 } == 1000
	assert comptime_string_bound('-7') or { 0 } == -7
	assert comptime_string_bound('+7') or { 0 } == 7
	assert comptime_string_bound('0x10') or { -1 } == 16
	assert comptime_string_bound('0b101') or { -1 } == 5
	assert comptime_string_bound('0o17') or { -1 } == 15
	assert comptime_string_bound('2147483647') or { -1 } == 2147483647
	assert comptime_string_bound('-2147483648') or { 0 } == -2147483648
	for raw in ['', 'abc', '12abc', '1.5', '2147483648', '-2147483649', '0x', '--1', ' 1', '1 '] {
		assert comptime_string_bound(raw) == none, '`${raw}` should not parse as a bound'
	}
}
