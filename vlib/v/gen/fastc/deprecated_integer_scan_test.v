module fastc

import v.pref

fn mentions_deprecated_integer(source string) bool {
	return fastc_source_uses_deprecated_integer(source, 'test.v', pref.new_preferences())
}

fn test_a_real_use_is_found_wherever_a_type_can_be_written() {
	for source in [
		'fn f() { x := usize(1) }',
		'fn f() { x := isize(-1) }',
		'fn f(x usize) {}',
		'fn f() isize { return 0 }',
		'fn (x usize) m() {}',
		'struct S { x []usize }',
		'type A = map[string]isize',
		'fn f() { g[Box[usize]]() }',
		"fn f() { s := 'a \${usize(1)} b' }",
		'fn f() { s := "\${\'in \${isize(3)}\'}" }',
	] {
		assert mentions_deprecated_integer(source), source
	}
}

fn test_text_that_only_mentions_the_names_is_not_a_use() {
	for source in [
		'fn f() {}',
		'// usize\nfn f() {}',
		'/* isize */ fn f() {}',
		"fn f() { s := 'usize' }",
		'fn f() { s := "isize" }',
		"fn f() { s := r'usize' }",
		"fn f() { s := 'a \${b} usize' }",
		'fn usize_of() {}',
		'fn f() { my_isize := 1 }',
	] {
		assert !mentions_deprecated_integer(source), source
	}
}

fn test_a_source_the_scanner_rejects_still_goes_to_the_parser() {
	assert mentions_deprecated_integer("// usize\nfn f() { s := 'unterminated }")
}
