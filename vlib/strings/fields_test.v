module strings

fn test_fields() {
	assert fields('  a b  c ') == ['a', 'b', 'c']
	assert fields('') == []
	assert fields('   ') == []
}

fn test_fields_func() {
	f := fn (r rune) bool { return r == `,` }
	assert fields_func('a,b,,c', f) == ['a', 'b', 'c']
}
