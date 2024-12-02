fn find_startswith_string(a []string, search string) ?string {
	for s in a {
		if s.starts_with(search) {
			return s
		}
	}
	return none
}

fn find_any_startswith_string(a []string, b []string, search string) ?string {
	// cannot convert 'struct _option_string' to 'struct string'
	// V wants the or {} block to return a string, but find_startswith_string returns ?string
	return find_startswith_string(a, search) or { find_startswith_string(b, search)? }
}

fn find_any_startswith_string_unwrapped(a []string, b []string, search string) ?string {
	return find_startswith_string(a, search) or { find_startswith_string(b, search)? }
}

fn test_main() {
	var1 := find_any_startswith_string(['foobar', 'barfoo'], ['deadbeef', 'beefdead'],
		'dead')
	assert var1 or { panic('unreachable') } == 'deadbeef'
	dump(var1)

	var2 := find_any_startswith_string_unwrapped(['foobar', 'barfoo'], ['deadbeef', 'beefdead'],
		'dead')
	dump(var2)
	assert var2 != none

	var3 := find_any_startswith_string_unwrapped(['foobar', 'barfoo'], ['deadbeef', 'beefdead'],
		'error')
	dump(var3)
	assert var3 == none
}
