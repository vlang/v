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
	return find_startswith_string(a, search) or { find_startswith_string(b, search) }
}

fn test_main() {
	deadbeef := find_any_startswith_string(['foobar', 'barfoo'], ['deadbeef', 'beefdead'],
		'dead')
	assert deadbeef or { panic('unreachable') } == 'deadbeef'
}
