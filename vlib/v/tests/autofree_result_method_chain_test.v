// vtest vflags: -autofree
// vtest build: !sanitize-address-gcc && !sanitize-address-clang

fn get_str_a() ?string {
	return 'hello world?'
}

fn process_a(s string) string {
	return s.to_upper()
}

fn get_str_b() !string {
	return 'hello world!'
}

fn process_b(s string) string {
	return s.to_upper()
}

fn test_autofree_chain_a() {
	result := process_a(get_str_a()?.to_upper())
	assert result == 'HELLO WORLD?'
}

fn test_autofree_chain_b() {
	result := process_b(get_str_b()!.to_upper())
	assert result == 'HELLO WORLD!'
}
