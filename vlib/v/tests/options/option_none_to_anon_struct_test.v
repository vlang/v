fn opt(params struct { name string surname string }) {
	assert '${params}' == 'Option(none)'
}

fn test_main() {
	opt(none)
}
