fn t() (string, string) {
	return 'foo', 'bar'
}

fn test_main() {
	mut a := ?string(none)
	mut b := ''
	a, b = t()
	assert a? == 'foo'
	assert b == 'bar'
}
