fn test() (string, string) {
	return 'foo', 'bar'
}

fn main() {
	mut a := ?string(none)
	mut b := ''
	a, b = test()
	assert a? == 'foo'
	assert b == 'bar'
}
