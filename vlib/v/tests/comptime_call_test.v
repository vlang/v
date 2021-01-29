struct Test {}

fn (test Test) v() {
	println('in v()')
}
fn (test Test) i() int {
	return 4
}
fn (test Test) s() string {
	return 'test'
}

fn test_call() {
	test := Test{}
	sv := 'v'
	test.$sv()
	si := 'i'
	ri := test.$si()
	assert ri == 4
	ss := 's'
	rs := test.$ss()
	assert rs == 'test'
}
