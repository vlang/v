struct Test {}

fn (test Test) v() {
	println('Test.v()')
}
fn (test Test) i() int {
	return 4
}
fn (test Test) s() string {
	return 'test'
}
fn (test Test) s2() string {
	return 'Two'
}

fn test_string_identifier() {
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

fn test_for_methods() {
	test := Test{}
	mut r := ''
	$for method in Test.methods {
		// currently the checker thinks all $method calls return string
		$if method.return_type is string {
			//~ $if method.name == 's' {println('yes')}
			v := test.$method()
			r += v.str()
		}
		$else $if method.return_type is int {
			// TODO
			//v := test.$method()
			v := '?'
			r += v.str()
			assert method.name == 'i'
		}
		$else {
			// no return type
			test.$method()
			assert method.name == 'v'
		}
	}
	assert r == '?testTwo'
}
