fn try(this string) !bool {
	return true
}

fn test_case1() {
	a, b := match true {
		try('foo')! { 'a', 'b' }
		try('bar')! { 'c', 'd' }
		else { 'e', 'f' }
	}
	assert a == 'a'
	assert b == 'b'
}

fn test_case2() {
	a, b := match true {
		try('foo')! && try('foo') or { false } { 'a', 'b' }
		try('bar') or { false } { 'c', 'd' }
		else { 'e', 'f' }
	}
	assert a == 'a'
	assert b == 'b'
}
