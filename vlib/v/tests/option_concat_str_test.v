fn test_str_concat() {
	opt := ?string(none)
	x := match 1 {
		0 { 'abc' }
		else { 'def' }
	} + opt or { '!!!' }
	assert x == 'def!!!'

	y := opt or { '!!!' } + match 1 {
		0 { 'abc' }
		else { 'def' }
	}
	println(y)
	assert y == '!!!def'
}
