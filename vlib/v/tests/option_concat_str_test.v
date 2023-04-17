fn test_str_concat() {
	opt := ?string(none)
	x := match 1 {
		0 { 'abc' }
		else { 'def' }
	} + opt or { '!!!' }
	println(x)

	y := opt or { '!!!' } + match 1 {
		0 { 'abc' }
		else { 'def' }
	}
	println(y)
}
