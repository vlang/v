fn test_inside_ternary() {
	x := if false {
		'foo'
	} else if true {
		'bar'
	} else {
		panic('err')
		'empty'
	}
	assert x == 'bar'
}
