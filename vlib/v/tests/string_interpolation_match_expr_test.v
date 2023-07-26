fn test_string_interpolation_match_expr() {
	x := 0

	println('Hello ${match x {
		0 { 'John' }
		1 { 'George' }
		else { 'Others' }
	}}')

	assert '${match x {
		0 { 'John' }
		1 { 'George' }
		else { 'Others' }
	}}' == 'John'
}
