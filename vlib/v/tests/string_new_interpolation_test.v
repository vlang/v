fn test_string_new_interpolation() {
	a, b, c, d := 1, 2, 3, 4

	println('{a}{b}{c}{d}')
	assert '{a}{b}{c}{d}' == '1234'

	println('{a} {b} {c} {d}')
	assert '{a} {b} {c} {d}' == '1 2 3 4'
}
