fn test_string_new_interpolation() {
	a, b, c, d := 1, 2, 3, 4

	println('{a}{b}{c}{d}')
	assert '{a}{b}{c}{d}' == '1234'

	println('{a} {b} {c} {d}')
	assert '{a} {b} {c} {d}' == '1 2 3 4'

	println('{a}{{b}}')
	assert '{a}{{b}}' == '1{2}'

	println('{a}\{{b}}')
	assert '{a}\{{b}}' == '1{2}'

	println('{a}{{{{{b}}}}}')
	assert '{a}{{{{{b}}}}}' == '1{{{{2}}}}'

	s := 'hello'
	println('{s == 'hello'}')
	assert '{s == 'hello'}' == 'true'
	println('{s != 'hello'}')
	assert '{s != 'hello'}' == 'false'

	n := 22
	println('{n >= 10}')
	assert '{n >= 10}' == 'true'
	println('{n <= 10}')
	assert '{n <= 10}' == 'false'
}
