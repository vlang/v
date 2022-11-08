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

	println('{n:10}')
	assert '{n:10}' == '        22'

	f := 2.234
	println('{f:05.2f}')
	assert '{f:05.2f}' == '02.23'

	println('{@FILE}')
	assert '{@FILE}'.contains('string_new_interpolation_test.v')

	ret := foo()
	println(ret)
	assert ret == r'[]T{aaa, bbb, ccc}'
}

fn foo() string {
	match true {
		true {
			fields := ['aaa', 'bbb', 'ccc']
			return '[]T{{fields.join(', ')}}'
		}
		else {
			fields := ['aaa', 'bbb', 'ccc']
			return 'const ({fields.join(' ')})'
		}
	}
}
