struct Test {
mut:
	a int
	b string
}

fn (mut test Test) with_single_return() int {
	defer {
		test.a = $res()
	}
	return 41
}

fn (mut test Test) with_multi_return() (int, string) {
	defer {
		test.a = $res(0)
		test.b = $res(1)
	}
	return 41, 'foo'
}

fn test_with_single_return() {
	mut test := Test{
		a: 0
	}
	assert test.with_single_return() == 41
	assert test.a == 41
}

fn test_with_multi_return() {
	mut test := Test{
		a: 0
		b: ''
	}
	a, b := test.with_multi_return()
	assert a == 41
	assert b == 'foo'
	assert test.a == a
	assert test.b == b
}

fn f() int {
	defer {
		a := $res()
		assert true
		println('result is: ${a}')
	}
	if true {
		return 123
	}
	return 42
}

fn test_res_in_defer_blocks_with_many_statements_works() {
	assert f() == 123
}
