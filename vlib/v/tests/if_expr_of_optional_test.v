fn foo1() ?int {
	return if true { 0 } else { none }
}

fn foo2() ?int {
	return if true { 1 } else { error('foo2 error') }
}

fn foo3() ?int {
	return if false { 1 } else { error('foo3 error') }
}

fn foo4() ?int {
	return if true { 2 } else { 0 }
}

fn test_if_expr_of_optional() {
	a1 := foo1() or { panic('error') }
	println(a1)
	assert a1 == 0

	a2 := foo2() or { panic('error') }
	println(a2)
	assert a2 == 1

	if _ := foo3() {
		assert false
	} else {
		assert err.msg == 'foo3 error'
	}

	a4 := foo4() or { panic('error') }
	println(a4)
	assert a4 == 2
}

fn foo_complex() ?int {
	a := 2
	return if a > 1 {
		mut b := 1
		b *= 10
		b
	} else {
		mut c := 0
		c += 2
		println(c)
		none
	}
}

fn test_if_expr_of_optional_complex() {
	a := foo_complex() or { panic('error') }
	println(a)
	assert a == 10
}
