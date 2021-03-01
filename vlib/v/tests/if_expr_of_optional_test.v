fn foo1() ?int {
    return if true { 0 } else { none }
}

fn foo2() ?int {
    return if true { 1 } else { error('foo2 error') }
}

fn foo3() ?int {
	return if true { 2 } else { 0 }
}

fn test_if_expr_of_optional() {
	a1 := foo1() or { panic('error') }
	println(a1)
	assert a1 == 0

	a2 := foo2() or { panic('error') }
	println(a2)
	assert a2 == 1

	a3 := foo3() or { panic('error') }
	println(a3)
	assert a3 == 2
}
