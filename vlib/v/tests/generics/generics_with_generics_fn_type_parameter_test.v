fn neg(a int) int {
	return -a
}

fn indirect_call(func fn (int) int, a int) int {
	println(typeof(func).name)
	assert typeof(func).name == typeof(neg).name
	return func(a)
}

fn generic_call[T](func T, a int) int {
	println(T.name)
	assert T.name == typeof(neg).name
	return func(a)
}

fn generic_indirect_call[T](func T, a int) int {
	println(T.name)
	assert T.name == typeof(neg).name
	return indirect_call(func, a)
}

fn indirect_call_v2(func fn (int) int, a int) int {
	f := func
	assert typeof(f).name == typeof(neg).name
	return f(a)
}

fn generic_call_v2[T](func T, a int) int {
	f := func
	assert typeof(f).name == typeof(neg).name
	return f(a)
}

fn generic_indirect_call_v2[T](func T, a int) int {
	f := func
	assert typeof(f).name == typeof(neg).name
	return indirect_call_v2(f, a)
}

fn test_generics_with_generics_fn_type_parameter() {
	mut ret := 0

	ret = indirect_call(neg, 2)
	println(ret)
	assert ret == -2

	ret = generic_call(neg, 3)
	println(ret)
	assert ret == -3

	ret = generic_indirect_call(neg, 4)
	println(ret)
	assert ret == -4

	ret = indirect_call_v2(neg, 5)
	println(ret)
	assert ret == -5

	ret = generic_call_v2(neg, 6)
	println(ret)
	assert ret == -6

	ret = generic_indirect_call_v2(neg, 7)
	println(ret)
	assert ret == -7
}
