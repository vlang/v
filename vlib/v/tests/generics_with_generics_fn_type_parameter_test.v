fn neg(a int) int {
	return -a
}

fn indirect_call(func fn (int) int, a int) int {
	println(typeof(func).name)
	return func(a)
}

fn generic_call<T>(func T, a int) int {
	println(T.name)
	return func(a)
}

fn generic_indirect_call<T>(func T, a int) int {
	println(T.name)
	return indirect_call(func, a)
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
}
