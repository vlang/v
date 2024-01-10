fn generic[T](t T) string {
	return '${t}'
}

fn test_generic_call_with_fixed_array_arg() {
	r1 := generic[[3]int]([1, 2, 3]!)
	println(r1)
	assert r1 == '[1, 2, 3]'

	r2 := generic[[]int]([1, 2, 3])
	println(r2)
	assert r2 == '[1, 2, 3]'

	r3 := generic[int](22)
	println(r3)
	assert r3 == '22'

	r4 := generic[bool](true)
	println(r4)
	assert r4 == 'true'
}
