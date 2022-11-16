fn generic<T>(items ...T) string {
	return '${items}'
}

fn test_generic_fn_infer_variadic() {
	ret1 := generic([1, 2, 3], [4, 5, 6])
	println(ret1)
	assert ret1 == '[[1, 2, 3], [4, 5, 6]]'

	ret2 := generic(['a', 'b'], ['c', 'd'])
	println(ret2)
	assert ret2 == "[['a', 'b'], ['c', 'd']]"

	ret3 := generic(1.1, 2.2, 3.3)
	println(ret3)
	assert ret3 == '[1.1, 2.2, 3.3]'
}
