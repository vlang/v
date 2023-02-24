type SumType = int | string

fn template_fn[T](a []T) []T {
	mut b := []T{}
	b << a
	return b
}

fn test_generic_array_of_sumtype_push() {
	ret1 := template_fn([SumType(1)])
	println(ret1)
	assert ret1.len == 1
	assert ret1[0] == SumType(1)

	ret2 := template_fn([SumType('hello')])
	println(ret2)
	assert ret2.len == 1
	assert ret2[0] == SumType('hello')
}
