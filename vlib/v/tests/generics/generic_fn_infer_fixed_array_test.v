fn get_element[T](arr [3]T) string {
	return '${arr[1]}'
}

fn test_generic_fn_infer_fixed_array() {
	a := [1, 2, 3]!
	mut ret := get_element(a)
	println(ret)
	assert ret == '2'

	b := ['a', 'b', 'c']!
	ret = get_element(b)
	println(ret)
	assert ret == 'b'

	c := [1.1, 2.2, 3.3]!
	ret = get_element(c)
	println(ret)
	assert ret == '2.2'

	d := [`a`, `b`, `c`]!
	ret = get_element(d)
	println(ret)
	assert ret == 'b'

	e := [true, false, true]!
	ret = get_element(e)
	println(ret)
	assert ret == 'false'
}
