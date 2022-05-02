fn get_arr_v1<N, T>(num N, val T) []T {
	return []T{len: num, init: val}
}

fn get_arr_v2<N, T>(num N, val T) []T {
	return []T{len: int(num), init: val}
}

fn get_arr_v3<N, T>(num N, val T) []T {
	tmp := num
	return []T{len: tmp, init: val}
}

fn get_arr_v4<N, T>(num N, val T) []T {
	tmp := num + 0
	return []T{len: tmp, init: val}
}

fn get_arr_v5<N, T>(num N, val T) []T {
	tmp := 0 + num
	return []T{len: tmp, init: val}
}

fn test_generic_array_init() {
	println(get_arr_v1(2, 'hallo v1'))
	a1 := get_arr_v1(2, 'hallo v1')
	assert a1 == ['hallo v1', 'hallo v1']

	println(get_arr_v2(2, 'hallo v2'))
	a2 := get_arr_v2(2, 'hallo v2')
	assert a2 == ['hallo v2', 'hallo v2']

	println(get_arr_v3(2, 'hallo v3'))
	a3 := get_arr_v3(2, 'hallo v3')
	assert a3 == ['hallo v3', 'hallo v3']

	println(get_arr_v4(2, 'hallo v4'))
	a4 := get_arr_v4(2, 'hallo v4')
	assert a4 == ['hallo v4', 'hallo v4']

	println(get_arr_v5(2, 'hallo v5'))
	a5 := get_arr_v5(2, 'hallo v5')
	assert a5 == ['hallo v5', 'hallo v5']
}
