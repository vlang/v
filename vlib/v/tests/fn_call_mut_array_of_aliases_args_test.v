fn f1(mut b []byte) string {
	return '${b}'
}

fn f2(b []byte) string {
	return '${b}'
}

fn f3(mut b []u8) string {
	return '${b}'
}

fn test_fn_call_mut_array_of_aliases() {
	mut s1 := 'test'.bytes()

	ret1 := f1(mut s1)
	println(ret1)
	assert ret1 == '[116, 101, 115, 116]'

	ret2 := f2(s1)
	println(ret2)
	assert ret2 == '[116, 101, 115, 116]'

	ret3 := f3(mut s1)
	println(ret3)
	assert ret3 == '[116, 101, 115, 116]'
}
