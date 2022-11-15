struct Info<T> {
	data T
}

fn get_info<T>(res Info<T>) string {
	return '${res}'
}

fn test_generic_struct_to_string() {
	mut ret := get_info(Info<bool>{true})
	println(ret)
	assert ret.contains('data: true')

	ret = get_info(Info<int>{123})
	println(ret)
	assert ret.contains('data: 123')

	ret = get_info(Info<f32>{f32(2.2)})
	println(ret)
	assert ret.contains('data: 2.2')

	ret = get_info(Info<f64>{2.2})
	println(ret)
	assert ret.contains('data: 2.2')

	ret = get_info(Info<string>{'aaa'})
	println(ret)
	assert ret.contains("data: 'aaa'")

	ret = get_info(Info<u64>{u64(234)})
	println(ret)
	assert ret.contains('data: 234')
}
