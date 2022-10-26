struct Response<U> {
	result U
}

pub fn send<T>(res T) string {
	msg := Response<T>{
		result: res
	}
	return '${msg}'
}

fn test_generics_struct_init_with_inconsistent_generic_types() {
	mut ret := send(123)
	println(ret)
	assert ret.contains('Response<int>{')
	assert ret.contains('result: 123')

	ret = send('abc')
	println(ret)
	assert ret.contains('Response<string>{')
	assert ret.contains("result: 'abc'")
}
