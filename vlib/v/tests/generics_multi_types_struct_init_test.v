struct Response<T> {
	result T
}

pub fn send<T>(res T) string {
	msg := Response<T>{
		result: res
	}
	return '${msg}'
}

struct Foo {}

struct Bar {}

fn test_generics_multi_types_struct_init() {
	mut ret := send(Foo{})
	println(ret)
	assert ret.contains('Response<Foo>{')
	assert ret.contains('result: Foo{}')

	ret = send(Bar{})
	println(ret)
	assert ret.contains('Response<Bar>{')
	assert ret.contains('result: Bar{}')

	ret = send(123)
	println(ret)
	assert ret.contains('Response<int>{')
	assert ret.contains('result: 123')

	ret = send('abc')
	println(ret)
	assert ret.contains('Response<string>{')
	assert ret.contains("result: 'abc'")

	ret = send(2.22)
	println(ret)
	assert ret.contains('Response<f64>{')
	assert ret.contains('result: 2.22')
}
