import json
import x.json2

struct Foo {
	name string
	num  ?int
}

fn test_json_encode_struct_with_option_field() {
	f1 := Foo{
		name: 'hello'
	}
	ret1 := json.encode(f1)
	println(ret1)
	assert ret1 == '{"name":"hello"}'

	ret2 := json2.encode(f1)
	println(ret2)
	assert ret2 == '{"name":"hello"}'

	f2 := Foo{'hello', 22}
	ret3 := json.encode(f2)
	println(ret3)
	assert ret3 == '{"name":"hello","num":22}'

	ret4 := json2.encode(f2)
	println(ret4)
	assert ret4 == '{"name":"hello","num":22}'
}
