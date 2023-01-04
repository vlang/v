import json

struct Foo {
	name string
	num  ?int
}

fn test_json_encode_struct_with_optional_field() {
	f1 := Foo{
		name: 'hello'
	}
	ret1 := json.encode(f1)
	println(ret1)
	assert ret1 == '{"name":"hello","num":null}'

	f2 := Foo{'hello', 22}
	ret2 := json.encode(f2)
	println(ret2)
	assert ret2 == '{"name":"hello","num":22}'
}
