import json

struct Bar {
	name ?string @[json_null]
}

struct Foo {
	name   ?string @[json_null]
	age    ?int    @[json_null]
	text   ?string
	other  ?Bar
	other2 ?Bar @[json_null]
}

fn test_main() {
	assert json.encode(Foo{}) == '{"name":null,"age":null,"other2":null}'
	assert json.encode(Foo{ name: '' }) == '{"name":"","age":null,"other2":null}'
	assert json.encode(Foo{ age: 10 }) == '{"name":null,"age":10,"other2":null}'
	assert json.encode(Foo{
		age:    10
		other2: Bar{
			name: none
		}
	}) == '{"name":null,"age":10,"other2":{"name":null}}'
	assert json.decode(Foo, json.encode(Foo{}))! == Foo{}
}
