// vtest vflags: -w
import json2

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
	assert json2.encode(Foo{}, escape_unicode: true) == '{"name":null,"age":null,"other2":null}'
	assert json2.encode(Foo{ name: '' }, escape_unicode: true) == '{"name":"","age":null,"other2":null}'
	assert json2.encode(Foo{ age: 10 }, escape_unicode: true) == '{"name":null,"age":10,"other2":null}'
	assert json2.encode(Foo{
		age:    10
		other2: Bar{
			name: none
		}
	}, escape_unicode: true) == '{"name":null,"age":10,"other2":{"name":null}}'
	assert json2.decode[Foo](json2.encode(Foo{}, escape_unicode: true))! == Foo{}
}
