import json

enum Foo {
	yay  @[json: 'A'; yay]
	foo  @[foo; json: 'B']
}

struct FooStruct {
	item Foo
}

fn test_comptime() {
	$for f in Foo.values {
		println(f)
		if f.value == Foo.yay {
			assert f.attrs[0] == 'json: A'
			assert f.attrs[1] == 'yay'
		}
		if f.value == Foo.foo {
			assert f.attrs[1] == 'json: B'
			assert f.attrs[0] == 'foo'
		}
	}
}

fn test_json_encode() {
	assert dump(json.encode(Foo.yay)) == '"A"'
	assert dump(json.encode(Foo.foo)) == '"B"'

	assert dump(json.encode(FooStruct{ item: Foo.yay })) == '{"item":"A"}'
	assert dump(json.encode(FooStruct{ item: Foo.foo })) == '{"item":"B"}'
}

fn test_json_decode() {
	dump(json.decode(FooStruct, '{"item": "A"}')!)
	dump(json.decode(FooStruct, '{"item": "B"}')!)
}
