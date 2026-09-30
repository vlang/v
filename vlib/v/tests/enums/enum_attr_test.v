// vtest vflags: -w
import json2

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
			assert f.attrs[0] == "json: 'A'"
			assert f.attrs[1] == 'yay'
		}
		if f.value == Foo.foo {
			assert f.attrs[1] == "json: 'B'"
			assert f.attrs[0] == 'foo'
		}
	}
}

fn test_json_encode() {
	assert dump(json2.encode(Foo.yay, escape_unicode: true)) == '"A"'
	assert dump(json2.encode(Foo.foo, escape_unicode: true)) == '"B"'

	assert dump(json2.encode(FooStruct{ item: Foo.yay }, escape_unicode: true)) == '{"item":"A"}'
	assert dump(json2.encode(FooStruct{ item: Foo.foo }, escape_unicode: true)) == '{"item":"B"}'
}

fn test_json_decode() {
	dump(json2.decode[FooStruct]('{"item": "A"}')!)
	dump(json2.decode[FooStruct]('{"item": "B"}')!)
}
