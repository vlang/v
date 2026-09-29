// vtest vflags: -w
import json2

struct Test {
	field MySumType
}

type MyInt = int
type MyString = string
type MySumType = MyString | int | string

struct UnicodeString {
	emoji string
}

fn test_alias_to_primitive() {
	mut test := Test{
		field: MyString('foo')
	}
	mut encoded := json2.encode(test, escape_unicode: true)
	assert dump(encoded) == '{"field":"foo"}'
	assert json2.decode[Test]('{"field":	"foo"}')!.field == MySumType('foo')

	test = Test{
		field: 'foo'
	}
	encoded = json2.encode(test, escape_unicode: true)
	assert dump(encoded) == '{"field":"foo"}'
	assert json2.decode[Test]('{"field":"foo"}')! == test

	test = Test{
		field: 1
	}
	encoded = json2.encode(test, escape_unicode: true)
	assert dump(encoded) == '{"field":1}'
	assert json2.decode[Test]('{"field":1}')! == test

	mut test2 := MyString('foo')
	encoded = json2.encode(test2, escape_unicode: true)
	assert dump(encoded) == '"foo"'

	mut test3 := MyInt(1000)
	encoded = json2.encode(test3, escape_unicode: true)
	assert dump(encoded) == '1000'
}

fn test_encode_unicode_as_ascii_escape_sequences() {
	valid_json := r'{"emoji":"\u3007"}'
	decoded := json2.decode[UnicodeString](valid_json)!
	assert decoded.emoji == '〇'
	assert json2.encode(UnicodeString{
		emoji: '〇'
	},
		escape_unicode: true
	) == valid_json
	assert json2.encode('😀', escape_unicode: true) == r'"\uD83D\ude00"'
}
