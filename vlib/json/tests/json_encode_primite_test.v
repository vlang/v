import json

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
	mut encoded := json.encode(test)
	assert dump(encoded) == '{"field":"foo"}'
	assert json.decode(Test, '{"field":	"foo"}')!.field == MySumType('foo')

	test = Test{
		field: 'foo'
	}
	encoded = json.encode(test)
	assert dump(encoded) == '{"field":"foo"}'
	assert json.decode(Test, '{"field":"foo"}')! == test

	test = Test{
		field: 1
	}
	encoded = json.encode(test)
	assert dump(encoded) == '{"field":1}'
	assert json.decode(Test, '{"field":1}')! == test

	mut test2 := MyString('foo')
	encoded = json.encode(test2)
	assert dump(encoded) == '"foo"'

	mut test3 := MyInt(1000)
	encoded = json.encode(test3)
	assert dump(encoded) == '1000'
}

fn test_encode_unicode_as_ascii_escape_sequences() {
	valid_json := r'{"emoji":"\u3007"}'
	decoded := json.decode(UnicodeString, valid_json)!
	assert decoded.emoji == '〇'
	assert json.encode(UnicodeString{
		emoji: '〇'
	}) == valid_json
	assert json.encode('😀') == r'"\uD83D\ude00"'
}
