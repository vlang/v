import json

struct Test {
	field MySumType
}

type MyInt = int
type MyString = string
type MySumType = MyString | int | string

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
