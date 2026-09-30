// vtest vflags: -w
import json2

pub struct SomeStruct {
pub mut:
	test ?string
}

pub struct MyStruct {
pub mut:
	result ?SomeStruct
	id     string
}

fn test_main() {
	a := MyStruct{
		id:     'some id'
		result: SomeStruct{}
	}
	encoded_string := json2.encode(a, escape_unicode: true)
	assert encoded_string == '{"result":{},"id":"some id"}'
	test := json2.decode[MyStruct](encoded_string)!
	assert test == a
}
