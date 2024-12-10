import json

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
	encoded_string := json.encode(a)
	assert encoded_string == '{"result":{},"id":"some id"}'
	test := json.decode(MyStruct, encoded_string)!
	assert test == a
}
