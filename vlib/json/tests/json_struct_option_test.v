import json

pub struct MyStruct[T] {
pub mut:
	result ?T
	id     string
}

fn test_gn_struct_string() ! {
	a := MyStruct[string]{
		result: 'test'
		id:     'some id'
	}

	encoded_string := json.encode(a)
	dump(encoded_string)
	test := json.decode(MyStruct[string], encoded_string)!
	dump(test)
	assert a == test
}

fn test_gn_struct_int() ! {
	a := MyStruct[int]{
		result: 1
		id:     'some id'
	}

	encoded_string := json.encode(a)
	dump(encoded_string)
	test := json.decode(MyStruct[int], encoded_string)!
	dump(test)
	assert a == test
}

fn test_gn_struct_f64() ! {
	a := MyStruct[f64]{
		result: 1.2
		id:     'some id'
	}

	encoded_string := json.encode(a)
	dump(encoded_string)
	test := json.decode(MyStruct[f64], encoded_string)!
	dump(test)
	assert a == test
}
