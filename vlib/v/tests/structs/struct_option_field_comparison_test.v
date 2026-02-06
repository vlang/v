pub struct SomeStruct {
pub mut:
	test ?string
}

pub struct MyStruct {
pub mut:
	id     string
	result ?SomeStruct
}

fn test_struct_with_option_fields_inequality() {
	a := MyStruct{
		id:     'some id'
		result: none
	}
	b := MyStruct{
		id:     'some id'
		result: SomeStruct{}
	}
	dump(a)
	dump(b)
	dump(a == b)
	dump(a != b)
	assert a != b
}
