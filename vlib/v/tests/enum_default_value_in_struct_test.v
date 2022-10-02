enum MyEnum {
	first = 20
	second
	third
}

struct MyStruct {
mut:
	e MyEnum = .second
}

fn test_enum_first_value() {
	assert MyEnum.first == unsafe { MyEnum(20) }
}

fn test_enum_default_value() {
	d := MyStruct{}
	assert int(d.e) == 21
	assert 'd.e: $d.e | int(d.e): ${int(d.e).str()}' == 'd.e: second | int(d.e): 21'
}

fn test_enum_non_default_value() {
	t := MyStruct{
		e: .third
	}
	assert int(t.e) == 22
	assert 't.e: $t.e | int(t.e): ${int(t.e).str()}' == 't.e: third | int(t.e): 22'
}

fn test_generation_of_string_interpolation_method_for_pointer_to_struct_containing_enum_fields() {
	t := &MyStruct{
		e: .third
	}
	assert 't: $t' == 't: &MyStruct{\n    e: third\n}'
}
