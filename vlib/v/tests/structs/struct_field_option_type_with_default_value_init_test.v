type SomeType = SomeStruct | string

struct SomeStruct {}

struct AnotherStruct {
	field ?SomeType = 'default_string'
}

fn test_struct_field_option_type_with_default_value() {
	s := AnotherStruct{}
	println(s.field)
	assert '${s.field}' == "Option(SomeType('default_string'))"
}
