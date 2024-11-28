import another_module

struct SomeStruct {}

fn type_from_another_mod() {
	_ = &[]another_module.SomeStruct{}
}

fn type_from_current_mod() {
	_ = &[]SomeStruct{}
}

fn test_parse_type_of_ref_array() {
	type_from_another_mod()
	type_from_current_mod()
	assert true
}
