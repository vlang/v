module main

import another_module as aaa

fn test_import_opt_struct_from_another_module() {
	x := ?aaa.SomeStruct{}
	assert dump(x == none) == true
}

fn test(a ?aaa.SomeStruct) ?aaa.SomeStruct {
	return none
}

fn test_fn_decl_with_struct_from_another_module() {
	x := test(?aaa.SomeStruct(none))
	assert dump(x == none) == true
}
