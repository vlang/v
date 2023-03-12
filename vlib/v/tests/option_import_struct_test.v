module main

import another_module as aaa

fn test_import_opt_struct_from_another_module() {
	x := ?aaa.SomeStruct{}
	assert dump(x == none) == true
}
