module main

import staticfnref

fn call_main(fun fn (value int) staticfnref.MyStruct, value int) staticfnref.MyStruct {
	return fun(value)
}

fn test_imported_static_method_reference() {
	s := staticfnref.MyStruct.new(3)
	assert s.value == 3

	s2 := call_main(staticfnref.MyStruct.new, 4)
	assert s2.value == 4
}
