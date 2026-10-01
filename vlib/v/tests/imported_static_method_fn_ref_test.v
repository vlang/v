module main

import staticfnref { MyStruct }

fn call_main(fun fn (value int) staticfnref.MyStruct, value int) staticfnref.MyStruct {
	return fun(value)
}

fn test_imported_static_method_reference() {
	s := staticfnref.MyStruct.new(3)
	assert s.value == 3

	s2 := call_main(staticfnref.MyStruct.new, 4)
	assert s2.value == 4
}

fn imported_static_method_value[T]() fn (int) staticfnref.MyStruct {
	f := staticfnref.MyStruct.new
	g := MyStruct.new
	assert voidptr(f) == voidptr(g)
	return g
}

fn test_imported_static_method_value_in_generic_and_voidptr() {
	f := imported_static_method_value[int]()
	assert f(5).value == 5
	assert voidptr(staticfnref.MyStruct.new) == voidptr(MyStruct.new)
}
