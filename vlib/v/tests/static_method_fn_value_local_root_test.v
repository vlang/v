module main

import staticfnref as staticref

struct MyStruct {
	new int
}

struct StaticWrapper {
	MyStruct
}

fn local_new[T](staticfnref T) int {
	return staticfnref.MyStruct.new
}

fn imported_new[T]() fn (int) staticref.MyStruct {
	return staticref.MyStruct.new
}

fn test_static_method_value_preserves_local_root() {
	assert staticref.MyStruct.new(3).value == 3
	new_struct := imported_new[int]()
	assert new_struct(4).value == 4
	staticfnref := StaticWrapper{ MyStruct: MyStruct{ new: 7 } }
	assert staticfnref.MyStruct.new == 7
	assert local_new[StaticWrapper](staticfnref) == 7
}
