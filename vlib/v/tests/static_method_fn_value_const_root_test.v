module main

import staticfnref as staticref

struct MyStruct {
	new int
}

struct StaticWrapper {
	MyStruct
}

const staticfnref = StaticWrapper{ MyStruct: MyStruct{ new: 17 } }

fn const_new[T]() int {
	return staticfnref.MyStruct.new
}

fn test_static_method_value_preserves_const_root() {
	assert staticref.MyStruct.new(3).value == 3
	assert staticfnref.MyStruct.new == 17
	assert const_new[int]() == 17
}
