@[has_globals]
module main

import staticfnref as staticref

struct MyStruct {
	new int
}

struct StaticWrapper {
	MyStruct
}

__global staticfnref = StaticWrapper{ MyStruct: MyStruct{ new: 27 } }

fn global_new[T]() int {
	return staticfnref.MyStruct.new
}

fn test_static_method_value_preserves_global_root() {
	assert staticref.MyStruct.new(3).value == 3
	assert staticfnref.MyStruct.new == 27
	assert global_new[int]() == 27
}
