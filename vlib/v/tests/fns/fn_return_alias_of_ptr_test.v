type HANDLE = voidptr

struct ExampleStruct {
	handle HANDLE
}

fn get_ptr(arg ExampleStruct) voidptr {
	return arg.handle
}

fn test_fn_return_alias_of_ptr() {
	h := ExampleStruct{}
	r := get_ptr(h)
	println(r)
	assert r == unsafe { nil }
}
