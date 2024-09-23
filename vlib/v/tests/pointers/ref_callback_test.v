module main

struct Struct_voidptr {
	func voidptr
}

fn function() string {
	return 'Function!'
}

fn test_main() {
	fun := function

	sct := Struct_voidptr{
		func: &fun
	}

	assert fun() == 'Function!'
	assert sct.func == &fun
}
