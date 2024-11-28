struct ClassInfo {
	func fn () = unsafe { nil }
}

pub fn func2[T]() {
}

pub fn func1[T]() {
	ci := ClassInfo{
		func: func2[T]
	}
	ci.func()
}

struct Struct1 {}

struct Struct2 {}

fn test_struct_field_init_with_generic_fn() {
	func1[i32]()
	func1[bool]()
	func1[Struct1]()
	func1[Struct2]()
	assert true
}
