struct ClassInfo {
	func fn () int = unsafe { nil }
}

pub fn func2[T]() int {
	return sizeof(T)
}

pub fn func1[T]() int {
	ci := ClassInfo{
		func: func2[T]
	}
	return ci.func()
}

struct Struct1 {}

struct Struct2 {}

fn test_struct_field_init_with_generic_fn() {
	r1 := func1[i32]()
	println(r1)
	assert r1 == 4

	r2 := func1[bool]()
	println(r2)
	assert r2 == 1

	r3 := func1[Struct1]()
	println(r3)
	assert r3 == 1

	r4 := func1[Struct2]()
	println(r4)
	assert r4 == 1
}
