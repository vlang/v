struct Foo {
	f ?fn (int) int
}

fn t1(a int) int {
	println(a)
	return a
}

fn test_if_guard_with_struct_option_fntype_field() {
	foo := Foo{t1}

	if ff := foo.f {
		ret := ff(22)
		assert ret == 22
	} else {
		assert false
	}
}
