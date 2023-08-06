struct Foo {
	f fn (Foo) int = dummy
}

fn dummy(s Foo) int {
	return 22
}

fn (mut s Foo) fun() int {
	return s.f(s)
}

fn test_struct_field_fn_call() {
	mut s := Foo{}
	ret := s.fun()
	println(ret)
	assert ret == 22
}
