interface Foo {}

fn has_interface_args(mut a Foo, b &Foo, c Foo) {
	assert a == &Foo(1)
	assert b == &Foo(1)
	assert c == Foo(1)
}

fn test_fn_call_interface_args() {
	mut arg := Foo(1)
	has_interface_args(mut arg, arg, arg)
}
