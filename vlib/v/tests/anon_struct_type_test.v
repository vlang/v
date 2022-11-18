module main

fn func(arg struct { foo string }) {
	assert arg.foo == 'foo'
}

fn test_anon_struct_as_parameter() {
	func(struct {
		foo: 'foo'
	})
}
