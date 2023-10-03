struct Foo {
}

fn Foo.bar() string {
	println('bar')
	return 'bar'
}

fn test_assign_static_method_to_anon_fn() {
	// vfmt off
	anon_fn := Foo.bar
	// vfmt on
	ret := anon_fn()
	assert ret == 'bar'
}
