struct Foo {
}

fn test_assign_static_method_to_anon_fn() {
	anon_fn := Foo.bar
	ret := anon_fn()
	assert ret == 'bar'
}

fn Foo.bar() string {
	println('bar')
	return 'bar'
}
