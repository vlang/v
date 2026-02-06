struct Foo {
}

fn Foo.bar() string {
	println('bar')
	return 'bar'
}

fn test_assign_static_method_to_anon_fn() {
	anon_fn := Foo.bar
	ret := anon_fn()
	assert ret == 'bar'
}
