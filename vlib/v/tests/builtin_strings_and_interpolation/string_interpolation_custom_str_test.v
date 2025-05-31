struct Foo {
	bar int
}

fn (f &Foo) str() string {
	return '${f.bar}'
}

struct Bar {
	foo &Foo
}

fn test_interpolation_with_custom_ref_str() {
	foo := Foo{}
	bar := Bar{&foo}
	println(bar)
	assert '${bar}'.contains('Bar{')
	assert '${bar}'.contains('foo: &0')
	assert '${bar}'.contains('}')
}
