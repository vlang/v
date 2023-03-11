struct Foo {
	x ?string
}

fn test_main() {
	x := ?string('hi')
	foo := Foo{
		x: x
	}
	assert foo.x? == 'hi'
}

fn test_none() {
	x := ?string(none)
	foo := Foo{
		x: x
	}
	assert foo.x == none
}
