struct Foo {
mut:
	state ?string
}

fn (mut c Foo) foo() int {
	if c.state as string != '' {
		return 1
	}
	return 0
}

fn test_main() {
	mut f1 := Foo{}
	mut f2 := Foo{''}
	mut f3 := Foo{'foo'}
	assert f1.foo() == 0
	assert f2.foo() == 0
	assert f3.foo() == 1
}
