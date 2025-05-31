pub struct Foo {
	a int
	b int
}

pub fn (f Foo) hello() ! {
	println('Hello')
}

fn default_foo() IFoo {
	return Foo{}
}

fn default_bar() IFoo {
	return Foo{}
}

pub struct Bar {
	foo ?IFoo
	bar ?IFoo
}

pub interface IFoo {
	hello() !
}

pub fn (b Bar) get_foo(baz string) !IFoo {
	if baz == 'foo' {
		return b.foo or { default_foo() }
	} else if baz == 'bar' {
		return b.bar or { default_bar() }
	}
	return error('unknown baz ${baz}')
}

fn test_main() {
	a := Bar{}.get_foo('foo')!
	a.hello()!
	assert a is IFoo
}
