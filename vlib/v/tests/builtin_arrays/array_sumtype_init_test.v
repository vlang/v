type SumFoo = int | string

struct Foo {
	bar []SumFoo
}

struct Bar {
	foo []Foo
}

fn test_main() {
	str := 'foobar'
	f := Bar{
		foo: [Foo{
			bar: [str]
		}]
	}
	assert f.foo.len == 1
	assert f.foo[0].bar.len == 1
	assert f.foo[0].bar[0] as string == str
}
