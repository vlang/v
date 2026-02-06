struct Foo {
	a      int
	b      string
	isbool bool
}

fn do[T](f T) T {
	return T{
		...f
		isbool: true
	}
}

fn test_main() {
	foo := do(Foo{
		a: 1
		b: '2'
	})
	assert foo.a == 1 && foo.b == '2' && foo.isbool
}
