fn example[T]() ?T {
	return T{}
}

struct Foo {
	a int = 10
	b string
}

fn test_main() {
	dump(example[[1]Foo]())

	a := example[[1]Foo]()
	assert a? == [Foo{}]!

	dump(example2[[1]Foo]()!)

	b := example2[[1]Foo]()!
	assert b == [Foo{}]!
}

fn example2[T]() !T {
	return T{}
}
