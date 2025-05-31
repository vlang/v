struct Undefined {}

type Foo[T] = ?T | Undefined

@[params]
struct Bar {
	x Foo[bool] = Undefined{}
}

fn f(b Bar) Bar {
	return dump(b)
}

fn test_main() {
	a := f()
	assert a == Bar{}
}
