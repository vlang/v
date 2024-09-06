interface Foo[T] {
	val T
	foo() T
}

struct Bar[T] implements Foo[T] {
	val T
}

fn (b Bar[T]) foo() T {
	return b.val
}

fn test_main() {
	b := Bar{
		val: 0
	}
	assert b.foo() == 0
}
