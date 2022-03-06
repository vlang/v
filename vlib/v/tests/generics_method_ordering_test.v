struct Foo<T> {
	x int
}

fn (f Foo<T>) pop() {
	println('hey')
}

struct Bar<T> {
	y int
}

// Note: Bar.foo before Bar.pop, should not cause a V compiler panic
fn (b Bar<T>) foo() bool {
	return true
}

fn (b Bar<T>) pop() {
	println(b.foo())
}

// fn dummy() { println(Bar<int>{}) }

fn test_foo() {
	dump(Foo<int>{})
	assert true
}
