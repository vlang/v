module locked

@[noinit]
pub struct Foo {
pub:
	digit int
}

@[noinit]
pub struct Generic[T] {
pub:
	digit T
}

// new_foo constructs Foo inside its declaring module.
pub fn new_foo() Foo {
	return Foo{ digit: 5 }
}
