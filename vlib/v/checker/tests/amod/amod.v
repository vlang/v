module amod

pub struct Xyz {
pub:
	x int
}

pub struct Bcg {
	x int
}

pub struct Bcg2[T] {
	x int
}

@[params]
pub struct FooParams {
	bar string
}

pub fn foo(opts FooParams) {}

struct PrivateFoo {
pub:
	bar int
}

pub fn new_private_foo(bar int) PrivateFoo {
	return PrivateFoo{
		bar: bar
	}
}
