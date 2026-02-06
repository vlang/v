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
