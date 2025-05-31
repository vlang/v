module amod

pub struct Xyz {
pub:
	x int
}

pub struct Bcg {
	x int
}

@[params]
pub struct FooParams {
	bar string
}

pub fn foo(opts FooParams) {}
