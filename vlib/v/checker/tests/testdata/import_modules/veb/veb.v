module veb

// Context is the embedded type that the route checker recognizes.
pub struct Context {}

// Result is the handler return type that the route checker recognizes.
pub struct Result {}

// no_result supplies the result used by the handler fixtures.
pub fn no_result() Result {
	return Result{}
}

// text supplies the context method used by the free-function fixture.
pub fn (mut ctx Context) text(_ string) Result {
	return Result{}
}

// run supplies the generic server entry point used by the free-function fixture.
pub fn run[A, C](mut _ A, _ int) {}
