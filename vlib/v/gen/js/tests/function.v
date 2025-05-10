const hello = 'hello world!'

fn main() {
	// calls a function directly
	cb1 := fn () string {
		return hello
	}
	assert cb1() == hello

	// calls a function indirectly
	cb2 := cb1
	assert cb2() == hello

	// calls a pointer function
	cb3 := &cb1
	assert cb3() == hello

	// calls a pointer function of a pointer function
	cb4 := &cb3
	assert cb4() == hello
}
