[unsafe]
fn foo() int {
	mut static x := 42
	x++
	return x
}

fn xfoo() int {
	return unsafe { foo() }
}

fn test_static_vars_work() {
	assert xfoo() == 43
	assert xfoo() == 44
	assert xfoo() == 45
}
