@[unsafe]
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

fn test_static_vars_in_unsafe_blocks() {
	f := fn () int {
		unsafe {
			mut static counter := 0
			counter++
			return counter
		}
	}
	assert f() == 1
	assert f() == 2
	dump(f()) // 3
}
