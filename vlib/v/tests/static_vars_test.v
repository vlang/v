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

@[unsafe]
fn next_fib() int {
	mut static fibs := []int{}
	if fibs.len == 0 {
		fibs << 1
		fibs << 1
	} else {
		fibs << fibs[fibs.len - 1] + fibs[fibs.len - 2]
	}
	return fibs[fibs.len - 1]
}

fn test_static_vars_with_array_initializers() {
	assert unsafe { next_fib() } == 1
	assert unsafe { next_fib() } == 2
	assert unsafe { next_fib() } == 3
	assert unsafe { next_fib() } == 5
}
