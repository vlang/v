struct Test {
mut:
	val int
}

// this must return a reference, or else the V compiler will complain

fn new(x int) &Test {
	return &Test{x}
}

fn (mut t Test) inc() &Test {
	t.val++
	return unsafe { &t }
}

fn (mut t Test) add(x int) &Test {
	t.val += x
	return unsafe { &t }
}

fn (mut t Test) div(x int) &Test {
	t.val /= x
	return unsafe { &t }
}

fn test_method_call_chains() {
	mut x := new(4).inc().inc().inc().inc().add(4).div(2).inc()
	assert x.val == 7
}
