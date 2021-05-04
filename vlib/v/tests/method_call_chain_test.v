struct Test {
mut:
	val int
}

// this must return a reference, or else you'll get a C error
// TODO: add a proper checker check for that case
fn new(x int) &Test {
	return &Test{x}
}

fn (mut t Test) inc() &Test {
	t.val++
	return t
}

fn (mut t Test) add(x int) &Test {
	t.val += x
	return t
}

fn (mut t Test) div(x int) &Test {
	t.val /= x
	return t
}

fn test_method_call_chains() {
	mut x := new(4).inc().inc().inc().inc().add(4).div(2).inc()
	assert x.val == 7
}
