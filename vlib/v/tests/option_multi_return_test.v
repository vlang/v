fn foo(val ?int) (?int, ?int) {
	return val, none
}

fn test_multi_return() {
	a, b := foo(100)
	assert a == 100
	assert b == none
}
