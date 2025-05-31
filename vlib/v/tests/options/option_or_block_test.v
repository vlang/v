fn foo(val ?int) {
	if val == none {
		a := val or { return }

		assert false
		println(a)
	}
	assert false
}

fn test_main() {
	foo(none)
	assert true
}
