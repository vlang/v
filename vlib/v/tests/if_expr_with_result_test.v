fn foo(i int) ?bool {
	return if i == 0 { true } else { none }
}

fn bar(i int) !bool {
	return if i == 0 { true } else { error('') }
}

fn test_if_expr_with_result() {
	r1 := foo(0) or { false }
	println(r1)
	assert r1

	r2 := bar(0) or { false }
	println(r2)
	assert r2
}
