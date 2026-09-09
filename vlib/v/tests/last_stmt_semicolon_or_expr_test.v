fn foo() !int {
	return 0
}

fn test_main() {
	mut x := 1
	// vfmt off
	x = foo() or { panic("failed"); }
	// vfmt on
	println('x=${x}')
}

fn return_zero_with_trailing_semicolon() int {
	// vfmt off
	return 0;
	// vfmt on
}

fn test_return_with_trailing_semicolon() {
	assert return_zero_with_trailing_semicolon() == 0
}
