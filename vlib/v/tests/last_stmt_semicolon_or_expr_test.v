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
