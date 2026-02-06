/*
Test for backtrace capability
*/
fn a_method() {
	print_backtrace()
}

fn test_backtrace() {
	a_method()
	// panic('hi')
}
