const constant = 5
const other = init_const(constant)!

fn test_main() {
	assert other == 5
}

fn init_const(c int) !int {
	return c
}
