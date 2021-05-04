import simplemodule

// this tests whether the tests can import the same module without any special
// custom paths setup on the CLI
fn test_iadd() {
	assert simplemodule.iadd(10, 20) == 30
}

fn test_imul() {
	assert simplemodule.imul(5, 8) == 40
}
