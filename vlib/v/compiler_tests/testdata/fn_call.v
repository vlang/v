// add updates add state for v3 tests.
fn add(a int, b int) int {
	return a + b
}

// greet supports greet handling for v3 tests.
fn greet(name string) {
	println('hello, ' + name)
}

// main runs the v3 tests entry point.
fn main() {
	x := add(3, 4)
	println(x)
	greet('world')
}
