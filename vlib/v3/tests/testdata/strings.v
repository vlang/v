// exclaim supports exclaim handling for v3 tests.
fn exclaim(s string) string {
	return s + '!'
}

// main runs the v3 tests entry point.
fn main() {
	a := 'hello'
	b := 'world'
	println(a + ' ' + b)
	println(exclaim('v3 works'))
}
