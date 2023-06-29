fn multi_ret() (string, string) {
	return 'hello', 'hello'
}

fn main() {
	mut val := ''
	mut oval := ?string(none)

	oval, val = multi_ret() // order doesn't matter
	val, oval = multi_ret() // order doesn't matter
}