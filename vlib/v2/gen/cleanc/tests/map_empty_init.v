module main

fn make_empty() map[string]int {
	return {}
}

fn main() {
	// Empty shorthand map literal `{}` should lower to a correct runtime map constructor.
	mut m := make_empty()
	key1 := 'abc'
	key2 := key1.clone() // Ensure key equality is based on string content, not pointer identity.
	m[key1] = 123
	println(m[key2])
	println(m['missing'])

	// Typed empty map init should also lower to a runtime map constructor.
	mut m2 := map[string]int{}
	m2['x'] = 7
	println(m2['x'])
}
