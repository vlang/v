module main

fn mk() map[string]int {
	return {
		'a': 10
	}
}

fn mk2() map[string]string {
	return {
		'x': 'y'
	}
}

fn main() {
	// Map index reads should lower in the transformer even when the map expression is not addressable.
	println(mk()['a'])
	println(mk()['missing'])

	// Missing string key should return an empty string (not nil).
	s := mk2()['missing']
	println('>${s}<')
}
