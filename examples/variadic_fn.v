module main

fn example_variadic_int(a string, b ...int) {
	println(' # variadic args ( int ):')
	for i:= 0; i<b.len; i++ {
		val := b[i]
		println(' * arg $i: $val')
	}
}

fn example_variadic_string(a string, b ...string) {
	println(' # variadic args ( string ):')
	for i:= 0; i<b.len; i++ {
		val := b[i]
		println(' * arg $i: $val')
	}
}
fn main() {
	a := 'hello'
	// b := 'world'
	example_variadic_int('a', 1, 2)
	example_variadic_string('a', a, 'v2')
}
