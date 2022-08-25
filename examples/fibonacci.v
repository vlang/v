//
// Get Fibonacci numbers with V
// Author:Zach721
// You can this code for free to your
// own projects and purposes
//

// Get is a function which returns the Fibonacci
// secuence
fn get(number f64) {
	// All variables must be f64 to not get any calculation error
	mut first := f64(0)
	mut second := f64(1)
	println('Fibbonacci secuence:')
	for i := 0; i < number - 1; i++ {
		if first == 0 {
			println(first)
		}
		mut third := f64(first + second)
		println(first + second)
		first = f64(second)
		second = f64(third)
	}
}

fn main() {
	// Let's print the first 100 numbers of the Fibonacci secuence
	get(100)
}

