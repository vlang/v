import math.big

fn main() {
	mut a := big.zero_int
	mut b := big.one_int
	mut c := a + b
	for _ in 2 .. 300 {
		a = b
		b = c
		c = a + b
	}
	// The 300th Fibonacci number
	println(c)
}
