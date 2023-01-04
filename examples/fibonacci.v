// This program displays the fibonacci sequence
import os

fn main() {
	// Check for user input
	if os.args.len != 2 {
		println('usage: fibonacci [rank]')

		return
	}

	// Parse first argument and cast it to int

	stop := os.args[1].int()
	// Can only calculate correctly until rank 92
	if stop > 92 {
		println('rank must be 92 or less')
		return
	}

	// Three consecutive terms of the sequence
	mut a := i64(0)
	mut b := i64(0)
	mut c := i64(1)
	println(a + b + c)
	for _ in 0 .. stop {
		// Set a and b to the next term
		a = b
		b = c
		// Compute the new term
		c = a + b

		// Print the new term
		println(c)
	}
}
