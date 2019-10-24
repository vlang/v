// This program displays the fibonacci sequence

import os

fn main() {
	// Check for user input
	if os.args.len != 2 {
		println('usage: fibonacci [rank]')

		// Exit
		return
	}
	
	// Parse first argument and cast it to int
	stop := os.args[1].int()

	// Three consecutive terms of the sequence
	mut a := i64(0)
	mut b := i64(0)
	mut c := i64(1)

	for i := 0; i < stop; i++ {
		// Set a and b to the next term
		a = b
		b = c
		// Compute the new term
		c = a + b

		// Print the new term
		println(c)
	}
}
