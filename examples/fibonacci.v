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

	// Can only calculate correctly until rank 92
	if stop > 92 {
		println('rank must be 92 or less')
		return
	}

	// Three consecutive terms of the sequence
	mut a := u64(0)
	mut b := u64(0)
	mut c := u64(1)

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
