module main

import atomics

fn main() {
	// Basic atomic load and store operations
	mut value := i32(0)

	// Atomically store a value
	atomics.store_i32(&value, 42)

	// Atomically load the value
	loaded := atomics.load_i32(&value)
	println('Loaded value: ${loaded}') // Output: 42

	// Atomic add: returns the new value after addition
	new_value := atomics.add_i32(&value, 10)
	println('After add: ${new_value}') // Output: 52

	// Atomic swap: returns the old value
	old := atomics.swap_i32(&value, 100)
	println('Old value: ${old}, new value: ${atomics.load_i32(&value)}') // Output: 52, 100
}
