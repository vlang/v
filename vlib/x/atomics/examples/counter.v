module main

import atomics

fn increment(counter &i64) {
	atomics.add_i64(counter, 1)
}

fn worker(counter &i64, iterations int) {
	for _ in 0 .. iterations {
		increment(counter)
	}
}

fn main() {
	mut counter := i64(0)

	num_threads := 4
	increments_per_thread := 10000

	mut threads := []thread{}

	for _ in 0 .. num_threads {
		threads << spawn worker(&counter, increments_per_thread)
	}

	threads.wait()

	expected := i64(num_threads * increments_per_thread)
	actual := atomics.load_i64(&counter)

	println('Expected: ${expected}')
	println('Actual:   ${actual}')

	if actual == expected {
		println('Counter is correct')
	} else {
		println('Counter mismatch - race condition detected')
	}
}
