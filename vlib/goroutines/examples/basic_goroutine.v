// Example: Basic goroutine usage with `go` keyword.
//
// This demonstrates how V's `go` keyword launches goroutines
// using the GMP scheduler (translated from Go's runtime).
//
// `go` launches a lightweight goroutine (like Go's goroutines)
// `spawn` launches an OS thread (like V's traditional threads)
module main

import time

fn say(msg string) {
	for i in 0 .. 5 {
		println('${msg}: ${i}')
		time.sleep(100 * time.millisecond)
	}
}

fn main() {
	// Launch goroutines with `go` - lightweight, Go-style concurrency
	go say('goroutine 1')
	go say('goroutine 2')

	// Main goroutine continues running
	say('main')
}
