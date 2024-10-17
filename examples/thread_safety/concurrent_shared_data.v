/*
This example demonstrates thread safety using V's concurrency features.
Key points:
- The `SharedData` struct contains a mutable counter that will be accessed by multiple threads.
- The `increment` function increments the counter within a lock to ensure that only one thread
can modify the counter at a time, preventing race conditions.
- In the `main` function, two threads are spawned to increment the shared counter concurrently.
- The `lock` keyword is used to ensure exclusive access to the shared data during modification,
and the `rlock` keyword is used to allow multiple threads to read the data concurrently without
modification.
This ensures that the counter is incremented safely and the final value is printed correctly.
*/

struct SharedData {
mut:
	counter int
}

fn increment(shared data SharedData) {
	lock data {
		data.counter++
	}
}

fn main() {
	shared data := SharedData{}
	threads := [spawn increment(shared data), spawn increment(shared data)]

	for t in threads {
		t.wait() // Wait for both threads to complete
	}

	rlock data {
		println('Counter: ${data.counter}')
	}
}
