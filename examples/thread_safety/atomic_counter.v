/*
This code demonstrates thread safety using atomic operations in V.

Thread safety is achieved by using atomic functions to manipulate the shared counter variable.
Atomic operations ensure that the read-modify-write sequence is performed as a single, indivisible operation,
preventing race conditions and ensuring data integrity when accessed by multiple threads concurrently.

Key points:
1. **Atomic Fetch and Add**: The `C.atomic_fetch_add_u32` function atomically increments the counter.
	This means that the increment operation is performed without interruption, ensuring that no two threads
	can increment the counter simultaneously and cause a race condition.

2. **Atomic Load**: The `C.atomic_load_u32` function atomically reads the value of the counter.
	This ensures that the read operation is consistent and not affected by concurrent writes from other threads.

3. **Thread Synchronization**: The `spawn` function is used to create new threads that run the `increment` function.
	The `wait` method is called on each thread to ensure that the main thread waits for both threads to complete
	before reading the final value of the counter.

By using atomic operations and proper thread synchronization, the code ensures that the shared counter is
incremented safely and correctly by multiple threads.
*/
import sync as _

// Function to increment the atomic counter
fn increment(atomic_counter &u32) {
	C.atomic_fetch_add_u32(atomic_counter, 1)
}

fn main() {
	atomic_counter := u32(0) // Atomic counter variable

	// Spawn two threads that increment the atomic counter
	t1 := spawn increment(&atomic_counter)
	t2 := spawn increment(&atomic_counter)

	// Wait for both threads to complete
	t1.wait()
	t2.wait()

	// Load and print the final value of the atomic counter
	final_count := C.atomic_load_u32(&atomic_counter)
	println('Atomic Counter: ${final_count}')
}
