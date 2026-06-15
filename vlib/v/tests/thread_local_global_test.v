@[has_globals]
module main

// Verifies the `@[thread_local]` attribute on a `__global` emits real
// thread-local storage (`_Thread_local`), so each thread sees its own copy.
// A channel barrier makes all threads write before any read: if the global were
// process-global (the attribute silently ignored), every thread would read the
// last writer's value and the per-thread assertion would fail deterministically.

@[thread_local]
__global tl_value = 0

@[thread_local]
__global tl_cast_value = u32(0)

fn tl_worker(id int, written chan int, release chan int) int {
	tl_value = id
	tl_cast_value = u32(id * 10)
	written <- id // signal: my write happened
	_ := <-release // wait until every thread has written
	return tl_value + int(tl_cast_value) // TLS -> my own values; shared globals -> last writer's values
}

fn test_thread_local_global_is_per_thread() {
	n := 8
	written := chan int{cap: n}
	release := chan int{cap: n}
	mut threads := []thread int{}
	for i in 0 .. n {
		threads << spawn tl_worker(i + 1, written, release)
	}
	// barrier: every thread has written its value before any thread reads
	for _ in 0 .. n {
		_ := <-written
	}
	for _ in 0 .. n {
		release <- 1
	}
	mut sum := 0
	for i, t in threads {
		got := t.wait()
		expected := (i + 1) * 11
		assert got == expected, 'thread ${i + 1} read ${got}: @[thread_local] global was clobbered (not real TLS)'
		sum += got / 11
	}
	assert sum == n * (n + 1) / 2
}
