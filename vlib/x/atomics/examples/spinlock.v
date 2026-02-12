module main

import x.atomics

struct SpinLock {
mut:
	state u32 // 0 = unlocked, 1 = locked
}

fn acquire(mut spinlock SpinLock) {
	for !atomics.cas_u32(&spinlock.state, 0, 1) {
		// Busy-wait
	}
}

fn release(mut spinlock SpinLock) {
	atomics.store_u32(&spinlock.state, 0)
}

struct SharedData {
mut:
	spinlock SpinLock
	value    int
}

fn worker(mut data SharedData, iterations int) {
	for _ in 0 .. iterations {
		acquire(mut data.spinlock)
		data.value++
		release(mut data.spinlock)
	}
}

fn main() {
	mut data := &SharedData{}

	num_threads := 4
	iterations_per_thread := 10000

	mut threads := []thread{}

	for _ in 0 .. num_threads {
		threads << spawn worker(mut data, iterations_per_thread)
	}

	threads.wait()

	expected := num_threads * iterations_per_thread
	actual := data.value

	println('Expected: ${expected}')
	println('Actual:   ${actual}')

	if actual == expected {
		println('Spinlock works correctly')
	} else {
		println('Race condition detected')
	}
}
