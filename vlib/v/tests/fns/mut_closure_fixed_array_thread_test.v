module main

const fixed_array_size = 4

fn forward_threads(threads [fixed_array_size]thread) thread {
	return threads[0]
}

fn worker_thread() {}

fn test_mut_closure_capture_can_update_fixed_array_elements() {
	mut values := [fixed_array_size]int{}
	update := fn [mut values] () {
		values[0] = 42
		values[3] = 7
	}
	update()
	assert values[0] == 42
	assert values[3] == 7
}

fn test_mut_closure_capture_of_fixed_array_sees_late_updates() {
	mut values := [fixed_array_size]int{}
	read := fn [mut values] () int {
		return values[0]
	}
	values[0] = 99
	assert read() == 99
}

fn test_mut_closure_capture_of_fixed_array_of_threads_sees_late_updates() {
	mut threads := [fixed_array_size]thread{}
	read := fn [mut threads] () thread {
		return forward_threads(threads)
	}
	threads[0] = spawn worker_thread()
	assert read() == threads[0]
	threads[0].wait()
}
