module main

fn issue27932_parallel_first_worker(a ?[2]int) int {
	arr := a or { return 0 }
	return arr[0] + arr[1]
}

fn issue27932_parallel_spawn_first(a ?[2]int) int {
	handle := spawn issue27932_parallel_first_worker(a)
	return handle.wait()
}
