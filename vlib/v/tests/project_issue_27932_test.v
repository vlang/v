fn issue27932_worker(a [3]int) int {
	return a[0] + a[1] + a[2]
}

fn test_spawn_with_fixed_array_argument() {
	a := [1, 2, 3]!
	handle := spawn issue27932_worker(a)
	assert handle.wait() == 6
}
