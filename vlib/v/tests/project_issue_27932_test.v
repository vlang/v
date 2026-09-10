fn issue27932_worker(a [3]int) int {
	return a[0] + a[1] + a[2]
}

fn issue27932_pointer_worker(a &[3]int) int {
	unsafe {
		// Fixed-array pointer indexing requires an unsafe block.
		return a[0] + a[1] + a[2]
	}
}

fn issue27932_optional_worker(a ?[2]int) int {
	arr := a or { return 0 }
	return arr[0] + arr[1]
}

fn test_spawn_with_fixed_array_argument() {
	a := [1, 2, 3]!
	handle := spawn issue27932_worker(a)
	assert handle.wait() == 6
}

fn test_spawn_with_fixed_array_pointer_argument() {
	a := &[3]int{}
	handle := spawn issue27932_pointer_worker(a)
	assert handle.wait() == 0
}

fn test_spawn_with_optional_fixed_array_argument() {
	a := ?[2]int(none)
	handle := spawn issue27932_optional_worker(a)
	assert handle.wait() == 0
}
