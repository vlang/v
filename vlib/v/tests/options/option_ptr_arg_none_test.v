fn takes_optional_pointer(maybe_ptr ?&int) int {
	if ptr := maybe_ptr {
		return *ptr
	} else {
		return 0
	}
}

fn test_main() {
	val := takes_optional_pointer(none)
	assert val == 0
}
