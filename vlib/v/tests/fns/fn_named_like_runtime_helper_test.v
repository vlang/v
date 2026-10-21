// The functions of a program can have the names that the C helpers of the
// compiler for `in`, `.index()`, `==` on arrays and `os.filelock` used to have
// in the generated C code. Their calls run them, and those operations still use
// the helpers.
fn array_contains_int(a []int, x int) bool {
	return false
}

fn array_index_string(a []string, s string) int {
	return -2
}

fn array_last_index_u8(a []u8, x u8) int {
	return -3
}

fn array_eq_raw(a []u8, b []u8) bool {
	return false
}

fn array_eq_string(a []string, b []string) bool {
	return false
}

fn fixed_array_contains_u8(a []u8, x u8) bool {
	return false
}

fn v_filelock_lock() int {
	return 3
}

fn v_filelock_unlock() int {
	return 4
}

fn test_functions_named_like_runtime_helpers() {
	numbers := [1, 2, 3]
	assert 2 in numbers
	assert !array_contains_int(numbers, 2)
	names := ['a', 'b']
	assert names.index('b') == 1
	assert array_index_string(names, 'b') == -2
	bytes := [u8(1), 2, 1]
	assert bytes.last_index(1) == 2
	assert array_last_index_u8(bytes, 1) == -3
	assert bytes == [u8(1), 2, 1]
	assert !array_eq_raw(bytes, bytes)
	assert names == ['a', 'b']
	assert !array_eq_string(names, names)
	fixed := [u8(1), 2]!
	assert u8(2) in fixed
	assert !fixed_array_contains_u8(bytes, 2)
	assert v_filelock_lock() + v_filelock_unlock() == 7
}
