module main

// map_fuzz_test covers the only helper in `fuzz/map_fuzz.v` that can be driven
// without running the fuzzers themselves: fuzz1..fuzz6 allocate and free maps
// of up to half a million entries, so they belong to `main`, not to a test.

fn test_generate_strings_returns_the_requested_count_and_length() {
	arr := generate_strings(20, 3)
	assert arr.len == 3
	assert arr.map(it.len) == [20, 20, 20]
}

fn test_generate_strings_with_zero_length_returns_empty_strings() {
	arr := generate_strings(0, 2)
	assert arr.len == 2
	assert arr == ['', '']
}

fn test_generate_strings_with_zero_count_returns_an_empty_array() {
	assert generate_strings(20, 0) == []string{}
}

fn test_generate_strings_allocates_the_array_up_front() {
	// The helper builds the array with `[]string{len: n}` and fills it, so the
	// result length must match the request without any further growth.
	for n in [1, 2, 5, 17] {
		assert generate_strings(4, n).len == n
	}
}
