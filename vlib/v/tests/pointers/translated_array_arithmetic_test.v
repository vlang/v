@[translated]
module main

fn read_translated_element(value &int) int {
	return unsafe { *value }
}

fn test_translated_fixed_array_pointer_arithmetic() {
	values := [3, 5, 7]!
	start := values + 0
	next := values + 1
	assert unsafe { *next } == 5
	assert next - start == 1
	assert next - values == 1
	assert values - next == -1
	assert (1 + values) - values == 1
	assert read_translated_element(values + 2) == 7
	assert read_translated_element(next - 1) == 3
}
