import lookup

const local_table = make_local_table()

fn make_local_table() [3]string { return ['one', 'two', 'three']! }

fn test_fixed_array_function_constants() {
	assert lookup.get(0) == 11
	assert lookup.get(3) == 44
	assert lookup.table[2] == 33
	assert lookup.get_alias(0) == 55
	assert lookup.aliased[1] == 66
	assert local_table[1] == 'two'
}
