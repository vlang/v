struct FixedArrayRecords {
mut:
	values [2]int
}

// https://github.com/vlang/v/issues/28036
fn test_heap_struct_init_with_explicit_fixed_array_field() {
	records := &FixedArrayRecords{
		values: [2]int{}
	}
	assert records.values == [0, 0]!
}
