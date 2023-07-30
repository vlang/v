[heap]
struct BloomFilter[T] {
	hash_func     fn (T) u32 = unsafe { nil } // hash function, input [T] , output u32
	table_size    int        // every entry is one-bit, packed into `table`
	num_functions int        // 1~16
}

fn test_generic_struct_field_with_default_fn_type() {
	filter := BloomFilter[int]{}
	println(filter)
	assert true
}
