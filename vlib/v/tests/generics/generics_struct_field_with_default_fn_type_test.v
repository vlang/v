@[heap]
struct BloomFilter1[T] {
	hash_func     fn (T) u32 = unsafe { nil } // hash function, input [T] , output u32
	table_size    int // every entry is one-bit, packed into `table`
	num_functions int // 1~16
}

@[heap]
struct BloomFilter2[T] {
	hash_func     fn (T) u32 = default_cb[T] // hash function, input [T] , output u32
	table_size    int // every entry is one-bit, packed into `table`
	num_functions int // 1~16
mut:
	table []u8
}

fn default_cb[T](x T) u32 {
	return 22
}

fn test_generic_struct_field_with_default_fn_type() {
	filter1 := BloomFilter1[int]{}
	println(filter1)
	assert true

	filter2 := BloomFilter2[int]{}
	println(filter2.hash_func(11))
	assert filter2.hash_func(11) == 22
}
