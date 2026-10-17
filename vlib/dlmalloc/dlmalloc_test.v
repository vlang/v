module dlmalloc

// This is an internal test, so `v test` type-checks every function body of the
// module, including the allocator paths that no other vlib code calls.
fn test_least_bit_isolates_the_lowest_set_bit() {
	assert least_bit(0b1011_0100) == 0b100
	assert least_bit(u32(1) << 31) == u32(1) << 31
	assert least_bit(0) == 0
}

fn test_new_starts_with_empty_bins() {
	dl := new(get_system_allocator())
	assert dl.smallmap == 0
	assert dl.treemap == 0
	assert dl.footprint == 0
}
