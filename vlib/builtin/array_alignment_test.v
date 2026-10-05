module builtin

fn test_array_alignment_does_not_overlap_retained_storage_flags() {
	for alignment in [usize(16), 32, 64, 128, 4096] {
		mut value := array{}
		value.set_data_alignment(alignment)
		value.mark_aligned_fixed_array_buffer()
		assert value.data_alignment() == alignment
		assert value.buffer_has_slices()
		value.set_data_alignment(64)
		assert value.data_alignment() == 64
		assert value.flags.has(array_flag_retained_aligned_fixed)
	}
}

fn test_aligned_array_header_initializes_retained_views() {
	mut value := __new_array_aligned(0, 4, int(sizeof(u64)), 64)
	assert usize(value.data) % 64 == 0
	assert !value.buffer_has_retained_fixed_views()
	assert !value.buffer_has_slices()
	unsafe { value.free() }
}
