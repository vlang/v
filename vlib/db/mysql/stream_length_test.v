module mysql

fn test_checked_stream_value_length() {
	assert checked_stream_value_length(0)! == 0
	assert checked_stream_value_length(u64(max_int))! == max_int
	if u64(max_int) < max_u64 {
		checked_stream_value_length(u64(max_int) + 1) or {
			assert err.msg().contains('exceeds the supported maximum')
			return
		}
		assert false
	}
}

fn test_c_mysql_lengths_allocator_pair() {
	lengths := C.v_mysql_lengths_new(2)
	assert lengths != unsafe { nil }
	assert C.v_mysql_length_at(lengths, 0) == 0
	assert C.v_mysql_length_at(lengths, 1) == 0
	C.v_mysql_lengths_free(lengths)
}
