fn test_array_indices_accept_any_integer_type() {
	s := 'abc'
	a := [1, 2, 3]
	idx_u32 := u32(1)
	idx_u64 := u64(1)
	idx_i64 := i64(1)
	idx_usize := usize(1)

	assert a[idx_u32] == 2
	assert a[idx_u64] == 2
	assert a[idx_i64] == 2
	assert a[..idx_usize].len == 1
	assert a[idx_usize..].len == 2
	assert a[idx_usize..][0] == 2
	assert s[idx_u64] == `b`
}
