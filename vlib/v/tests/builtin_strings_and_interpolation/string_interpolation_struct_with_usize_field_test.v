struct StructWithUsizeField {
	f_usize usize
	f_u64   u64
	f_u32   u32

	f_isize isize
	f_i64   i64
	f_i32   i32
}

fn test_stringified_usize_field_should_be_always_positive() {
	a := StructWithUsizeField{
		f_usize: usize(-1)
		f_isize: isize(-1)
		f_u64:   u64(-1)
		f_i64:   i64(-1)
		f_u32:   u32(-1)
		f_i32:   i32(-1)
	}
	// dump(a)
	sa := a.str()
	assert sa.contains('f_isize: -1')
	assert sa.contains('f_i64: -1')
	assert sa.contains('f_i32: -1')
	i := sa.split_into_lines().filter(it.contains('isize'))[0]
	assert i.contains('-'), 'all `i` fields should be negative, but ${i} != ${a.f_isize}'

	assert sa.contains('f_u64: 18446744073709551615')
	assert sa.contains('f_u32: 4294967295')
	u := sa.split_into_lines().filter(it.contains('usize'))[0]
	assert !u.contains('-'), 'all `u` fields should be positive, but ${u} != ${a.f_usize}'
}
