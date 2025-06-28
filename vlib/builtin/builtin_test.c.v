fn test_g_main_argc() {
	assert g_main_argc > 0
}

fn test_g_main_argv() {
	assert g_main_argv != 0
	mut first_arg := ''
	$if windows {
		first_arg = unsafe { string_from_wide(&&u16(g_main_argv)[0]) }
	} $else {
		first_arg = unsafe { cstring_to_vstring(&&char(g_main_argv)[0]) }
	}
	assert first_arg.contains('builtin_test')
}

@[if windows]
fn test_bool_size() {
	println(@LOCATION)
	assert sizeof(C.BOOL) == 4
}

fn test_min() {
	assert min(voidptr(100), voidptr(200)) == voidptr(100)
	assert min(`a`, `z`) == `a`
	assert min(1, 2) == 1
	assert min(f32(-2.3), f32(-1.1)) == f32(-2.3)
	assert min(f64(-2.3), f64(-1.1)) == f64(-2.3)
	assert min(u8(1), u8(2)) == u8(1)
	assert min(u16(0), u16(5)) == u16(0)
	assert min(u32(5), u32(0)) == u32(0)
	assert min(u64(1), u64(2)) == u64(1)
	assert min(usize(1), usize(2)) == usize(1)
	assert min(i8(-5), i8(5)) == i8(-5)
	assert min(i16(5), i16(-5)) == i16(-5)
	assert min(i32(5), i32(-5)) == i32(-5)
	assert min(i64(5), i64(-5)) == i64(-5)
	assert min(isize(5), isize(-5)) == isize(-5)

	assert min(1, 2, 3, 4, 5, 6, 7, 8, 9) == 1
	assert min(`a`, `b`, `c`, `d`, `e`) == `a`

	// special case test
	assert min[int]() == 0
	assert min(1) == 1
}

fn test_max() {
	assert max(voidptr(100), voidptr(200)) == voidptr(200)
	assert max(`a`, `z`) == `z`
	assert max(1, 2) == 2
	assert max(f32(-2.3), f32(-1.1)) == f32(-1.1)
	assert max(f64(-2.3), f64(-1.1)) == f64(-1.1)
	assert max(u8(1), u8(2)) == u8(2)
	assert max(u16(0), u16(5)) == u16(5)
	assert max(u32(5), u32(0)) == u32(5)
	assert max(u64(1), u64(2)) == u64(2)
	assert max(usize(1), usize(2)) == usize(2)
	assert max(i8(-5), i8(5)) == i8(5)
	assert max(i16(5), i16(-5)) == i16(5)
	assert max(i32(5), i32(-5)) == i32(5)
	assert max(i64(5), i64(-5)) == i64(5)
	assert max(isize(5), isize(-5)) == isize(5)

	assert max(1, 2, 3, 4, 5, 6, 7, 8, 9) == 9
	assert max(`a`, `b`, `c`, `d`, `e`) == `e`

	// special case test
	assert max[int]() == 0
	assert max(1) == 1
}
