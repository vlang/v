module overflow

fn test_add_sat_signed_min_plus_min_saturates_to_min() {
	assert add_sat_i8(min_i8, min_i8) == min_i8
	assert add_sat_i16(min_i16, min_i16) == min_i16
	assert add_sat_i32(min_i32, min_i32) == min_i32
	assert add_sat_i64(min_i64, min_i64) == min_i64
}

fn test_add_sat_signed_max_plus_max_saturates_to_max() {
	assert add_sat_i8(max_i8, max_i8) == max_i8
	assert add_sat_i16(max_i16, max_i16) == max_i16
	assert add_sat_i32(max_i32, max_i32) == max_i32
	assert add_sat_i64(max_i64, max_i64) == max_i64
}

fn test_add_sat_signed_no_overflow() {
	assert add_sat_i8(100, 27) == 127
	assert add_sat_i8(-100, -27) == -127
	assert add_sat_i16(1000, -500) == 500
	assert add_sat_i32(-1, 1) == 0
	assert add_sat_i64(min_i64 + 1, -1) == min_i64
	assert add_sat_i64(max_i64 - 1, 1) == max_i64
}

fn test_add_sat_unsigned_wraps_to_max() {
	assert add_sat_u8(max_u8, 1) == max_u8
	assert add_sat_u16(max_u16, 1) == max_u16
	assert add_sat_u32(max_u32, 1) == max_u32
	assert add_sat_u64(max_u64, 1) == max_u64
	assert add_sat_u8(200, 100) == max_u8
}

fn test_sub_sat_signed_boundaries() {
	assert sub_sat_i8(min_i8, max_i8) == min_i8
	assert sub_sat_i8(max_i8, min_i8) == max_i8
	assert sub_sat_i16(min_i16, max_i16) == min_i16
	assert sub_sat_i16(max_i16, min_i16) == max_i16
	assert sub_sat_i32(min_i32, max_i32) == min_i32
	assert sub_sat_i32(max_i32, min_i32) == max_i32
	assert sub_sat_i64(min_i64, max_i64) == min_i64
	assert sub_sat_i64(max_i64, min_i64) == max_i64
	assert sub_sat_u8(0, 1) == min_u8
}

fn test_mul_sat_signed_min_times_minus_one() {
	assert mul_sat_i8(min_i8, -1) == max_i8
	assert mul_sat_i16(min_i16, -1) == max_i16
	assert mul_sat_i32(min_i32, -1) == max_i32
	assert mul_sat_i64(min_i64, -1) == max_i64
	assert mul_sat_i8(min_i8, min_i8) == max_i8
	assert mul_sat_i8(min_i8, 2) == min_i8
	assert mul_sat_i8(11, 11) == 121
}

fn test_neg_sat_signed_min() {
	assert neg_sat_i8(min_i8) == max_i8
	assert neg_sat_i16(min_i16) == max_i16
	assert neg_sat_i32(min_i32) == max_i32
	assert neg_sat_i64(min_i64) == max_i64
	assert neg_sat_i8(42) == -42
}
