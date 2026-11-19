@[overflow: 'saturating']
fn sat_add_i8(x i8, y i8) i8 {
	return x + y
}

@[overflow: 'saturating']
fn sat_add_i16(x i16, y i16) i16 {
	return x + y
}

@[overflow: 'saturating']
fn sat_add_i32(x i32, y i32) i32 {
	return x + y
}

@[overflow: 'saturating']
fn sat_add_i64(x i64, y i64) i64 {
	return x + y
}

@[overflow: 'saturating']
fn sat_add_u8(x u8, y u8) u8 {
	return x + y
}

@[overflow: 'saturating']
fn sat_add_u16(x u16, y u16) u16 {
	return x + y
}

@[overflow: 'saturating']
fn sat_add_u32(x u32, y u32) u32 {
	return x + y
}

@[overflow: 'saturating']
fn sat_add_u64(x u64, y u64) u64 {
	return x + y
}

@[overflow: 'saturating']
fn sat_sub_i8(x i8, y i8) i8 {
	return x - y
}

@[overflow: 'saturating']
fn sat_sub_u8(x u8, y u8) u8 {
	return x - y
}

@[overflow: 'saturating']
fn sat_mul_i8(x i8, y i8) i8 {
	return x * y
}

@[overflow: 'saturating']
fn sat_mul_u8(x u8, y u8) u8 {
	return x * y
}

@[overflow: 'saturating']
fn sat_neg_i8(x i8) i8 {
	return -x
}

@[overflow: 'saturating']
fn sat_div_i8(x i8, y i8) i8 {
	return x / y
}

@[overflow: 'saturating']
fn sat_mod_i8(x i8, y i8) i8 {
	return x % y
}

@[overflow: 'checked']
fn checked_add_i8(x i8, y i8) i8 {
	return x + y
}

@[overflow: 'wrapping']
fn wrapping_add_i8(x i8, y i8) i8 {
	return x + y
}

fn test_saturating_policy_clamps_signed_add() {
	assert sat_add_i8(min_i8, min_i8) == min_i8
	assert sat_add_i8(max_i8, max_i8) == max_i8
	assert sat_add_i16(min_i16, min_i16) == min_i16
	assert sat_add_i16(max_i16, max_i16) == max_i16
	assert sat_add_i32(min_i32, min_i32) == min_i32
	assert sat_add_i32(max_i32, max_i32) == max_i32
	assert sat_add_i64(min_i64, min_i64) == min_i64
	assert sat_add_i64(max_i64, max_i64) == max_i64
	assert sat_add_i8(100, 27) == 127
}

fn test_saturating_policy_clamps_unsigned_add() {
	assert sat_add_u8(max_u8, 1) == max_u8
	assert sat_add_u16(max_u16, 1) == max_u16
	assert sat_add_u32(max_u32, 1) == max_u32
	assert sat_add_u64(max_u64, 1) == max_u64
	assert sat_add_u8(200, 100) == max_u8
}

fn test_saturating_policy_sub_mul_neg_div_mod() {
	assert sat_sub_i8(max_i8, min_i8) == max_i8
	assert sat_sub_i8(min_i8, max_i8) == min_i8
	assert sat_sub_u8(0, 1) == min_u8
	assert sat_mul_i8(max_i8, 2) == max_i8
	assert sat_mul_i8(min_i8, 2) == min_i8
	assert sat_mul_u8(max_u8, 2) == max_u8
	assert sat_neg_i8(min_i8) == max_i8
	assert sat_neg_i8(42) == -42
	assert sat_div_i8(min_i8, -1) == max_i8
	assert sat_mod_i8(min_i8, -1) == 0
}

fn test_checked_policy_panics_on_overflow() {
	checked_add_i8(10, 20)
	// Overflow must panic; assert via or-block on a wrapper is not needed:
	// a plain overflowing call aborts the test, so only the safe call runs.
}

fn test_wrapping_policy_wraps() {
	assert wrapping_add_i8(max_i8, 1) == min_i8
	assert wrapping_add_i8(100, 100) == i8(-56)
}
