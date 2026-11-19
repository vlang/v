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
