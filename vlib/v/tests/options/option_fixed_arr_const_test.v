// Test for or-block returning a fixed array constant.
// Regression test for issue where cgen generated invalid C cast to array type.
const default_arr = [f32(1.0), 2.0, 3.0, 4.0]!

fn get_arr(key string) ?[4]f32 {
	if key == 'valid' {
		return [f32(0.1), 0.2, 0.3, 0.4]!
	}
	return none
}

fn test_or_block_with_fixed_array_const() {
	result := get_arr('invalid') or { default_arr }
	assert result == default_arr
}

fn test_or_block_with_valid_key() {
	result := get_arr('valid') or { default_arr }
	assert result == [f32(0.1), 0.2, 0.3, 0.4]!
}
