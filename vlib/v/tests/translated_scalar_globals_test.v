@[has_globals; translated]
module main

__global translated_int_global int = u32(0xffff_ffff)
__global translated_bool_global bool = 256
__global translated_bool_fraction bool = 0.5
__global translated_bool_zero bool = 0
__global translated_bool_array [3]bool = [256, 0.5, 0]!

fn test_translated_int_global_uses_c_width() {
	assert translated_int_global == -1
}

fn test_translated_boolean_globals_are_normalized() {
	assert translated_bool_global == true
	assert translated_bool_fraction == true
	assert translated_bool_zero == false
	assert translated_bool_array == [true, true, false]!
}
