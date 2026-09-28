@[has_globals; translated]
module main

__global translated_int_global int = u32(0xffff_ffff)

fn test_translated_int_global_uses_c_width() {
	assert translated_int_global == -1
}
