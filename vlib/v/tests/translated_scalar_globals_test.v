@[has_globals; translated]
module main

__global translated_int_global int = u32(0xffff_ffff)
__global translated_bool_global bool = 256
__global translated_bool_fraction bool = 0.5
__global translated_bool_zero bool = 0
__global translated_bool_array [3]bool = [256, 0.5, 0]!

type ScalarGlobalInt = int
type ScalarGlobalBool = bool

struct ScalarGlobalFields {
	value    int
	alias    ScalarGlobalInt
	wide     i64
	flag     bool
	fraction ScalarGlobalBool
	zero     bool
}

const scalar_global_wide = u32(0xffff_ffff)
const scalar_global_nonzero = 256
const scalar_global_fraction = 0.5
const scalar_global_zero = 0

@[cinit]
__global translated_scalar_fields = ScalarGlobalFields{
	value:    scalar_global_wide
	alias:    scalar_global_wide
	wide:     scalar_global_wide
	flag:     scalar_global_nonzero
	fraction: scalar_global_fraction
	zero:     scalar_global_zero
}

fn test_translated_int_global_uses_c_width() {
	assert translated_int_global == -1
}

fn test_translated_static_struct_fields_convert_constants() {
	assert i64(translated_scalar_fields.value) == -1
	assert i64(translated_scalar_fields.alias) == -1
	assert translated_scalar_fields.wide == i64(0xffff_ffff)
	assert translated_scalar_fields.flag == true
	assert translated_scalar_fields.fraction == ScalarGlobalBool(true)
	assert translated_scalar_fields.zero == false
}

fn test_translated_boolean_globals_are_normalized() {
	assert translated_bool_global == true
	assert translated_bool_fraction == true
	assert translated_bool_zero == false
	assert translated_bool_array == [true, true, false]!
}
