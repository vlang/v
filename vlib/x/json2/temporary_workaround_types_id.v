module json2

fn gen_workaround<T>(result T) T {
	return result
}

fn gen_workaround_result<T>(result T) ?T {
	return result
}

fn gen_workaround_optional<T>(result T) !T {
	return result
}

const (
	string_type_idx              = typeof(gen_workaround<string>(unsafe { nil })).idx
	result_string_type_idx       = typeof(gen_workaround_result<string>(unsafe { nil })).idx
	optional_string_type_idx     = typeof(gen_workaround_optional<string>(unsafe { nil })).idx

	int_array_type_idx           = typeof(gen_workaround<[]int>(unsafe { nil })).idx
	result_int_array_type_idx    = typeof(gen_workaround_result<[]int>(unsafe { nil })).idx
	optional_int_array_type_idx  = typeof(gen_workaround_optional<[]int>(unsafe { nil })).idx

	byte_array_type_idx          = typeof(gen_workaround<[]byte>(unsafe { nil })).idx
	result_byte_array_type_idx   = typeof(gen_workaround_result<[]byte>(unsafe { nil })).idx
	optional_byte_array_type_idx = typeof(gen_workaround_optional<[]byte>(unsafe { nil })).idx

	bool_type_idx                = typeof(gen_workaround<bool>(unsafe { nil })).idx
	result_bool_type_idx         = typeof(gen_workaround_result<bool>(unsafe { nil })).idx
	optional_bool_type_idx       = typeof(gen_workaround_optional<bool>(unsafe { nil })).idx

	f32_type_idx                 = typeof(gen_workaround<f32>(unsafe { nil })).idx

	f64_type_idx                 = typeof(gen_workaround<f64>(unsafe { nil })).idx

	i8_type_idx                  = typeof(gen_workaround<i8>(unsafe { nil })).idx

	i16_type_idx                 = typeof(gen_workaround<i16>(unsafe { nil })).idx

	int_type_idx                 = typeof(gen_workaround<int>(unsafe { nil })).idx

	i64_type_idx                 = typeof(gen_workaround<i64>(unsafe { nil })).idx

	u8_type_idx                  = typeof(gen_workaround<u8>(unsafe { nil })).idx

	u16_type_idx                 = typeof(gen_workaround<u16>(unsafe { nil })).idx

	u32_type_idx                 = typeof(gen_workaround<u32>(unsafe { nil })).idx

	u64_type_idx                 = typeof(gen_workaround<u64>(unsafe { nil })).idx
)
