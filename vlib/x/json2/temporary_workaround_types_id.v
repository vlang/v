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

	int_type_idx                 = typeof(gen_workaround<int>(unsafe { nil })).idx
	result_int_type_idx          = typeof(gen_workaround_result<int>(unsafe { nil })).idx
	optional_int_type_idx        = typeof(gen_workaround_optional<int>(unsafe { nil })).idx

	int_array_type_idx           = typeof(gen_workaround<[]int>(unsafe { nil })).idx
	result_int_array_type_idx    = typeof(gen_workaround_result<[]int>(unsafe { nil })).idx
	optional_int_array_type_idx  = typeof(gen_workaround_optional<[]int>(unsafe { nil })).idx

	byte_array_type_idx          = typeof(gen_workaround<[]byte>(unsafe { nil })).idx
	result_byte_array_type_idx   = typeof(gen_workaround_result<[]byte>(unsafe { nil })).idx
	optional_byte_array_type_idx = typeof(gen_workaround_optional<[]byte>(unsafe { nil })).idx
)
