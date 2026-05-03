type IntMapper = fn (int) int

type StringMapper = fn (string) string

fn is_int_mapper[T]() bool {
	$if T is fn (int) int {
		return true
	} $else {
		return false
	}
}

fn is_ref_int_mapper[T]() bool {
	$if T is &fn (int) int {
		return true
	} $else {
		return false
	}
}

fn accepts_fixed_array_param_mapper[T]() bool {
	return true
}

fn test_comptime_if_fn_type_signature() {
	assert is_int_mapper[IntMapper]()
	assert !is_int_mapper[StringMapper]()
	assert is_int_mapper[fn (int) int]()
	assert !is_int_mapper[&fn (int) int]()
	assert !is_int_mapper[fn (string) string]()
}

fn test_comptime_if_fn_type_signature_with_shared_modifier() {
	assert is_ref_int_mapper[&fn (int) int]()
	assert !is_ref_int_mapper[shared fn (int) int]()
	assert !is_int_mapper[atomic fn (int) int]()
}

fn test_fn_type_generic_call_detection_with_fixed_array_param() {
	assert accepts_fixed_array_param_mapper[fn ([4]int) bool]()
}
