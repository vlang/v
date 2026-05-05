fn option_fn_type_name[T]() string {
	return typeof[T]().name
}

fn is_option_fn[T]() bool {
	$if T is ?fn (int) int {
		return true
	} $else {
		return false
	}
}

fn test_option_fn_concrete_type() {
	assert option_fn_type_name[fn (int) int]() == 'fn (int) int'
	assert option_fn_type_name[?fn (int) int]() == '?fn (int) int'
	assert is_option_fn[?fn (int) int]()
	assert !is_option_fn[fn (int) int]()
}
