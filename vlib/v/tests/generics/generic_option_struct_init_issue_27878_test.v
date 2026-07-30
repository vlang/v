fn generic_option_default[T]() ?T {
	mut opt := ?T{}
	return opt
}

fn test_generic_option_struct_init_preserves_option_flag() {
	assert generic_option_default[int]()? == 0
}
