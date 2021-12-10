fn a_const_accepting_fn(x &int, const_x &int) int {
	return *x + *const_x
}

fn test_fn_with_const_ref_param_can_be_called() {
	a := 1
	b := 2
	assert a_const_accepting_fn(&a, &b) == 3
}
