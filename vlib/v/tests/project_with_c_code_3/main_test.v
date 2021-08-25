import v.tests.project_with_c_code_3.mod1

fn test_using_c_code_in_the_same_module_works() {
	$if js {
		assert 2003 == mod1.vadd(1, 2)
	} $else {
		assert 1003 == mod1.vadd(1, 2)
	}
}

fn test_a_common_pure_v_fn_works() {
	assert mod1.a_common_pure_v_fn() == 987654
}
