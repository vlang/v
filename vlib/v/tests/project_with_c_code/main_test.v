import v.tests.project_with_c_code.mod1

fn test_using_c_code_in_the_same_module_works() {
	assert 1003 == mod1.vadd(1, 2)
}
