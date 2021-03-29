import v.tests.project_with_c_code_2.modc

fn test_using_c_code_in_the_same_module_works() {
	x := modc.new_vtype(123)
	modc.destroy_vtype(x)
	assert true
}
