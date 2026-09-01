module driver

fn test_input_is_v3_compiler_tree() {
	assert input_is_v3_compiler_tree('${@VEXEROOT}/vlib/v3')
	assert input_is_v3_compiler_tree('${@VEXEROOT}/vlib/v3/transform/fn_test.v')
	assert !input_is_v3_compiler_tree('${@VEXEROOT}/cmd/v')
	assert !input_is_v3_compiler_tree('${@VEXEROOT}/vlib/v/tests/array_test.v')
}
