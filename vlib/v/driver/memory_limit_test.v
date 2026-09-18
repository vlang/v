module driver

fn test_input_is_compiler_tree() {
	assert input_is_compiler_tree('${@VEXEROOT}/vlib/v')
	assert input_is_compiler_tree('${@VEXEROOT}/vlib/v/transform/fn_test.v')
	assert input_is_compiler_tree('${@VEXEROOT}/vlib/v/compiler_tests/driver_cli_test.v')
	assert !input_is_compiler_tree('${@VEXEROOT}/cmd/v')
	assert !input_is_compiler_tree('${@VEXEROOT}/vlib/v/tests/array_test.v')
	assert !input_is_compiler_tree('${@VEXEROOT}/vlib/v/parser/tests/invalid_syntax.vv')
	assert !input_is_compiler_tree('${@VEXEROOT}/vlib/v/slow_tests/inout/compiler_test.v')
}
