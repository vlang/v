import os

fn vroot_path(relpath string) string {
	return os.real_path(os.join_path(@VMODROOT, relpath))
}

fn vexecute(relpath string) os.Result {
	return os.execute('${@VEXE} ' + vroot_path(relpath))
}

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn test_returning_options() {
	res := vexecute('vlib/v/tests/testdata/tests_returning_options_failing_test.v')
	assert res.exit_code == 1
	// dump(res)
	assert res.output.contains('tests_returning_options_failing_test.v:13: fn test_example failed propagation with error: failing test with return, err: oh no')
	assert res.output.contains('tests_returning_options_failing_test.v:19: fn test_example_2 failed propagation with error: oh no')
}

fn test_sizeof_in_assert() {
	res := vexecute('vlib/v/tests/testdata/sizeof_used_in_assert_test.v')
	assert res.exit_code == 1
	// dump(res)
	assert res.output.contains('sizeof_used_in_assert_test.v:11: fn test_assert_offsetof')
	assert res.output.contains('assert __offsetof(main.Abc, y) == 1')
	//
	assert res.output.contains('sizeof_used_in_assert_test.v:15: fn test_assert_sizeof')
	assert res.output.contains('assert sizeof(main.Abc) == sizeof(main.Xyz)')
}
