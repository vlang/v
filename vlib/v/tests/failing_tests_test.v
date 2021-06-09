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
	dump(res)
	assert res.output.contains('tests_returning_options_failing_test.v:13: fn test_example failed propagation with error: failing test with return, err: oh no')
	assert res.output.contains('tests_returning_options_failing_test.v:19: fn test_example_2 failed propagation with error: oh no')
}
