import os

const assert_failed_defer_cleanup_path = os.join_path(os.vtmp_dir(),
	'v_assert_failed_defer_cleanup_test.txt')

fn vroot_path(relpath string) string {
	return os.real_path(os.join_path(@VMODROOT, relpath))
}

fn vexecute(relpath string) os.Result {
	vexe := @VEXE
	return os.execute('${os.quoted_path(vexe)} -test-runner normal ' +
		os.quoted_path(vroot_path(relpath)))
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

	assert res.output.contains('sizeof_used_in_assert_test.v:15: fn test_assert_sizeof')
	assert res.output.contains('assert sizeof(main.Abc) == sizeof(main.Xyz)')
}

fn test_assert_failure_runs_scoped_defer_cleanup() {
	defer {
		os.rm(assert_failed_defer_cleanup_path) or {}
	}
	os.rm(assert_failed_defer_cleanup_path) or {}
	res := vexecute('vlib/v/tests/testdata/assert_with_scoped_defer_cleanup_failing_test.v')
	assert res.exit_code == 1
	assert res.output.contains('assert_with_scoped_defer_cleanup_failing_test.v')
	assert res.output.contains('assert false')
	assert !os.exists(assert_failed_defer_cleanup_path)
}

fn test_run_only_reports_filtered_failures() {
	test_path := os.join_path(os.vtmp_dir(), 'issue_22778_run_only_${os.getpid()}_test.v')
	defer {
		os.rm(test_path) or {}
	}
	test_source := [
		'fn test_ok() {',
		'	assert true',
		'}',
		'',
		'fn test_fail() {',
		"	assert false, 'expected to fail'",
		'}',
	].join_lines()
	os.write_file(test_path, test_source)!
	res :=
		os.execute('${os.quoted_path(@VEXE)} -test-runner normal -run-only test_fail ${os.quoted_path(test_path)}')
	assert res.exit_code == 1
	assert res.output.contains('fn test_fail'), res.output
	assert res.output.contains('expected to fail'), res.output
	assert !res.output.contains('fn test_ok'), res.output
}
