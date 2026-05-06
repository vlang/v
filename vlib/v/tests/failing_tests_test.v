import os

const assert_failed_defer_cleanup_path = os.join_path(os.vtmp_dir(),
	'v_assert_failed_defer_cleanup_test.txt')
const assert_failed_deferred_file_close_path = os.join_path(os.vtmp_dir(),
	'v_assert_failed_deferred_file_close_test.txt')

fn vroot_path(relpath string) string {
	return os.real_path(os.join_path(@VMODROOT, relpath))
}

fn vexecute(relpath string) os.Result {
	vexe := @VEXE
	return os.execute('${os.quoted_path(vexe)} -test-runner normal ' +
		os.quoted_path(vroot_path(relpath)))
}

fn deferred_file_flush_line(i int) string {
	return '第${i:03}: {"code":200,"msg":"数据请求成功","word":"possible","meaning":"可能的；合适的","url":"https://dict.example/api?word=possible"}'
}

fn deferred_file_flush_expected() string {
	line_sep := $if windows { '\r\n' } $else { '\n' }
	mut lines := []string{cap: 160}
	for i in 0 .. 160 {
		lines << deferred_file_flush_line(i)
	}
	return lines.join(line_sep) + line_sep
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

fn test_assert_failure_runs_deferred_file_close_flush() {
	defer {
		os.rm(assert_failed_deferred_file_close_path) or {}
	}
	os.rm(assert_failed_deferred_file_close_path) or {}
	res := vexecute('vlib/v/tests/testdata/assert_with_scoped_defer_cleanup_failing_test.v')
	assert res.exit_code == 1
	assert res.output.contains('assert_with_scoped_defer_cleanup_failing_test.v')
	assert res.output.contains('assert false')
	content := os.read_file(assert_failed_deferred_file_close_path)!
	assert content == deferred_file_flush_expected()
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

fn test_before_each_and_after_each_run_around_each_test() {
	test_path := os.join_path(os.vtmp_dir(), 'issue_19699_${os.getpid()}_test.v')
	log_path := os.join_path(os.vtmp_dir(), 'issue_19699_${os.getpid()}.log')
	defer {
		os.rm(test_path) or {}
		os.rm(log_path) or {}
		os.unsetenv('V_ISSUE_19699_LOG_PATH')
	}
	test_source := [
		'import os',
		'',
		"const issue_19699_log_path = os.getenv('V_ISSUE_19699_LOG_PATH')",
		'',
		'fn append_log(line string) {',
		'	mut entries := os.read_lines(issue_19699_log_path) or { []string{} }',
		'	entries << line',
		"	os.write_file(issue_19699_log_path, entries.join_lines() + '\\n') or { panic(err) }",
		'}',
		'',
		"fn testsuite_begin() { append_log('testsuite_begin') }",
		"fn before_each() { append_log('before_each') }",
		"fn after_each() { append_log('after_each') }",
		"fn test_one() { append_log('test_one') }",
		"fn test_two() { append_log('test_two') }",
		"fn testsuite_end() { append_log('testsuite_end') }",
	].join_lines()
	os.write_file(test_path, test_source)!
	os.rm(log_path) or {}
	os.setenv('V_ISSUE_19699_LOG_PATH', log_path, true)
	res := os.execute('${os.quoted_path(@VEXE)} -test-runner normal ${os.quoted_path(test_path)}')
	assert res.exit_code == 0, res.output
	log_lines := os.read_lines(log_path)!
	assert log_lines == [
		'testsuite_begin',
		'before_each',
		'test_one',
		'after_each',
		'before_each',
		'test_two',
		'after_each',
		'testsuite_end',
	]
}

fn test_windows_c_system_info_is_undefined_on_non_windows() {
	$if windows {
		return
	}
	source_path := os.join_path(os.vtmp_dir(), 'issue_25821_${os.getpid()}.v')
	source := [
		'module main',
		'fn main() {',
		'	x := C.SYSTEM_INFO{}',
		'	dump(x)',
		'}',
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
	}
	res := os.execute('${os.quoted_path(@VEXE)} ${os.quoted_path(source_path)}')
	assert res.exit_code != 0, res.output
	assert res.output.contains('unknown type `C.SYSTEM_INFO`'), res.output
	assert !res.output.contains('C compilation error'), res.output
	assert !res.output.contains('builder error'), res.output
}
