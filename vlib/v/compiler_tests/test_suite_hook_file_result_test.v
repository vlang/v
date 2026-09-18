import os

// `testsuite_begin` runs before the first per-file baseline is captured and `testsuite_end`
// runs after every per-file result is finalized, so a failing suite hook incremented the
// global `__test_failures` without touching any file's range: the file printed `OK` while the
// process exited nonzero. A plain `assert false` in a hook aborts the process, so the hooks
// here are `@[assert_continues]` -- that is the path that records a failure and keeps going,
// and therefore the one that could contradict itself.

const suite_hook_vexe = @VEXE

fn run_v_test(source string, name string) (string, int) {
	directory := os.join_path(os.vtmp_dir(), 'v_suite_hook_${name}_${os.getpid()}')
	os.rmdir_all(directory) or {}
	os.mkdir_all(directory) or { panic(err) }
	defer {
		os.rmdir_all(directory) or {}
	}
	path := os.join_path(directory, '${name}_test.v')
	os.write_file(path, source) or { panic(err) }
	// `VTEST_HIDE_OK=0` keeps the passing lines visible, so a file reported as `OK` is
	// distinguishable from a file that was not reported at all.
	os.setenv('VTEST_HIDE_OK', '0', true)
	result := os.execute('${os.quoted_path(suite_hook_vexe)} test ${os.quoted_path(path)}')
	return result.output, result.exit_code
}

fn test_a_failing_testsuite_begin_fails_its_file() {
	output, code := run_v_test('@[assert_continues]
fn testsuite_begin() {
	assert 1 == 2, "suite begin failed"
}

fn test_that_passes() {
	assert true
}
', 'begin')
	assert code != 0, 'a failing suite hook has to fail the run:\n${output}'
	assert output.contains('FAIL'), 'the file has to be reported as failing:\n${output}'
	assert !output.contains('OK    '), 'the file must not also be reported as passing:\n${output}'
}

fn test_a_failing_testsuite_end_fails_its_file() {
	output, code := run_v_test('@[assert_continues]
fn testsuite_end() {
	assert 1 == 2, "suite end failed"
}

fn test_that_passes() {
	assert true
}
', 'end')
	assert code != 0, 'a failing suite hook has to fail the run:\n${output}'
	assert output.contains('FAIL'), 'the file has to be reported as failing:\n${output}'
	assert !output.contains('OK    '), 'the file must not also be reported as passing:\n${output}'
}

// The hooks must not make a healthy file look broken either, or the fix would just move the
// contradiction to the other side.
fn test_passing_suite_hooks_keep_their_file_passing() {
	output, code := run_v_test('@[assert_continues]
fn testsuite_begin() {
	assert true
}

@[assert_continues]
fn testsuite_end() {
	assert true
}

fn test_that_passes() {
	assert true
}
', 'clean')
	assert code == 0, 'a passing run has to succeed:\n${output}'
	assert output.contains('OK    '), 'the file has to be reported as passing:\n${output}'
	assert !output.contains('FAIL'), 'nothing may be reported as failing:\n${output}'
}
