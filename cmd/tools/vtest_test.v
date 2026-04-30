import os
import encoding.txtar

const vexe = @VEXE
const vroot = os.dir(vexe)
const tpath = os.join_path(os.vtmp_dir(), 'vtest_folder')
const tpath_passing = os.join_path(tpath, 'passing')
const tpath_impure = os.join_path(tpath, 'impure')
const tpath_js_runtime_error = os.join_path(tpath, 'js_runtime_error')
const tpath_partial = os.join_path(tpath, 'partial')
const mytest_exe = os.join_path(tpath, 'mytest.exe')

fn testsuite_end() {
	os.rmdir_all(tpath) or {}
}

fn testsuite_begin() {
	os.setenv('VFLAGS', '', true)
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VJOBS', '2', true)
	os.setenv('VTEST_HIDE_OK', '0', true)
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!

	txtar.parse('Some known test files to make sure `v test` and `v -stats test` work:
-- passing/1_test.v --
fn test_abc() { assert true; assert true; assert true }
fn test_def() { assert 2 * 2 == 4 }
-- passing/2_test.v --
fn test_xyz() { assert 1 == 2 - 1 }
fn test_abc() { assert 10 == 2 * 5 }
-- impure/warning_test.v --
fn test_warning() {
	C.printf(c"")
}
-- js_runtime_error/runtime_error_test.js.v --
fn test_runtime_error() {
	JS.eval("(()=>{ throw new TypeError(`boom`) })()".str)
}
-- partial/passing_test.v --
fn test_xyz() { assert 3 == 10 - 7 }
fn test_def() { assert 10 == 100 / 10 }
-- partial/failing_test.v --
fn test_xyz() { assert 5 == 7, "oh no" }
').unpack_to(tpath)!
	assert os.exists(os.join_path(tpath, 'passing/1_test.v'))
	assert os.exists(os.join_path(tpath, 'passing/2_test.v'))
	assert os.exists(os.join_path(tpath, 'impure/warning_test.v'))
	assert os.exists(os.join_path(tpath, 'js_runtime_error/runtime_error_test.js.v'))
	assert os.exists(os.join_path(tpath, 'partial/passing_test.v'))
	assert os.exists(os.join_path(tpath, 'partial/failing_test.v'))
}

fn test_vtest_executable_compiles() {
	os.chdir(vroot)!
	os.execute_or_exit('${os.quoted_path(vexe)} -o ${tpath}/mytest.exe cmd/tools/vtest.v')
	assert os.exists(mytest_exe), 'executable file: `${mytest_exe}` should exist'
}

fn test_with_several_test_files() {
	res := os.execute_or_exit('${os.quoted_path(mytest_exe)} test ${os.quoted_path(tpath_passing)}')
	assert !res.output.contains('1 assert'), res.output
	assert !res.output.contains('3 asserts'), res.output
	assert res.output.contains('2 passed, 2 total'), res.output
	assert res.output.count('OK') == 2, res.output
	assert res.output.contains('on 2 parallel jobs'), res.output
}

fn test_with_stats_and_several_test_files() {
	// There should be more OKs here, since the output will have the inner OKs for each individual test fn:
	res :=
		os.execute_or_exit('${os.quoted_path(mytest_exe)} -stats test ${os.quoted_path(tpath_passing)}')
	assert res.output.contains('1 assert'), res.output
	assert res.output.contains('3 asserts'), res.output
	assert res.output.contains('2 passed, 2 total'), res.output
	assert res.output.count('OK') == 6, res.output
	run_1 := '1_test.v\n     OK'
	run_2 := '2_test.v\n     OK'
	summary_1 := 'Summary for running V tests in "1_test.v"'
	summary_2 := 'Summary for running V tests in "2_test.v"'
	run_1_idx := res.output.index(run_1) or { -1 }
	run_2_idx := res.output.index(run_2) or { -1 }
	summary_1_idx := res.output.index(summary_1) or { -1 }
	summary_2_idx := res.output.index(summary_2) or { -1 }
	assert run_1_idx != -1, res.output
	assert run_2_idx != -1, res.output
	assert summary_1_idx != -1, res.output
	assert summary_2_idx != -1, res.output
	assert run_1_idx < summary_1_idx, res.output
	assert run_2_idx < summary_2_idx, res.output
	assert summary_1_idx < run_2_idx || summary_2_idx < run_1_idx, res.output
}

fn test_partial_failure() {
	res := os.execute('${os.quoted_path(mytest_exe)} test ${os.quoted_path(tpath_partial)}')
	assert res.exit_code == 1
	assert res.output.contains('assert 5 == 7'), res.output
	assert res.output.contains(' 1 failed, 1 passed, 2 total'), res.output
	assert res.output.contains('To reproduce just failure'), res.output
}

fn test_run_only_reports_filtered_failures() {
	failing_test_path := os.join_path(tpath_partial, 'failing_test.v')
	res :=
		os.execute('${os.quoted_path(mytest_exe)} test -run-only test_xyz ${os.quoted_path(failing_test_path)}')
	assert res.exit_code == 1
	assert res.output.contains('assert 5 == 7'), res.output
	assert res.output.contains(' 1 failed, 1 total'), res.output
}

fn test_wimpure_v_warnings_are_shown_for_test_files() {
	res :=
		os.execute_or_exit('${os.quoted_path(mytest_exe)} -Wimpure-v test ${os.quoted_path(tpath_impure)}')
	assert res.output.contains('warning_test.v'), res.output
	assert res.output.contains('warning: C code will not be allowed in pure .v files'), res.output
}

fn test_js_runtime_errors_are_shown_for_js_tests() {
	if @CCOMPILER.contains('musl') || os.getenv('VFLAGS').contains('musl') {
		return
	}
	if os.execute('node --version').exit_code != 0 {
		return
	}
	res :=
		os.execute('${os.quoted_path(mytest_exe)} test ${os.quoted_path(tpath_js_runtime_error)}')
	assert res.exit_code == 1, res.output
	assert res.output.contains('runtime_error_test.js.v'), res.output
	assert res.output.contains('TypeError: boom'), res.output
}

fn test_with_stats_and_partial_failure() {
	res := os.execute('${os.quoted_path(mytest_exe)} -stats test ${os.quoted_path(tpath_partial)}')
	assert res.exit_code == 1
	assert res.output.contains('assert 5 == 7'), res.output
	assert res.output.contains(' 1 failed, 1 passed, 2 total'), res.output
	assert res.output.contains('To reproduce just failure'), res.output
}
