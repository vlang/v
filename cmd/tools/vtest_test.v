import os
import encoding.txtar

const vexe = @VEXE
const vroot = os.dir(vexe)
const tpath = os.join_path(os.vtmp_dir(), 'vtest_folder')
const tpath_passing = os.join_path(tpath, 'passing')
const tpath_partial = os.join_path(tpath, 'partial')
const mytest_exe = os.join_path(tpath, 'mytest.exe')

fn testsuite_end() {
	os.rmdir_all(tpath) or {}
}

fn testsuite_begin() {
	os.setenv('VFLAGS', '', true)
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VJOBS', '2', true)
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!

	txtar.parse('Some known test files to make sure `v test` and `v -stats test` work:
-- passing/1_test.v --
fn test_abc() { assert true; assert true; assert true }
fn test_def() { assert 2 * 2 == 4 }
-- passing/2_test.v --
fn test_xyz() { assert 1 == 2 - 1 }
fn test_abc() { assert 10 == 2 * 5 }
-- partial/passing_test.v --
fn test_xyz() { assert 3 == 10 - 7 }
fn test_def() { assert 10 == 100 / 10 }
-- partial/failing_test.v --
fn test_xyz() { assert 5 == 7, "oh no" }
').unpack_to(tpath)!
	assert os.exists(os.join_path(tpath, 'passing/1_test.v'))
	assert os.exists(os.join_path(tpath, 'passing/2_test.v'))
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
	res := os.execute_or_exit('${os.quoted_path(mytest_exe)} -stats test ${os.quoted_path(tpath_passing)}')
	assert res.output.contains('1 assert'), res.output
	assert res.output.contains('3 asserts'), res.output
	assert res.output.contains('2 passed, 2 total'), res.output
	assert res.output.count('OK') == 6, res.output
}

fn test_partial_failure() {
	res := os.execute('${os.quoted_path(mytest_exe)} test ${os.quoted_path(tpath_partial)}')
	assert res.exit_code == 1
	assert res.output.contains('assert 5 == 7'), res.output
	assert res.output.contains(' 1 failed, 1 passed, 2 total'), res.output
	assert res.output.contains('To reproduce just failure'), res.output
}

fn test_with_stats_and_partial_failure() {
	res := os.execute('${os.quoted_path(mytest_exe)} -stats test ${os.quoted_path(tpath_partial)}')
	assert res.exit_code == 1
	assert res.output.contains('assert 5 == 7'), res.output
	assert res.output.contains(' 1 failed, 1 passed, 2 total'), res.output
	assert res.output.contains('To reproduce just failure'), res.output
}
