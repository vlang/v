module fixturetest

import os

fn test_is_diagnostic_fixture_dir_restricts_dispatch_to_diagnostic_suites() {
	root := os.join_path(os.temp_dir(), 'v3_fixturetest_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}

	checker_dir := os.join_path(root, 'vlib', 'v', 'checker', 'tests')
	os.mkdir_all(checker_dir) or { panic(err) }
	os.write_file(os.join_path(checker_dir, 'sample_test.vv'), 'fn main() {}\n') or { panic(err) }
	os.write_file(os.join_path(checker_dir, 'sample_test.out'), '') or { panic(err) }
	assert is_diagnostic_fixture_dir(checker_dir)
	os.write_file(os.join_path(checker_dir, 'var_duplicate_const.vv'), 'fn main() {}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(checker_dir, 'var_duplicate_const.out'), 'unstable output\n') or {
		panic(err)
	}
	assert is_comparable_fixture(checker_dir, 'sample_test.vv')
	assert !is_comparable_fixture(checker_dir, 'var_duplicate_const.vv')

	runtime_dir := os.join_path(root, 'vlib', 'v', 'gen', 'c', 'testdata')
	os.mkdir_all(runtime_dir) or { panic(err) }
	os.write_file(os.join_path(runtime_dir, 'sample.vv'), "fn main() { println('runtime') }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(runtime_dir, 'sample.out'), 'runtime\n') or { panic(err) }
	assert !is_diagnostic_fixture_dir(runtime_dir)

	os.write_file(os.join_path(checker_dir, 'compiler_test.v'), 'module main\n') or { panic(err) }
	assert !is_diagnostic_fixture_dir(checker_dir)
}

fn test_fixture_repo_root_falls_back_to_requested_suite() {
	base := os.join_path(os.temp_dir(), 'v3_fixture_root_${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}

	repo_root := os.join_path(base, 'checkout')
	fixture_dir := os.join_path(repo_root, 'vlib', 'v', 'checker', 'tests')
	os.mkdir_all(fixture_dir) or { panic(err) }
	os.mkdir_all(os.join_path(repo_root, 'vlib', 'v3')) or { panic(err) }

	install_dir := os.join_path(base, 'installed', 'bin')
	os.mkdir_all(install_dir) or { panic(err) }
	vexe := os.join_path(install_dir, 'v3')
	os.write_file(vexe, '') or { panic(err) }

	assert fixture_repo_root(vexe, fixture_dir) == repo_root
}

fn test_expected_fixture_exit_code_follows_diagnostic_severity() {
	assert expected_fixture_exit_code('') == 0
	assert expected_fixture_exit_code('sample.vv:1:1: warning: warning text') == 0
	assert expected_fixture_exit_code('sample.vv:1:1: notice: notice text') == 0
	assert expected_fixture_exit_code('sample.vv:1:1: error: error text') == 1
	assert expected_fixture_exit_code('sample.vv:1:1: builder error: missing module') == 1
	assert expected_fixture_exit_code('sample.vv:1:1: cgen error: invalid expression') == 1
	assert expected_fixture_exit_code("    1 | println(': error: source text')") == 0
	assert expected_fixture_exit_code('sample.vv:1:1: error: cannot use `(i8 | i16 | int | i64)` as type `SimpleInt`') == 1
}

fn test_fixture_result_requires_expected_exit_code() {
	error_output := 'sample.vv:1:1: error: error text'
	assert fixture_result_matches('', '', 0, 0)
	assert !fixture_result_matches('', '', 0, 1)
	assert fixture_result_matches(error_output, error_output, 1, 1)
	assert !fixture_result_matches(error_output, error_output, 1, 0)
}
