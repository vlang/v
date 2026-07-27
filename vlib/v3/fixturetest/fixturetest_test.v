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
