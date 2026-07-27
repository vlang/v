module fixturetest

import os

fn test_is_fixture_dir_does_not_override_standard_test_harness() {
	dir := os.join_path(os.temp_dir(), 'v3_fixturetest_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}

	os.write_file(os.join_path(dir, 'sample_test.vv'), 'fn main() {}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'sample_test.out'), '') or { panic(err) }
	assert is_fixture_dir(dir)

	os.write_file(os.join_path(dir, 'compiler_test.v'), 'module main\n') or { panic(err) }
	assert !is_fixture_dir(dir)
}
