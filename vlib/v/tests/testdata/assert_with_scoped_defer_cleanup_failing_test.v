import os

const cleanup_path = os.join_path(os.vtmp_dir(), 'v_assert_failed_defer_cleanup_test.txt')

fn testsuite_begin() {
	os.rm(cleanup_path) or {}
}

fn test_scoped_defer_cleanup_runs_on_assert_failure() {
	{
		os.write_file(cleanup_path, 'temporary file') or { panic(err) }
		defer {
			os.rm(cleanup_path) or {}
		}
		assert false
	}
}
