import os

const cleanup_path = os.join_path(os.vtmp_dir(), 'v_assert_failed_defer_cleanup_test.txt')
const deferred_file_close_path = os.join_path(os.vtmp_dir(),
	'v_assert_failed_deferred_file_close_test.txt')

fn deferred_file_close_line(i int) string {
	return '第${i:03}: {"code":200,"msg":"数据请求成功","word":"possible","meaning":"可能的；合适的","url":"https://dict.example/api?word=possible"}'
}

fn testsuite_begin() {
	os.rm(cleanup_path) or {}
	os.rm(deferred_file_close_path) or {}
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

fn test_deferred_file_close_flushes_on_assert_failure() {
	mut f := os.open_file(deferred_file_close_path, 'w+', 0o666)!
	defer {
		f.close()
	}
	for i in 0 .. 160 {
		f.writeln(deferred_file_close_line(i))!
	}
	assert false
}
