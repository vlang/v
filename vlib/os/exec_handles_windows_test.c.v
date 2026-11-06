module os

fn C.GetProcessHandleCount(voidptr, &C.DWORD) i32

fn exec_test_handle_count() u32 {
	mut count := C.DWORD(0)
	assert C.GetProcessHandleCount(C.GetCurrentProcess(), &count) != 0
	return u32(count)
}

fn test_exec_releases_captured_pipe_handles() {
	// Warm command execution and the collector before measuring this process.
	for _ in 0 .. 2 {
		assert exec(['cmd', '/d', '/c', 'echo', 'captured']).exit_code == 0
		assert exec(['cmd', '/d', '/c', 'exit', '/b', '23']).exit_code == 23
	}
	gc_collect()
	before := exec_test_handle_count()
	for _ in 0 .. 24 {
		captured := exec(['cmd', '/d', '/c', 'echo', 'captured'])
		assert captured.exit_code == 0, captured.output
		assert captured.output == 'captured\r\n', captured.output
		exited := exec(['cmd', '/d', '/c', 'exit', '/b', '23'])
		assert exited.exit_code == 23, exited.output
		assert exited.output == '', exited.output
	}
	after := exec_test_handle_count()
	assert after == before, 'os.exec leaked handles: before=${before}, after=${after}'
}

fn test_exec_releases_pipe_handles_when_process_creation_fails() {
	root := join_path(vtmp_dir(), 'exec_missing_handles_${getpid()}')
	rmdir_all(root) or {}
	mkdir_all(root)!
	defer { rmdir_all(root) or {} }
	missing := join_path(root, 'missing.exe')
	assert !exists(missing)
	warmup := exec([missing])
	assert warmup.exit_code != 0, warmup.output
	assert warmup.output.contains('exec failed (CreateProcess)'), warmup.output
	gc_collect()
	before := exec_test_handle_count()
	for _ in 0 .. 24 {
		result := exec([missing])
		assert result.exit_code == warmup.exit_code, result.output
		assert result.output == warmup.output, result.output
	}
	after := exec_test_handle_count()
	assert after == before, 'failed os.exec leaked handles: before=${before}, after=${after}'
}
