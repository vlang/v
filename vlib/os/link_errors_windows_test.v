module os

fn test_windows_link_failure_keeps_error_code() {
	root := join_path(vtmp_dir(), 'windows_link_errors_${getpid()}')
	mkdir_all(root)!
	defer {
		rmdir_all(root) or {}
	}
	origin := join_path(root, 'origin.txt')
	target := join_path(root, 'target.txt')
	write_file(origin, 'origin')!
	write_file(target, 'target')!
	mut failed := false
	link(origin, target) or {
		failed = true
		assert is_exist(err), 'got ${err.code()}: ${err.msg()}'
	}
	assert failed
	assert read_file(target)! == 'target'

	failed = false
	link(join_path(root, 'missing.txt'), join_path(root, 'new.txt')) or {
		failed = true
		assert is_not_exist(err), 'got ${err.code()}: ${err.msg()}'
	}
	assert failed
}

fn test_windows_symlink_failure_keeps_error_code() {
	root := join_path(vtmp_dir(), 'windows_symlink_errors_${getpid()}')
	mkdir_all(root)!
	defer {
		rmdir_all(root) or {}
	}
	origin := join_path(root, 'origin.txt')
	target := join_path(root, 'target.txt')
	write_file(origin, 'origin')!
	write_file(target, 'target')!
	mut failed := false
	symlink(origin, target) or {
		// Older Windows versions or accounts without symlink privileges cannot
		// reach the duplicate-target check. Preserve their actual error too.
		if err.code() == 1314 || err.code() == 87 {
			eprintln('skipping symlink classification: ${err}')
			return
		}
		failed = true
		assert is_exist(err), 'got ${err.code()}: ${err.msg()}'
	}
	assert failed
	assert read_file(target)! == 'target'

	failed = false
	symlink(origin, join_path(root, 'missing', 'link.txt')) or {
		failed = true
		assert is_not_exist(err), 'got ${err.code()}: ${err.msg()}'
	}
	assert failed
}

fn test_windows_long_path_failure_keeps_error_code() {
	root := join_path(vtmp_dir(), 'windows_long_path_errors_${getpid()}')
	mkdir_all(root)!
	defer {
		rmdir_all(root) or {}
	}
	mut failed := false
	get_long_path(join_path(root, 'MISSIN~1.TXT')) or {
		failed = true
		assert is_not_exist(err), 'got ${err.code()}: ${err.msg()}'
	}
	assert failed
}
