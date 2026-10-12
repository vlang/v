import os

// `-o build/prog` writes `build/prog.exe`, and the compiler tests then run `build/prog`
// without the suffix. `os.exec` must find the `.exe` the way a shell would.
fn test_exec_runs_extensionless_absolute_path_as_exe() {
	dir := os.join_path(os.vtmp_dir(), 'exec_extensionless_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.cp(os.join_path(os.getenv('SystemRoot'), 'System32', 'where.exe'), os.join_path(dir, 'where_copy.exe')) or {
		panic(err)
	}
	res := os.exec([os.join_path(dir, 'where_copy'), '/q', 'cmd'])
	assert res.exit_code == 0, res.output
}
