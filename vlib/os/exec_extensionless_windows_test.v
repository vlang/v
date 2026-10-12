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

fn test_process_runs_extensionless_absolute_path_as_exe() {
	dir := os.join_path(os.vtmp_dir(), 'process_extensionless_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	os.cp(os.join_path(os.getenv('SystemRoot'), 'System32', 'where.exe'),
		os.join_path(dir, 'where_copy.exe'))!
	mut process := os.new_process(os.join_path(dir, 'where_copy'))
	process.set_args(['/q', 'cmd'])
	process.run()
	process.wait()
	assert process.code == 0, process.err
	process.close()
}
