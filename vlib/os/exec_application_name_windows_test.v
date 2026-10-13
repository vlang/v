module os

fn test_win_exec_application_name_preserves_exact_path_before_exe_fallback() {
	root := join_path(vtmp_dir(), 'win_application_name_${getpid()}')
	mkdir_all(root)!
	defer { rmdir_all(root) or {} }
	path := join_path(root, 'program')
	assert win_exec_application_name(path) == path
	write_file(path + '.exe', '')!
	assert win_exec_application_name(path) == path + '.exe'
	write_file(path, '')!
	assert win_exec_application_name(path) == path
	assert win_exec_application_name(path + '.exe') == path + '.exe'
	write_file(path + '.com.exe', '')!
	assert win_exec_application_name(path + '.com') == path + '.com'
}
