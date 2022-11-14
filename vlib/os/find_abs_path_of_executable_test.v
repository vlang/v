import os

fn test_find_abs_path_of_executable() {
	tfolder := os.join_path(os.vtmp_dir(), 'v', 'tests', 'filepath_test')
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder)!
	defer {
		os.rmdir_all(tfolder) or {}
	}
	//
	original_path := os.getenv('PATH')
	original_wdir := os.getwd()
	defer {
		os.chdir(original_wdir) or {}
	}
	//
	new_path := tfolder + os.path_delimiter + original_path
	os.setenv('PATH', new_path, true)
	//
	mut myclang_file := 'myclang'
	$if windows {
		myclang_file += '.bat'
	}
	//
	os.chdir(tfolder)!
	os.write_file(myclang_file, 'echo hello')!
	os.chmod(myclang_file, 0o0777)!
	dump(os.real_path(myclang_file))
	dump(os.is_executable(myclang_file))
	defer {
		os.rm(myclang_file) or {}
	}
	//
	fpath := os.find_abs_path_of_executable('myclang') or {
		assert false
		return
	}
	dump(fpath)
	//
	os.setenv('PATH', original_path, true)
	os.chdir(os.home_dir())! // change to a *completely* different folder, to avoid the original PATH containing `.`
	if x := os.find_abs_path_of_executable('myclang') {
		eprintln('> find_abs_path_of_executable should have failed, but instead it found: ${x}')
		assert false
	}
}
