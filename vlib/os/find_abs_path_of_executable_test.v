module os

fn test_find_abs_path_of_executable() ? {
	tfolder := join_path(temp_dir(), 'v', 'tests', 'filepath_test')
	rmdir_all(tfolder) or {}
	assert !is_dir(tfolder)
	mkdir_all(tfolder)?
	defer {
		rmdir_all(tfolder) or {}
	}
	//
	original_path := getenv('PATH')
	original_wdir := getwd()
	defer {
		chdir(original_wdir) or {}
	}
	//
	new_path := tfolder + path_delimiter + original_path
	setenv('PATH', new_path, true)
	//
	mut myclang_file := 'myclang'
	$if windows {
		myclang_file += '.bat'
	}
	//
	chdir(tfolder)?
	write_file(myclang_file, 'echo hello')?
	chmod(myclang_file, 0o0777)?
	dump(real_path(myclang_file))
	dump(is_executable(myclang_file))
	defer {
		rm(myclang_file) or {}
	}
	//
	fpath := find_abs_path_of_executable('myclang') or {
		assert false
		return
	}
	dump(fpath)
	//
	setenv('PATH', original_path, true)
	if x := find_abs_path_of_executable('myclang') {
		eprintln('> find_abs_path_of_executable should have failed, but instead it found: $x')
		assert false
	}
}
