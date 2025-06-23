import os

const original_path = os.getenv('PATH')

fn test_find_abs_path_of_executable() {
	tfolder := os.join_path(os.vtmp_dir(), 'filepath_tests')
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder)!
	defer {
		os.rmdir_all(tfolder) or {}
	}

	original_wdir := os.getwd()
	defer {
		os.chdir(original_wdir) or {}
	}

	mut myclang_file := os.join_path(tfolder, 'myclang')
	$if windows {
		myclang_file += '.bat'
	}

	os.write_file(myclang_file, 'echo hello')!
	os.chmod(myclang_file, 0o0777)!
	dump(os.real_path(myclang_file))
	dump(os.is_executable(myclang_file))
	defer {
		os.rm(myclang_file) or {}
	}

	prepend_to_original_path(tfolder)
	assert find_and_check('myclang')? == myclang_file

	prepend_to_original_path('.')
	os.chdir(tfolder)!
	assert find_and_check('myclang')? == myclang_file

	restore_original_path()
	os.chdir(os.home_dir())! // Change to a *completely* different folder, just in case the original PATH contains `.`
	assert find_and_check('myclang') == none
}

fn find_and_check(executable string) ?string {
	fpath := os.find_abs_path_of_executable(executable) or { return none }

	dump(fpath)
	assert os.is_abs_path(fpath)

	return fpath
}

fn restore_original_path() {
	os.setenv('PATH', original_path, true)
}

fn prepend_to_original_path(to_prepend string) {
	new_path := to_prepend + os.path_delimiter + original_path
	os.setenv('PATH', new_path, true)
}
