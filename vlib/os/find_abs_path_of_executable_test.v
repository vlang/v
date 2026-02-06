import os

const tfolder = os.join_path(os.real_path(os.vtmp_dir()), 'filepath_tests')
const original_path = os.getenv('PATH')

fn testsuite_begin() {
	eprintln('testsuite_begin, tfolder = ${tfolder}')
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder) or { panic(err) }
	os.chdir(tfolder) or { panic(err) }
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup) or {}
	os.rmdir_all(tfolder) or {}
}

fn test_find_abs_path_of_executable() {
	exefolder := os.join_path(tfolder, 'exe')
	os.mkdir(exefolder) or { panic(err) }

	mut myclang_file := os.join_path(exefolder, 'myclang')
	$if windows {
		myclang_file += '.bat'
	}

	mut mylink_file := os.join_path(tfolder, 'mylink')
	$if windows {
		mylink_file += '.bat'
	}

	os.write_file(myclang_file, 'echo hello')!
	os.chmod(myclang_file, 0o0777)!
	dump(os.abs_path(myclang_file))
	dump(os.real_path(myclang_file))
	dump(os.is_executable(myclang_file))

	$if windows {
		eprintln('Windows requires admin privileges in order to create symlinks, related tests will be faked.')
		os.cp(myclang_file, mylink_file) or { panic(err) }
	} $else {
		os.symlink(myclang_file, mylink_file) or { panic(err) }
	}

	dump(os.abs_path(mylink_file))
	dump(os.real_path(mylink_file))
	dump(os.is_executable(mylink_file))

	prepend_to_original_path(exefolder)
	assert find_and_check('myclang')? == myclang_file
	assert find_and_check('mylink') == none

	prepend_to_original_path('.')
	assert find_and_check('mylink')? == mylink_file
	assert find_and_check('myclang') == none

	os.chdir(exefolder) or { panic(err) }
	assert find_and_check('myclang')? == myclang_file
	assert find_and_check('mylink') == none

	prepend_to_original_path(tfolder)
	assert find_and_check('mylink')? == mylink_file
	assert find_and_check('myclang') == none

	restore_original_path()
	os.chdir(os.home_dir())! // Change to a *completely* different folder, just in case the original PATH contains `.`
	assert find_and_check('myclang') == none
	assert find_and_check('mylink') == none
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
