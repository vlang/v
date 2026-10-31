import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_const_error_tests')

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder)!
	os.chdir(tfolder)!
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

// Pin the values on the platforms that are tested. The tests below check that
// the os.* functions really return them.
fn test_error_code_constants_match_platform() {
	$if windows {
		assert os.error_code_noent == 2 // ERROR_FILE_NOT_FOUND
		assert os.error_code_denied == 5 // ERROR_ACCESS_DENIED
		assert os.error_code_perm == 5 // ERROR_ACCESS_DENIED
		assert os.error_code_exist == 183 // ERROR_ALREADY_EXISTS
		assert os.error_code_notdir == 267 // ERROR_DIRECTORY
	} $else {
		assert os.error_code_noent == 2 // ENOENT
		assert os.error_code_perm == 1 // EPERM
		assert os.error_code_denied == 13 // EACCES
		assert os.error_code_exist == 17 // EEXIST
		assert os.error_code_notdir == 20 // ENOTDIR
	}
}

fn test_error_code_predicates() {
	noent := error_with_code('x', os.error_code_noent)
	assert os.is_not_exist(noent)
	assert !os.is_exist(noent)
	assert !os.is_permission_denied(noent)

	exist := error_with_code('x', os.error_code_exist)
	assert os.is_exist(exist)
	assert !os.is_not_exist(exist)

	// EACCES and EPERM are distinct on POSIX and a single code on Windows, so
	// is_permission_denied has to accept both.
	assert os.is_permission_denied(error_with_code('x', os.error_code_denied))
	assert os.is_permission_denied(error_with_code('x', os.error_code_perm))
	assert !os.is_permission_denied(noent)

	// On Windows, the functions built on the C runtime report C errno values,
	// and the ones built on the Win32 API report Win32 codes.
	$if windows {
		assert os.is_not_exist(error_with_code('x', 3)) // ERROR_PATH_NOT_FOUND
		assert os.is_exist(error_with_code('x', 17)) // EEXIST
		assert os.is_exist(error_with_code('x', 80)) // ERROR_FILE_EXISTS
		assert os.is_permission_denied(error_with_code('x', 1)) // EPERM
		assert os.is_permission_denied(error_with_code('x', 13)) // EACCES
	}
}

// A missing path has to be recognisable without a separate os.exists() call,
// which would be a time-of-check/time-of-use race.
fn test_is_not_exist_on_missing_path() {
	missing := os.join_path_single(tfolder, 'no_such_file_for_error_codes')
	assert !os.exists(missing)
	mut failed := false
	os.stat(missing) or {
		failed = true
		assert err.code() == os.error_code_noent, 'got ${err.code()} for ${err.msg()}'
		assert os.is_not_exist(err)
	}
	assert failed, 'stat of a missing path unexpectedly succeeded'
}

// The parent directory being absent, rather than the leaf, must still report
// error_code_noent. On POSIX both are ENOENT. On Windows the leaf case is
// ERROR_FILE_NOT_FOUND, and a missing component has been measured to report the
// same code, so this is asserted rather than assumed.
fn test_is_not_exist_on_missing_parent() {
	deep := os.join_path_single(os.join_path_single(tfolder, 'no_such_dir'), 'f.txt')
	mut failed := false
	os.stat(deep) or {
		failed = true
		assert err.code() == os.error_code_noent, 'got ${err.code()} for ${err.msg()}'
		assert os.is_not_exist(err)
	}
	assert failed, 'stat under a missing directory unexpectedly succeeded'
}

// The paths below used to return an error with code 0 (or -1 for rename), so no
// error-code comparison could ever match them.
fn test_ls_reports_a_code_for_a_missing_directory() {
	missing := os.join_path_single(tfolder, 'no_such_dir_for_ls')
	mut failed := false
	os.ls(missing) or {
		failed = true
		assert os.is_not_exist(err), 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.ls of a missing directory unexpectedly succeeded'
}

fn test_ls_reports_notdir_code_for_a_file() {
	f := os.join_path_single(tfolder, 'plain_for_ls.txt')
	os.write_file(f, 'x') or { panic(err) }
	mut failed := false
	os.ls(f) or {
		failed = true
		assert err.code() == os.error_code_notdir, 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.ls of a file unexpectedly succeeded'
}

fn test_mkdir_reports_a_code_for_an_existing_directory() {
	existing := os.join_path_single(tfolder, 'already_a_dir')
	os.mkdir_all(existing) or { panic(err) }
	mut failed := false
	os.mkdir(existing) or {
		failed = true
		assert os.is_exist(err), 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.mkdir of an existing directory unexpectedly succeeded'
}

// On Windows, CreateDirectory reports ERROR_PATH_NOT_FOUND, not
// ERROR_FILE_NOT_FOUND, for a missing parent directory.
fn test_mkdir_reports_a_code_for_a_missing_parent() {
	nested := os.join_path(tfolder, 'no_such_parent_for_mkdir', 'x')
	mut failed := false
	os.mkdir(nested) or {
		failed = true
		assert os.is_not_exist(err), 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.mkdir under a missing directory unexpectedly succeeded'
}

// os.rmdir_all used to lose the code because it propagated os.ls()'s code-less
// error.
fn test_rmdir_all_reports_a_code_for_a_missing_directory() {
	missing := os.join_path_single(tfolder, 'no_such_dir_for_rmdir_all')
	mut failed := false
	os.rmdir_all(missing) or {
		failed = true
		assert os.is_not_exist(err), 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.rmdir_all of a missing directory unexpectedly succeeded'
}

fn test_rename_reports_a_code_for_a_missing_source() {
	missing := os.join_path_single(tfolder, 'no_such_file_for_rename')
	dst := os.join_path_single(tfolder, 'rename_target')
	mut failed := false
	os.rename(missing, dst) or {
		failed = true
		assert os.is_not_exist(err), 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.rename of a missing file unexpectedly succeeded'
	failed = false
	os.rename_dir(missing, dst) or {
		failed = true
		assert os.is_not_exist(err), 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.rename_dir of a missing folder unexpectedly succeeded'
}

fn test_open_file_reports_a_code_for_a_missing_file() {
	missing := os.join_path_single(tfolder, 'no_such_file_for_open_file')
	mut failed := false
	os.open_file(missing, 'r') or {
		failed = true
		assert os.is_not_exist(err), 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.open_file of a missing file unexpectedly succeeded'
}

// rmdir on a file fails with ENOTDIR on POSIX and ERROR_DIRECTORY on Windows,
// so it pins error_code_notdir against a real return value rather than a literal.
fn test_rmdir_a_file_reports_notdir_code() {
	f := os.join_path_single(tfolder, 'plain.txt')
	os.write_file(f, 'x') or { panic(err) }
	mut failed := false
	os.rmdir(f) or {
		failed = true
		assert err.code() == os.error_code_notdir, 'got ${err.code()} for ${err.msg()}'
	}
	assert failed, 'os.rmdir of a file unexpectedly succeeded'
}
