import os

fn test_utime_directory() {
	dir := os.join_path(os.vtmp_dir(), 'utime_directory_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or { panic(err) }
	}
	child := os.join_path(dir, 'child.txt')
	os.write_file(child, 'keep the directory contents')!
	atime := i64(2_147_483_648)
	mtime := i64(2_306_102_494)
	os.utime(dir, atime, mtime)!
	assert os.file_last_mod_unix(dir) == mtime
	$if !windows {
		assert (os.stat(dir)!).atime == atime
	}
	assert os.read_file(child)! == 'keep the directory contents'
	os.utime(dir, atime, 1_704_067_200)!
	assert os.file_last_mod_unix(dir) == 1_704_067_200
}

fn test_utime_unicode_file_and_directory() {
	dir := os.join_path(os.vtmp_dir(), 'utime_世界_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or { panic(err) }
	}
	file := os.join_path(dir, 'étoile.txt')
	os.write_file(file, 'contents')!
	mtime := i64(2_306_102_494)
	os.utime(file, 1_704_067_200, mtime)!
	assert os.file_last_mod_unix(file) == mtime
	assert os.read_file(file)! == 'contents'
	os.utime(dir, 1_704_067_200, mtime)!
	assert os.file_last_mod_unix(dir) == mtime
}

fn test_utime_missing_path() {
	missing := os.join_path(os.vtmp_dir(), 'utime_missing_${os.getpid()}', 'file.txt')
	assert !os.exists(missing)
	if _ := os.utime(missing, 1_704_067_200, 1_704_067_200) {
		assert false, 'utime must not create a missing file'
	} else {
		assert os.is_not_exist(err)
	}
	assert !os.exists(missing)
}
