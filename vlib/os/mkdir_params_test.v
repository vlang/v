import os

fn test_mkdir_all_creates_nested_folders() {
	base := os.join_path(os.vtmp_dir(), 'vcov_mkdir_params_test_${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	nested := os.join_path(base, 'a', 'b', 'c', 'd')
	os.mkdir_all(nested) or {
		assert false, 'mkdir_all(${nested}) failed: ${err}'
	}
	assert os.is_dir(base)
	assert os.is_dir(os.join_path(base, 'a'))
	assert os.is_dir(os.join_path(base, 'a', 'b'))
	assert os.is_dir(nested)
}

fn test_mkdir_all_is_idempotent() {
	base := os.join_path(os.vtmp_dir(), 'vcov_mkdir_params_test_${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	nested := os.join_path(base, 'once', 'twice')
	os.mkdir_all(nested) or { panic(err) }
	os.mkdir_all(nested) or { panic(err) }
	assert os.is_dir(nested)
}

fn test_mkdir_all_accepts_the_alternate_separator() {
	base := os.join_path(os.vtmp_dir(), 'vcov_mkdir_params_test_${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	// build the same path out of both separators and check both land on one folder
	forward := os.join_path(base, 'x', 'y')
	backward := forward.replace(os.path_separator, '/')
	os.mkdir_all(backward) or { panic(err) }
	assert os.is_dir(forward)
}

fn test_mkdir_all_with_an_explicit_mode() {
	base := os.join_path(os.vtmp_dir(), 'vcov_mkdir_params_test_${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	target := os.join_path(base, 'moded')
	os.mkdir_all(target, mode: 0o700) or { panic(err) }
	assert os.is_dir(target)
}

fn test_mkdir_params_default_mode_is_0o777() {
	// the default is documented in MkdirParams; mkdir itself cannot observe it on every platform
	params := os.MkdirParams{}
	assert params.mode == u32(0o777)
	explicit := os.MkdirParams{ mode: 0o755 }
	assert explicit.mode == u32(0o755)
}

fn test_mkdir_all_fails_when_a_file_is_in_the_way() {
	base := os.join_path(os.vtmp_dir(), 'vcov_mkdir_params_test_${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	fpath := os.join_path(base, 'afile')
	os.mkdir_all(base) or { panic(err) }
	os.write_file(fpath, 'in the way') or { panic(err) }
	os.mkdir_all(fpath) or {
		assert err.msg() == 'path `${fpath}` already exists, and is not a folder'
		return
	}
	assert false, 'mkdir_all over an existing file should have failed'
}

fn test_disk_usage_is_self_consistent() {
	du := os.disk_usage(os.temp_dir()) or { panic(err) }
	assert du.total > 0
	assert du.available > 0
	assert du.available <= du.total
	assert du.used <= du.total
	assert du.total == du.available + du.used
}
