import os

fn test_rm_restores_read_only_file_attributes_after_a_sharing_failure() {
	target := os.join_path(os.vtmp_dir(), 'os_rm_locked_attributes_${os.getpid()}.txt')
	os.write_file(target, 'kept')!
	defer {
		os.rm(target) or {}
	}
	wpath := target.to_wide()
	defer {
		// The test owns the wide path used by the Windows APIs.
		unsafe { free(wpath) }
	}
	attrs := u32(C.FILE_ATTRIBUTE_READONLY | C.FILE_ATTRIBUTE_HIDDEN | C.FILE_ATTRIBUTE_ARCHIVE)
	assert C.SetFileAttributesW(wpath, attrs)
	mut file := os.open(target)!
	defer {
		file.close()
	}
	os.rm(target) or {
		assert os.is_permission_denied(err), err.msg()
		assert C.GetFileAttributesW(wpath) == attrs
		return
	}
	assert false, 'removing an open file should fail'
}

fn test_rm_does_not_change_directory_attributes() {
	target := os.join_path(os.vtmp_dir(), 'os_rm_directory_attributes_${os.getpid()}')
	os.mkdir_all(target)!
	defer {
		os.rmdir(target) or {}
	}
	wpath := target.to_wide()
	defer {
		// The test owns the wide path used by the Windows APIs.
		unsafe { free(wpath) }
	}
	assert C.SetFileAttributesW(wpath, u32(C.FILE_ATTRIBUTE_READONLY | C.FILE_ATTRIBUTE_HIDDEN))
	attrs := C.GetFileAttributesW(wpath)
	os.rm(target) or {
		assert os.is_permission_denied(err), err.msg()
		assert C.GetFileAttributesW(wpath) == attrs
		return
	}
	assert false, 'os.rm should refuse a directory'
}
