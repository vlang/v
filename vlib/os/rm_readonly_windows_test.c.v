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

fn test_rm_removes_a_read_only_directory_symlink_without_touching_its_target() {
	base := os.join_path(os.vtmp_dir(), 'os_rm_read_only_dir_link_${os.getpid()}')
	target := os.join_path(base, 'target')
	link := os.join_path(base, 'link')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	os.mkdir_all(target)!
	os.symlink(target, link) or {
		eprintln('skipping ${@FN}: cannot create a directory symlink: ${err}')
		return
	}
	link_wpath := link.to_wide()
	target_wpath := target.to_wide()
	defer {
		// The test owns the wide paths used by the Windows APIs.
		unsafe {
			free(link_wpath)
			free(target_wpath)
		}
	}
	// SetFileAttributesW changes the link itself, not the directory it points to.
	assert C.SetFileAttributesW(link_wpath, u32(C.FILE_ATTRIBUTE_READONLY | C.FILE_ATTRIBUTE_HIDDEN))
	target_attrs := C.GetFileAttributesW(target_wpath)
	assert target_attrs & u32(C.FILE_ATTRIBUTE_READONLY) == 0
	os.rm(link)!
	assert !os.is_link(link)
	assert os.is_dir(target)
	assert C.GetFileAttributesW(target_wpath) == target_attrs
}

fn test_rm_restores_read_only_directory_symlink_attributes_after_a_sharing_failure() {
	base := os.join_path(os.vtmp_dir(), 'os_rm_locked_dir_link_${os.getpid()}')
	target := os.join_path(base, 'target')
	link := os.join_path(base, 'link')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	os.mkdir_all(target)!
	os.symlink(target, link) or {
		eprintln('skipping ${@FN}: cannot create a directory symlink: ${err}')
		return
	}
	wpath := link.to_wide()
	defer {
		// The test owns the wide path used by the Windows APIs.
		unsafe { free(wpath) }
	}
	assert C.SetFileAttributesW(wpath, u32(C.FILE_ATTRIBUTE_READONLY | C.FILE_ATTRIBUTE_HIDDEN))
	attrs := C.GetFileAttributesW(wpath)
	// Holding the link open without FILE_SHARE_DELETE makes RemoveDirectoryW fail.
	// An attributes-only handle is exempt from share checks, so request GENERIC_READ.
	handle := C.CreateFileW(wpath, u32(C.GENERIC_READ), u32(C.FILE_SHARE_READ | C.FILE_SHARE_WRITE),
		unsafe { nil }, u32(C.OPEN_EXISTING), u32(C.FILE_FLAG_BACKUP_SEMANTICS | C.FILE_FLAG_OPEN_REPARSE_POINT),
		unsafe { nil })
	assert handle != C.INVALID_HANDLE_VALUE
	defer {
		C.CloseHandle(handle)
	}
	os.rm(link) or {
		assert err.code() == 32, err.msg() // ERROR_SHARING_VIOLATION
		assert os.is_link(link)
		assert C.GetFileAttributesW(wpath) == attrs
		return
	}
	assert false, 'removing an open directory symlink should fail'
}
