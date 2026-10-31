module os

fn set_negative_stat_times_for_test(path string, seconds i64) ! {
	filetime := windows_filetime_from_unix_seconds(seconds)!
	w_path := path.to_wide()
	defer {
		// to_wide allocates the UTF-16 buffer passed to CreateFileW.
		unsafe { free(w_path) }
	}
	share_mode := C.FILE_SHARE_READ | C.FILE_SHARE_WRITE | C.FILE_SHARE_DELETE
	handle := C.CreateFileW(w_path, C.FILE_WRITE_ATTRIBUTES, share_mode, 0,
		C.OPEN_EXISTING, C.FILE_FLAG_BACKUP_SEMANTICS, 0)
	assert handle != invalid_handle_value, get_error_msg(int(C.GetLastError()))
	defer {
		C.CloseHandle(handle)
	}
	assert C.SetFileTime(handle, voidptr(&filetime), voidptr(&filetime),
		voidptr(&filetime)) != 0, get_error_msg(int(C.GetLastError()))
}

fn test_windows_stat_reports_negative_filetime_values() {
	assert sizeof(WindowsFileInformation) == 52
	root := join_path(vtmp_dir(), 'stat_negative_times_${getpid()}')
	mkdir_all(root)!
	defer {
		rmdir_all(root) or {}
	}
	file := join_path(root, 'file.txt')
	folder := join_path(root, 'folder')
	write_file(file, 'stat times')!
	mkdir(folder)!
	for seconds in [i64(-2_000_000_000), -86_400, -13_000, -12_601, -12_600, -1, 0, 1, 1_700_000_000] {
		for path in [file, folder] {
			set_negative_stat_times_for_test(path, seconds)!
		}
		for path in [file, folder, folder + path_separator] {
			st := stat(path)!
			assert st.atime == seconds, '${path}: ${seconds}: ${st.atime}'
			assert st.mtime == seconds, '${path}: ${seconds}: ${st.mtime}'
			assert st.ctime == seconds, '${path}: ${seconds}: ${st.ctime}'
		}
	}
	if _ := stat(join_path(root, 'missing')) {
		assert false, 'stat must still fail for a missing path'
	} else {
		assert err.code() == int(C.ENOENT)
	}
}

fn test_windows_stat_negative_times_follow_live_symlinks() {
	$if !x64 && !x32 {
		// os.symlink currently supports Windows x64 and x32 only.
		return
	}
	root := join_path(vtmp_dir(), 'stat_negative_symlink_${getpid()}')
	mkdir_all(root)!
	defer {
		rmdir_all(root) or {}
	}
	file := join_path(root, 'target.txt')
	folder := join_path(root, 'target_dir')
	write_file(file, 'target metadata')!
	mkdir(folder)!
	for path in [file, folder] {
		link_path := path + '_link'
		symlink(path, link_path) or {
			if err.code() == 1314 {
				eprintln('skipping symlink timestamp regression: ${err}')
				return
			}
			panic(err)
		}
		set_negative_stat_times_for_test(path, -86_400)!
		target_stat := stat(path)!
		link_stat := stat(link_path)!
		assert link_stat.atime == target_stat.atime
		assert link_stat.mtime == target_stat.mtime
		assert link_stat.ctime == target_stat.ctime
	}
}
