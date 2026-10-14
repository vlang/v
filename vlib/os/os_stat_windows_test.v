module os

fn windows_filetime_for_test(ticks u64) Filetime {
	return Filetime{
		dw_low_date_time:  u32(ticks)
		dw_high_date_time: u32(ticks >> 32)
	}
}

fn test_windows_lstat_fallback_error_gate() {
	assert windows_should_try_dangling_symlink_stat(C.ENOENT)
	assert !windows_should_try_dangling_symlink_stat(C.EINVAL)
	assert !windows_should_try_dangling_symlink_stat(C.EACCES)
}

fn test_windows_filetime_to_unix_seconds() {
	epoch_ticks := u64(windows_filetime_unix_epoch_seconds) * windows_filetime_ticks_per_second
	assert windows_filetime_to_unix_seconds(windows_filetime_for_test(0)) == 0
	assert windows_filetime_to_unix_seconds(windows_filetime_for_test(epoch_ticks)) == 0
	assert windows_filetime_to_unix_seconds(windows_filetime_for_test(epoch_ticks - 1)) == -1
	assert windows_filetime_to_unix_seconds(windows_filetime_for_test(epoch_ticks +
		windows_filetime_ticks_per_second)) == 1
}

fn test_windows_stat_from_find_data() {
	epoch_ticks := u64(windows_filetime_unix_epoch_seconds) * windows_filetime_ticks_per_second
	find_data := Win32finddata{
		ft_last_access_time: windows_filetime_for_test(epoch_ticks - 1)
		ft_last_write_time:  windows_filetime_for_test(epoch_ticks)
		ft_creation_time:    windows_filetime_for_test(epoch_ticks +
			2 * windows_filetime_ticks_per_second)
		n_file_size_high:    1
		n_file_size_low:     7
	}
	link_stat := windows_stat_from_find_data(r'C:\tmp\DANGLING.EXE', find_data)

	assert link_stat.get_filetype() == .regular
	assert link_stat.nlink == 1
	assert link_stat.mode & u32(C.S_IREAD) != 0
	assert link_stat.mode & u32(C.S_IWRITE) != 0
	assert link_stat.mode & u32(C.S_IEXEC) != 0
	assert link_stat.size == (u64(1) << 32) | 7
	assert link_stat.atime == -1
	assert link_stat.mtime == 0
	assert link_stat.ctime == 2

	for extension in ['EXE', 'COM', 'BAT', 'CMD'] {
		executable_stat := windows_stat_from_find_data('dangling.${extension}', find_data)
		assert executable_stat.mode & u32(C.S_IEXEC) != 0
	}

	mut read_only_data := find_data
	read_only_data.dw_file_attributes = u32(C.FILE_ATTRIBUTE_READONLY)
	read_only_stat := windows_stat_from_find_data('dangling.TXT', read_only_data)
	assert read_only_stat.mode & u32(C.S_IREAD) != 0
	assert read_only_stat.mode & u32(C.S_IWRITE) == 0
	assert read_only_stat.mode & u32(C.S_IEXEC) == 0
}

fn test_windows_get_mode_maps_stat_bits_to_file_mode() {
	epoch_ticks := u64(windows_filetime_unix_epoch_seconds) * windows_filetime_ticks_per_second
	find_data := Win32finddata{
		ft_last_write_time: windows_filetime_for_test(epoch_ticks)
	}
	writable := windows_stat_from_find_data('a.TXT', find_data)

	// Windows reports the same permissions for owner, group and others, and a
	// plain file is neither readable-as-executable nor a directory.
	filemode := writable.get_mode()
	assert filemode.typ == .regular
	assert filemode.owner.read
	assert filemode.owner.write
	assert !filemode.owner.execute
	assert filemode.group.read
	assert filemode.group.write
	assert !filemode.group.execute
	assert filemode.others.read
	assert filemode.others.write
	assert !filemode.others.execute

	// The read-only attribute clears the write bit for all three groups.
	mut read_only_data := find_data
	read_only_data.dw_file_attributes = u32(C.FILE_ATTRIBUTE_READONLY)
	read_only_mode := windows_stat_from_find_data('b.TXT', read_only_data).get_mode()
	assert read_only_mode.owner.read
	assert !read_only_mode.owner.write
	assert !read_only_mode.group.write
	assert !read_only_mode.others.write

	// An executable extension sets the execute bit for all three groups.
	executable_mode := windows_stat_from_find_data('c.EXE', find_data).get_mode()
	assert executable_mode.owner.execute
	assert executable_mode.group.execute
	assert executable_mode.others.execute
}

fn test_windows_stat_reports_directory_and_file_types() {
	dir_path := join_path(temp_dir(), 'v_os_stat_get_mode_dir_${getpid()}')
	if !exists(dir_path) {
		mkdir_all(dir_path)!
	}
	defer {
		rmdir_all(dir_path) or {}
	}
	file_path := join_path(dir_path, 'regular.txt')
	write_file(file_path, 'get_filetype and get_mode')!

	dir_stat := stat(dir_path)!
	assert dir_stat.get_filetype() == .directory
	assert dir_stat.get_mode().typ == .directory
	assert dir_stat.get_mode().owner.read
	assert dir_stat.get_mode().owner.write

	file_stat := stat(file_path)!
	assert file_stat.get_filetype() == .regular
	assert file_stat.get_mode().typ == .regular
	assert file_stat.get_mode().owner.read
	assert file_stat.get_mode().owner.write
	assert !file_stat.get_mode().owner.execute
}

fn test_short_path_empty_string_is_empty() {
	assert short_path('') == ''
}

fn test_short_path_accepts_forward_slashes() {
	// Create an owned fixture so concurrent runs cannot remove each other's files.
	dir := join_path(vtmp_dir(), 'short_path_${getpid()}')
	mkdir(dir)!
	defer {
		rmdir(dir) or {}
	}
	backslashed := short_path(dir)
	assert backslashed != ''
	forward_slashed := dir.replace('\\', '/')
	assert forward_slashed.contains('/')
	// If short names are unavailable, short_path preserves the original input.
	assert short_path(forward_slashed).replace('/', '\\') == backslashed
	assert backslashed.contains('\\')
	assert !backslashed.contains('/')
}
