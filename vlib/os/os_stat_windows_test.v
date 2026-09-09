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
