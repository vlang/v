module os

fn C.GetShortPathNameW(&u16, &u16, u32) u32

fn test_get_long_path_expands_existing_short_name() {
	root := join_path(vtmp_dir(), 'long_path_units_${getpid()}')
	mkdir_all(root)!
	defer { rmdir_all(root) or {} }
	path := join_path(root, 'a filename with spaces.txt')
	write_file(path, 'content')!
	wide := path.to_wide()
	defer {
		// to_wide returns an owned allocation for the Win32 input buffer.
		unsafe { free(wide) }
	}
	short_buf := [4096]u16{}
	count := C.GetShortPathNameW(wide, unsafe { &short_buf[0] }, u32(short_buf.len))
	assert count > 0 && count < u32(short_buf.len)
	short_name := unsafe { string_from_wide2(&short_buf[0], int(count)) }
	if !short_name.contains('~') {
		// Filesystems with 8.3-name creation disabled cannot exercise expansion.
		return
	}
	assert get_long_path(short_name)! == path
}
