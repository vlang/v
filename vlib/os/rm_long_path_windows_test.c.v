import os

fn C.CreateDirectoryW(&u16, voidptr) bool
fn C.CreateFileW(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr
fn C.CloseHandle(voidptr) bool
fn C.SetFileAttributesW(&u16, u32) bool
fn C.GetLastError() u32
fn C.DeviceIoControl(voidptr, u32, voidptr, u32, voidptr, u32, &u32, voidptr) bool

const long_rm_generic_write = u32(0x40000000)
const long_rm_create_always = u32(2)
const long_rm_file_attribute_normal = u32(0x80)
const long_rm_file_attribute_readonly = u32(0x01)
const long_rm_open_existing = u32(3)
const long_rm_file_flag_backup_semantics = u32(0x02000000)
const long_rm_file_flag_open_reparse_point = u32(0x00200000)
const long_rm_fsctl_set_reparse_point = u32(0x000900A4)
const long_rm_io_reparse_tag_mount_point = u32(0xA0000003)

// extended_path_of_test spells `path` with the Win32 extended-length prefix,
// which resolves past MAX_PATH even when long paths are disabled in the OS.
fn extended_path_of_test(path string) string {
	n := path.replace('/', '\\')
	if n.starts_with('\\\\') {
		return '\\\\?\\UNC\\' + n[2..]
	}
	return '\\\\?\\' + n
}

fn mk_extended_dir_of_test(path string) {
	w := extended_path_of_test(path).to_wide()
	defer {
		unsafe { free(w) }
	}
	assert C.CreateDirectoryW(w, unsafe { nil }), 'could not create dir ${path}: ${int(C.GetLastError())}'
}

fn create_extended_file_of_test(path string) {
	w := extended_path_of_test(path).to_wide()
	defer {
		unsafe { free(w) }
	}
	h := C.CreateFileW(w, long_rm_generic_write, 0, unsafe { nil }, long_rm_create_always,
		long_rm_file_attribute_normal, unsafe { nil })
	assert h != voidptr(-1), 'could not create file ${path}: ${int(C.GetLastError())}'
	C.CloseHandle(h)
}

fn set_extended_readonly_of_test(path string) {
	w := extended_path_of_test(path).to_wide()
	defer {
		unsafe { free(w) }
	}
	assert C.SetFileAttributesW(w, long_rm_file_attribute_readonly), 'could not mark ${path} read-only: ${int(C.GetLastError())}'
}

// test_rm_and_rmdir_all_remove_paths_past_max_path covers the `v wipe-cache`
// failure (which also fails `v up`): removing a tool cache folder stopped with
// `could not remove V tool cache folder ...: No such file or directory` on
// staging files reaching past MAX_PATH. The CRT deletion in `os.rm` cannot
// resolve those, while listing them (Win32) still works. `os.rm`, `os.rmdir`,
// `os.ls`, `os.is_dir` and `os.is_link` retry such paths through their
// extended-length form.
fn test_rm_and_rmdir_all_remove_paths_past_max_path() {
	base := os.join_path(os.vtmp_dir(), 'os_long_rm_${os.getpid()}')
	os.mkdir_all(base)!
	defer {
		// Best effort; the assertions below remove everything first.
		os.rmdir_all(base) or {}
	}
	// Grow one level at a time through the extended form, because the plain
	// APIs cannot create entries once the path reaches MAX_PATH while long
	// paths are disabled in the OS.
	mut dir := base
	for _ in 0 .. 5 {
		dir = os.join_path(dir, 'd'.repeat(50))
		mk_extended_dir_of_test(dir)
	}
	assert dir.len > 260, 'test setup did not reach past MAX_PATH: ${dir.len}'
	file := os.join_path(dir, 'f'.repeat(40) + '.txt')
	readonly_file := os.join_path(dir, 'r'.repeat(40) + '.txt')
	assert file.len > 260, 'test setup did not reach past MAX_PATH: ${file.len}'
	create_extended_file_of_test(file)
	create_extended_file_of_test(readonly_file)
	set_extended_readonly_of_test(readonly_file)
	// From here on only plain, unprefixed paths are used, like `rmdir_all`
	// and `v wipe-cache` see them.
	assert os.is_dir(dir)
	assert !os.is_link(file)
	assert !os.is_link(dir)
	assert os.file_name(file) in os.ls(dir)!
	assert os.file_name(readonly_file) in os.ls(dir)!
	os.rm(readonly_file)!
	assert os.file_name(readonly_file) !in os.ls(dir)!
	os.rm(file)!
	assert os.file_name(file) !in os.ls(dir)!
	os.rmdir_all(base)!
	assert !os.is_dir(base)
}

// wide_bytes_of_test returns the UTF-16LE bytes of `s` without its
// terminator, discovered by scanning so non-ASCII paths keep working.
fn wide_bytes_of_test(s string) []u8 {
	w := s.to_wide()
	defer {
		unsafe { free(w) }
	}
	mut units := 0
	unsafe {
		for w[units] != 0 {
			units++
		}
	}
	mut out := []u8{len: units * 2}
	if units > 0 {
		unsafe {
			vmemcpy(voidptr(out.data), voidptr(w), units * 2)
		}
	}
	return out
}

// make_junction_of_test creates a directory junction at `link` pointing at
// `target`, spelling both through the extended-length form, which resolves
// past MAX_PATH even when long paths are disabled in the OS. Unlike
// symlinks, junctions need no special privilege to create.
fn make_junction_of_test(link string, target string) {
	// An empty directory becomes the junction once it carries a mount point.
	mk_extended_dir_of_test(link)
	substitute := '\??\\' + target.replace('/', '\\')
	sub_bytes := wide_bytes_of_test(substitute)
	print_bytes := wide_bytes_of_test(target.replace('/', '\\'))
	mut buf := []u8{len: 16 + sub_bytes.len + 2 + print_bytes.len + 2}
	// IO_REPARSE_TAG_MOUNT_POINT, little-endian.
	buf[0] = 0x03
	buf[3] = 0xA0
	data_len := u16(8 + sub_bytes.len + 2 + print_bytes.len + 2)
	buf[4] = u8(data_len)
	buf[5] = u8(data_len >> 8)
	sub_len := u16(sub_bytes.len)
	buf[10] = u8(sub_len)
	buf[11] = u8(sub_len >> 8)
	print_off := u16(sub_bytes.len + 2)
	buf[12] = u8(print_off)
	buf[13] = u8(print_off >> 8)
	print_len := u16(print_bytes.len)
	buf[14] = u8(print_len)
	buf[15] = u8(print_len >> 8)
	mut at := 16
	for b in sub_bytes {
		buf[at] = b
		at++
	}
	// The substitute-name null terminator is already zeroed; skip it.
	at += 2
	for b in print_bytes {
		buf[at] = b
		at++
	}
	// The print-name null terminator is already zeroed.
	wlink := extended_path_of_test(link).to_wide()
	defer {
		unsafe { free(wlink) }
	}
	handle := C.CreateFileW(wlink, long_rm_generic_write, 0, unsafe { nil }, long_rm_open_existing,
		long_rm_file_flag_backup_semantics | long_rm_file_flag_open_reparse_point, unsafe { nil })
	assert handle != voidptr(-1), 'could not open ${link} for junction: ${int(C.GetLastError())}'
	defer {
		C.CloseHandle(handle)
	}
	mut returned := u32(0)
	mut ok := false
	unsafe {
		ok = C.DeviceIoControl(handle, long_rm_fsctl_set_reparse_point, voidptr(&buf[0]),
			u32(buf.len), nil, 0, &returned, nil)
	}
	assert ok, 'could not set junction ${link}: ${int(C.GetLastError())}'
}

// test_ls_lists_dirs_at_search_pattern_boundaries covers the MAX_PATH edge
// for directory enumeration: FindFirstFileW receives `path` plus `\*`, so a
// 258 or 259-unit directory already exceeds the limit while the directory
// path alone stays below it. Only plain, unprefixed paths are listed, like
// `rmdir_all` sees them.
fn test_ls_lists_dirs_at_search_pattern_boundaries() {
	base := os.join_path(os.vtmp_dir(), 'os_long_ls_${os.getpid()}')
	os.mkdir_all(base)!
	defer {
		os.rmdir_all(base) or {}
	}
	if !base.is_ascii() {
		eprintln('skipping: temp dir is not ASCII, unit lengths would be off')
		return
	}
	for units in [257, 258, 259, 260] {
		dir := os.join_path(base, 'd'.repeat(units - base.len - 1))
		assert dir.len == units, 'test setup length ${dir.len} != ${units}'
		mk_extended_dir_of_test(dir)
		create_extended_file_of_test(os.join_path(dir, 'f.txt'))
		assert os.ls(dir)! == ['f.txt'], 'ls(${units})'
	}
	os.rmdir_all(base)!
	assert !os.is_dir(base)
}

// test_rm_removes_long_directory_link_through_extended_form covers the
// extended-length deletion retry for a directory link past MAX_PATH: the
// plain lookup fails, so only the retry sees the directory/reparse
// attributes, and it must use RemoveDirectoryW (like `os.rm` does) instead
// of DeleteFileW, clearing the read-only attribute first. The target keeps
// its contents.
fn test_rm_removes_long_directory_link_through_extended_form() {
	base := os.join_path(os.vtmp_dir(), 'os_long_link_${os.getpid()}')
	os.mkdir_all(base)!
	defer {
		os.rmdir_all(base) or {}
	}
	if !base.is_ascii() {
		eprintln('skipping: temp dir is not ASCII, unit lengths would be off')
		return
	}
	target := os.join_path(base, 'target')
	os.mkdir(target)!
	os.write_file(os.join_path(target, 'keep.txt'), 'keep')!
	link := os.join_path(base, 'l'.repeat(220))
	assert link.len > 260, 'test setup did not reach past MAX_PATH: ${link.len}'
	make_junction_of_test(link, target)
	assert os.is_link(link)
	// Read-only handling mirrors `os.rm`: the link itself carries it.
	set_extended_readonly_of_test(link)
	// From here on only the plain, unprefixed link path is used.
	os.rm(link)!
	assert !os.exists(link)
	assert os.read_file(os.join_path(target, 'keep.txt'))! == 'keep'
	os.rmdir_all(base)!
	assert !os.is_dir(base)
}
