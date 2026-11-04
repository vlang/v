import os

fn C.CreateDirectoryW(&u16, voidptr) bool
fn C.CreateFileW(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr
fn C.CloseHandle(voidptr) bool
fn C.SetFileAttributesW(&u16, u32) bool
fn C.GetLastError() u32

const long_rm_generic_write = u32(0x40000000)
const long_rm_create_always = u32(2)
const long_rm_file_attribute_normal = u32(0x80)
const long_rm_file_attribute_readonly = u32(0x01)

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
