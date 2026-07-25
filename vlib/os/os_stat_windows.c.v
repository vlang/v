module os

const windows_file_attribute_reparse_point = u32(0x00000400)
const windows_io_reparse_tag_symlink = u32(0xa000000c)
const windows_filetime_ticks_per_second = u64(10_000_000)
const windows_filetime_unix_epoch_seconds = i64(11_644_473_600)

// stat returns metadata for the given file/folder.
// It will return a POSIX error message, if it can not do so.
// C._wstat64() can be used on 32- and 64-bit Windows per
// https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/stat-functions?view=msvc-170
pub fn stat(path string) !Stat {
	mut s := C.__stat64{}
	unsafe {
		res := C._wstat64(path.to_wide(), &s)
		if res != 0 {
			return error_posix()
		}
		return Stat{
			dev:   s.st_dev
			inode: s.st_ino
			nlink: s.st_nlink
			mode:  s.st_mode
			uid:   s.st_uid
			gid:   s.st_gid
			rdev:  s.st_rdev
			size:  s.st_size
			atime: s.st_atime
			mtime: s.st_mtime
			ctime: s.st_ctime
		}
	}
}

// lstat is the same as stat() for Windows, including reporting symbolic links as regular files.
// Unlike stat(), it can also report a dangling symbolic link.
@[inline]
pub fn lstat(path string) !Stat {
	return stat(path) or {
		if !windows_should_try_dangling_symlink_stat(err.code()) {
			return err
		}
		if link_stat := windows_dangling_symlink_stat(path) {
			return link_stat
		}
		return err
	}
}

fn windows_should_try_dangling_symlink_stat(error_code int) bool {
	return error_code == C.ENOENT
}

fn windows_dangling_symlink_stat(path string) ?Stat {
	normalized_path := path.replace('/', '\\')
	wildcard_start := if normalized_path.starts_with('\\\\?\\') { 4 } else { 0 }
	if normalized_path[wildcard_start..].contains_any('*?') {
		return none
	}
	w_path := normalized_path.to_wide()
	defer {
		// `to_wide` allocates this buffer outside V's managed memory.
		unsafe { free(voidptr(w_path)) }
	}

	mut find_data := Win32finddata{}
	find_handle := C.FindFirstFileW(w_path, voidptr(&find_data))
	if find_handle == C.INVALID_HANDLE_VALUE {
		return none
	}
	defer {
		C.FindClose(find_handle)
	}
	if find_data.dw_file_attributes & windows_file_attribute_reparse_point == 0
		|| find_data.dw_file_attributes & u32(C.FILE_ATTRIBUTE_DIRECTORY) != 0
		|| find_data.dw_reserved0 != windows_io_reparse_tag_symlink {
		return none
	}
	return windows_stat_from_find_data(normalized_path, find_data)
}

fn windows_stat_from_find_data(path string, find_data Win32finddata) Stat {
	mut mode := u32(C.S_IFREG) | u32(C.S_IREAD)
	if find_data.dw_file_attributes & u32(C.FILE_ATTRIBUTE_READONLY) == 0 {
		mode |= u32(C.S_IWRITE)
	}
	match file_ext(path).to_lower() {
		'.exe', '.com', '.bat', '.cmd' { mode |= u32(C.S_IEXEC) }
		else {}
	}
	return Stat{
		nlink: 1
		mode:  mode
		size:  (u64(find_data.n_file_size_high) << 32) | u64(find_data.n_file_size_low)
		atime: windows_filetime_to_unix_seconds(find_data.ft_last_access_time)
		mtime: windows_filetime_to_unix_seconds(find_data.ft_last_write_time)
		ctime: windows_filetime_to_unix_seconds(find_data.ft_creation_time)
	}
}

fn windows_filetime_to_unix_seconds(filetime Filetime) i64 {
	ticks := (u64(filetime.dw_high_date_time) << 32) | u64(filetime.dw_low_date_time)
	if ticks == 0 {
		return 0
	}
	return i64(ticks / windows_filetime_ticks_per_second) - windows_filetime_unix_epoch_seconds
}

// get_filetype returns the FileType from the Stat struct.
pub fn (st Stat) get_filetype() FileType {
	match st.mode & u32(C.S_IFMT) {
		u32(C.S_IFDIR) {
			return .directory
		}
		else {
			return .regular
		}
	}
}

// get_mode returns the file type and permissions (readable, writable, executable) in owner/group/others format.
// Note: they will all be the same for Windows.
pub fn (st Stat) get_mode() FileMode {
	return FileMode{
		typ:    st.get_filetype()
		owner:  FilePermission{
			read:    (st.mode & u32(C.S_IREAD)) != 0
			write:   (st.mode & u32(C.S_IWRITE)) != 0
			execute: (st.mode & u32(C.S_IEXEC)) != 0
		}
		group:  FilePermission{
			read:    (st.mode & u32(C.S_IREAD)) != 0
			write:   (st.mode & u32(C.S_IWRITE)) != 0
			execute: (st.mode & u32(C.S_IEXEC)) != 0
		}
		others: FilePermission{
			read:    (st.mode & u32(C.S_IREAD)) != 0
			write:   (st.mode & u32(C.S_IWRITE)) != 0
			execute: (st.mode & u32(C.S_IEXEC)) != 0
		}
	}
}

// is_dir returns a `bool` indicating whether the given `path` is a directory.
pub fn is_dir(path string) bool {
	w_path := path.replace('/', '\\')
	attr := C.GetFileAttributesW(w_path.to_wide())
	if attr == u32(C.INVALID_FILE_ATTRIBUTES) {
		return false
	}
	if int(attr) & C.FILE_ATTRIBUTE_DIRECTORY != 0 {
		return true
	}
	return false
}

// is_link returns a boolean indicating whether `path` is a link.
// Warning: `is_link()` is known to cause a TOCTOU vulnerability when used incorrectly
// (for more information: https://github.com/vlang/v/blob/master/vlib/os/README.md)
pub fn is_link(path string) bool {
	path_ := path.replace('/', '\\')
	attr := C.GetFileAttributesW(path_.to_wide())
	return int(attr) != int(C.INVALID_FILE_ATTRIBUTES) && (attr & 0x400) != 0
}

// kind_of_existing_path identifies whether path is a file, directory, or link.
fn kind_of_existing_path(path string) PathKind {
	mut res := PathKind{}
	attr := C.GetFileAttributesW(path.to_wide())
	if attr != u32(C.INVALID_FILE_ATTRIBUTES) {
		if (int(attr) & C.FILE_ATTRIBUTE_NORMAL) != 0 {
			res.is_file = true
		}
		if (int(attr) & C.FILE_ATTRIBUTE_DIRECTORY) != 0 {
			res.is_dir = true
		}
		if (int(attr) & 0x400) != 0 {
			res.is_link = true
		}
	}
	return res
}
