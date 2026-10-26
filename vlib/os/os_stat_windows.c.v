module os

#include <sys/stat.h>

const windows_file_attribute_reparse_point = u32(0x00000400)
const windows_io_reparse_tag_symlink = u32(0xa000000c)
const windows_filetime_ticks_per_second = u64(10_000_000)
const windows_filetime_unix_epoch_seconds = i64(11_644_473_600)

// Windows BY_HANDLE_FILE_INFORMATION layout: thirteen 32-bit words.
struct WindowsFileInformation {
mut:
	file_attributes u32
	creation_time   Filetime
	access_time     Filetime
	write_time      Filetime
	volume_serial   u32
	file_size_high  u32
	file_size_low   u32
	number_of_links u32
	file_index_high u32
	file_index_low  u32
}

fn C.GetFileInformationByHandle(handle voidptr, information voidptr) i32

// stat returns metadata for the given file/folder.
// It will return a POSIX error message, if it can not do so.
// Pre-1970 timestamps are read from the target's Win32 file information.
// C._wstat64() can be used on 32- and 64-bit Windows per
// https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/stat-functions?view=msvc-170
pub fn stat(path string) !Stat {
	mut s := C.__stat64{}
	unsafe {
		res := C._wstat64(path.to_wide(), &s)
		if res != 0 {
			return error_posix()
		}
		mut st := Stat{
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
		// The CRT clamps timestamps before its local epoch to -1. Query a handle
		// to the same target; directory entries would describe a symlink itself.
		if st.atime < 0 || st.mtime < 0 || st.ctime < 0 {
			if information := windows_stat_file_information(path) {
				st.atime = windows_filetime_to_unix_seconds(information.access_time)
				st.mtime = windows_filetime_to_unix_seconds(information.write_time)
				st.ctime = windows_filetime_to_unix_seconds(information.creation_time)
			}
		}
		return st
	}
}

fn windows_stat_file_information(path string) ?WindowsFileInformation {
	normalized_path := path.replace('/', '\\')
	root_len := win_root_len(normalized_path)
	mut path_end := normalized_path.len
	for path_end > root_len + 1 && normalized_path[path_end - 1] == `\\` {
		path_end--
	}
	w_path := normalized_path[..path_end].to_wide()
	defer {
		// to_wide allocates the UTF-16 buffer passed to CreateFileW.
		unsafe { free(w_path) }
	}
	share_mode := C.FILE_SHARE_READ | C.FILE_SHARE_WRITE | C.FILE_SHARE_DELETE
	// Zero access requests metadata only; omitting OPEN_REPARSE_POINT follows links.
	handle := C.CreateFileW(w_path, 0, share_mode, 0, C.OPEN_EXISTING,
		C.FILE_FLAG_BACKUP_SEMANTICS, 0)
	if handle == invalid_handle_value {
		return none
	}
	defer {
		C.CloseHandle(handle)
	}
	mut information := WindowsFileInformation{}
	if C.GetFileInformationByHandle(handle, voidptr(&information)) == 0 {
		return none
	}
	return information
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
	if find_handle == invalid_handle_value {
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
	mut mode := u32(s_ifreg) | u32(s_irusr)
	if find_data.dw_file_attributes & u32(C.FILE_ATTRIBUTE_READONLY) == 0 {
		mode |= u32(s_iwusr)
	}
	match file_ext(path).to_lower() {
		'.exe', '.com', '.bat', '.cmd' { mode |= u32(s_ixusr) }
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
	match st.mode & u32(s_ifmt) {
		u32(s_ifdir) {
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
			read:    (st.mode & u32(s_irusr)) != 0
			write:   (st.mode & u32(s_iwusr)) != 0
			execute: (st.mode & u32(s_ixusr)) != 0
		}
		group:  FilePermission{
			read:    (st.mode & u32(s_irusr)) != 0
			write:   (st.mode & u32(s_iwusr)) != 0
			execute: (st.mode & u32(s_ixusr)) != 0
		}
		others: FilePermission{
			read:    (st.mode & u32(s_irusr)) != 0
			write:   (st.mode & u32(s_iwusr)) != 0
			execute: (st.mode & u32(s_ixusr)) != 0
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
