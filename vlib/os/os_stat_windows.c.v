module os

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

// lstat is the same as stat() for Windows.
@[inline]
pub fn lstat(path string) !Stat {
	return stat(path)
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
