module os

// stat returns a platform-agnostic Stat struct, containing metadata about the given file/folder path.
// It returns a POSIX error, if it can not do so.
// Note: symlinks are followed, and the resulting Stat for their target will be returned.
// If this is not desired, call lstat/1 instead.
pub fn stat(path string) !Stat {
	mut s := C.stat{}
	unsafe {
		res := C.stat(&char(path.str), &s)
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

// lstat is similar to stat/1 for normal files/folders.
// Unlike stat/1, however, it will return the stat info for a symlink, instead of its target.
pub fn lstat(path string) !Stat {
	mut s := C.stat{}
	unsafe {
		res := C.lstat(&char(path.str), &s)
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

// get_filetype returns the FileType from the Stat struct.
pub fn (st Stat) get_filetype() FileType {
	match st.mode & u32(C.S_IFMT) {
		u32(C.S_IFREG) {
			return .regular
		}
		u32(C.S_IFDIR) {
			return .directory
		}
		u32(C.S_IFCHR) {
			return .character_device
		}
		u32(C.S_IFBLK) {
			return .block_device
		}
		u32(C.S_IFIFO) {
			return .fifo
		}
		u32(C.S_IFLNK) {
			return .symbolic_link
		}
		u32(C.S_IFSOCK) {
			return .socket
		}
		else {
			return .unknown
		}
	}
}

// get_mode returns the file type and permissions (readable, writable, executable) in owner/group/others format.
pub fn (st Stat) get_mode() FileMode {
	return FileMode{
		typ:    st.get_filetype()
		owner:  FilePermission{
			read:    (st.mode & u32(C.S_IRUSR)) != 0
			write:   (st.mode & u32(C.S_IWUSR)) != 0
			execute: (st.mode & u32(C.S_IXUSR)) != 0
		}
		group:  FilePermission{
			read:    (st.mode & u32(C.S_IRGRP)) != 0
			write:   (st.mode & u32(C.S_IWGRP)) != 0
			execute: (st.mode & u32(C.S_IXGRP)) != 0
		}
		others: FilePermission{
			read:    (st.mode & u32(C.S_IROTH)) != 0
			write:   (st.mode & u32(C.S_IWOTH)) != 0
			execute: (st.mode & u32(C.S_IXOTH)) != 0
		}
	}
}

// is_dir returns a `bool` indicating whether the given `path` is a directory.
pub fn is_dir(path string) bool {
	attr := stat(path) or { return false }
	return attr.get_filetype() == .directory
}

// is_link returns a boolean indicating whether `path` is a link.
// Warning: `is_link()` is known to cause a TOCTOU vulnerability when used incorrectly
// (for more information: https://github.com/vlang/v/blob/master/vlib/os/README.md).
pub fn is_link(path string) bool {
	attr := lstat(path) or { return false }
	return attr.get_filetype() == .symbolic_link
}

// kind_of_existing_path identifies whether path is a file, directory, or link.
fn kind_of_existing_path(path string) PathKind {
	mut res := PathKind{}
	attr := lstat(path) or { return res }
	res.is_file = attr.get_filetype() == .regular
	res.is_dir = attr.get_filetype() == .directory
	res.is_link = attr.get_filetype() == .symbolic_link
	return res
}
