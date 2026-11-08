module os

// stat returns a platform-agnostic Stat struct, containing metadata about the given file/folder path.
// It returns a POSIX error, if it can not do so.
// Note: symlinks are followed, and the resulting Stat for their target will be returned.
// If this is not desired, call lstat/1 instead.
pub fn stat(path string) !Stat {
	$if freestanding || syscalls {
		stx, e := bf_stat(path, true)
		if e != 0 {
			return error_with_code(fsys_errno_message(e), e)
		}
		return fsys_stat_from_norm(stx)
	}
	$if !freestanding && !syscalls {
		mut s := C.stat{}
		unsafe {
			res := C.stat(&char(path.str), &s)
			if res != 0 {
				return error_posix()
			}
			return Stat{
				dev:   u64(s.st_dev)
				inode: s.st_ino
				nlink: s.st_nlink
				mode:  s.st_mode
				uid:   s.st_uid
				gid:   s.st_gid
				rdev:  u64(s.st_rdev)
				size:  u64(s.st_size)
				atime: s.st_atime
				mtime: s.st_mtime
				ctime: s.st_ctime
			}
		}
	}
}

// lstat is similar to stat/1 for normal files/folders.
// Unlike stat/1, however, it will return the stat info for a symlink, instead of its target.
pub fn lstat(path string) !Stat {
	$if freestanding || syscalls {
		stx, e := bf_stat(path, false)
		if e != 0 {
			return error_with_code(fsys_errno_message(e), e)
		}
		return fsys_stat_from_norm(stx)
	}
	$if !freestanding && !syscalls {
		mut s := C.stat{}
		unsafe {
			res := C.lstat(&char(path.str), &s)
			if res != 0 {
				return error_posix()
			}
			return Stat{
				dev:   u64(s.st_dev)
				inode: s.st_ino
				nlink: s.st_nlink
				mode:  s.st_mode
				uid:   s.st_uid
				gid:   s.st_gid
				rdev:  u64(s.st_rdev)
				size:  u64(s.st_size)
				atime: s.st_atime
				mtime: s.st_mtime
				ctime: s.st_ctime
			}
		}
	}
}

// get_filetype returns the FileType from the Stat struct.
pub fn (st Stat) get_filetype() FileType {
	match st.mode & s_ifmt {
		s_ifreg {
			return .regular
		}
		s_ifdir {
			return .directory
		}
		s_ifchr {
			return .character_device
		}
		s_ifblk {
			return .block_device
		}
		s_ififo {
			return .fifo
		}
		s_iflnk {
			return .symbolic_link
		}
		s_ifsock {
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
			read:    (st.mode & s_irusr) != 0
			write:   (st.mode & s_iwusr) != 0
			execute: (st.mode & s_ixusr) != 0
		}
		group:  FilePermission{
			read:    (st.mode & s_irgrp) != 0
			write:   (st.mode & s_iwgrp) != 0
			execute: (st.mode & s_ixgrp) != 0
		}
		others: FilePermission{
			read:    (st.mode & s_iroth) != 0
			write:   (st.mode & s_iwoth) != 0
			execute: (st.mode & s_ixoth) != 0
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
