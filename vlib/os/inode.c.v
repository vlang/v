// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

pub struct FileMode {
pub:
	typ    FileType
	owner  FilePermission
	group  FilePermission
	others FilePermission
}

pub struct FileInfo {
	FileMode
pub:
	size  u64 // size of the file in bytes
	mtime i64 // last modification time in seconds after the Unix epoch
}

pub enum FileType {
	unknown
	regular
	directory
	character_device
	block_device
	fifo
	symbolic_link
	socket
}

pub struct FilePermission {
pub:
	read    bool
	write   bool
	execute bool
}

// bitmask returns a 3 bit sequence in the order RWE where
// the bit is set to 1 if the value is true or 0 otherwise.
pub fn (p FilePermission) bitmask() u32 {
	mut mask := u32(0)
	if p.read {
		mask |= 4
	}
	if p.write {
		mask |= 2
	}
	if p.execute {
		mask |= 1
	}
	return mask
}

// bitmask returns a 9 bit sequence in the order owner + group + others.
// This is a valid bitmask to use with `os.chmod`.
pub fn (m FileMode) bitmask() u32 {
	return m.owner.bitmask() << 6 | m.group.bitmask() << 3 | m.others.bitmask()
}

// inode returns the metadata of the file/inode, containing inode type, permission information, size and modification time.
// it supports windows for regular files, but it doesn't matter if you use owner, group or others when checking permissions on windows.
pub fn inode(path string) FileInfo {
	mut attr := C.stat{}
	$if windows {
		// TODO: replace this with a C.GetFileAttributesW call instead.
		// Use stat, lstat is not available on windows
		unsafe { C.stat(&char(path.str), &attr) }
		mut typ := FileType.regular
		if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFDIR) {
			typ = .directory
		}
		return FileInfo{
			typ: typ
			size: attr.st_size
			mtime: attr.st_mtime
			owner: FilePermission{
				read: (attr.st_mode & u32(C.S_IREAD)) != 0
				write: (attr.st_mode & u32(C.S_IWRITE)) != 0
				execute: (attr.st_mode & u32(C.S_IEXEC)) != 0
			}
			group: FilePermission{
				read: (attr.st_mode & u32(C.S_IREAD)) != 0
				write: (attr.st_mode & u32(C.S_IWRITE)) != 0
				execute: (attr.st_mode & u32(C.S_IEXEC)) != 0
			}
			others: FilePermission{
				read: (attr.st_mode & u32(C.S_IREAD)) != 0
				write: (attr.st_mode & u32(C.S_IWRITE)) != 0
				execute: (attr.st_mode & u32(C.S_IEXEC)) != 0
			}
		}
	} $else {
		// note, that we use lstat here on purpose, to know the information about
		// the potential symlinks themselves, not about the entities they point at
		unsafe { C.lstat(&char(path.str), &attr) }
		mut typ := FileType.unknown
		if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFREG) {
			typ = .regular
		} else if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFDIR) {
			typ = .directory
		} else if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFCHR) {
			typ = .character_device
		} else if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFBLK) {
			typ = .block_device
		} else if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFIFO) {
			typ = .fifo
		} else if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFLNK) {
			typ = .symbolic_link
		} else if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFSOCK) {
			typ = .socket
		}
		return FileInfo{
			typ: typ
			size: attr.st_size
			mtime: attr.st_mtime
			owner: FilePermission{
				read: (attr.st_mode & u32(C.S_IRUSR)) != 0
				write: (attr.st_mode & u32(C.S_IWUSR)) != 0
				execute: (attr.st_mode & u32(C.S_IXUSR)) != 0
			}
			group: FilePermission{
				read: (attr.st_mode & u32(C.S_IRGRP)) != 0
				write: (attr.st_mode & u32(C.S_IWGRP)) != 0
				execute: (attr.st_mode & u32(C.S_IXGRP)) != 0
			}
			others: FilePermission{
				read: (attr.st_mode & u32(C.S_IROTH)) != 0
				write: (attr.st_mode & u32(C.S_IWOTH)) != 0
				execute: (attr.st_mode & u32(C.S_IXOTH)) != 0
			}
		}
	}
}
