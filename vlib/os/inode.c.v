// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

pub enum FileType {
	regular
	directory
	character_device
	block_device
	fifo
	symbolic_link
	socket
}

struct FilePermission {
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

struct FileMode {
pub:
	typ    FileType
	owner  FilePermission
	group  FilePermission
	others FilePermission
}

// bitmask returns a 9 bit sequence in the order owner + group + others.
// This is a valid bitmask to use with `os.chmod`.
pub fn (m FileMode) bitmask() u32 {
	return m.owner.bitmask() << 6 | m.group.bitmask() << 3 | m.others.bitmask()
}

// inode returns the mode of the file/inode containing inode type and permission information
// it supports windows for regular files but it doesn't matter if you use owner, group or others when checking permissions on windows
pub fn inode(path string) FileMode {
	mut attr := C.stat{}
	unsafe { C.stat(&char(path.str), &attr) }
	mut typ := FileType.regular
	if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFDIR) {
		typ = .directory
	}
	$if !windows {
		if attr.st_mode & u32(C.S_IFMT) == u32(C.S_IFCHR) {
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
	}
	$if windows {
		return FileMode{
			typ: typ
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
		return FileMode{
			typ: typ
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
