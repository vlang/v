// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

enum FileType {
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
	read bool
	write bool
	execute bool
}

struct FileMode {
pub:
	typ FileType
	owner FilePermission
	group FilePermission
	others FilePermission
}

// inode returns the mode of the file/inode containing inode type and permission information
// it supports windows for regular files but it doesn't matter if you use owner, group or others when checking permissions on windows
pub fn inode(path string) FileMode {
	mut attr := C.stat{}
	C.stat(path.str, &attr)

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
				read: bool(attr.st_mode & u32(C.S_IREAD))
				write: bool(attr.st_mode & u32(C.S_IWRITE))
				execute: bool(attr.st_mode & u32(C.S_IEXEC))
			}
			group: FilePermission{
				read: bool(attr.st_mode & u32(C.S_IREAD))
				write: bool(attr.st_mode & u32(C.S_IWRITE))
				execute: bool(attr.st_mode & u32(C.S_IEXEC))
			}
			others: FilePermission{
				read: bool(attr.st_mode & u32(C.S_IREAD))
				write: bool(attr.st_mode & u32(C.S_IWRITE))
				execute: bool(attr.st_mode & u32(C.S_IEXEC))
			}
		}
	} $else {
		return FileMode{
			typ: typ
			owner: FilePermission{
				read: bool(attr.st_mode & u32(C.S_IRUSR))
				write: bool(attr.st_mode & u32(C.S_IWUSR))
				execute: bool(attr.st_mode & u32(C.S_IXUSR))
			}
			group: FilePermission{
				read: bool(attr.st_mode & u32(C.S_IRGRP))
				write: bool(attr.st_mode & u32(C.S_IWGRP))
				execute: bool(attr.st_mode & u32(C.S_IXGRP))
			}
			others: FilePermission{
				read: bool(attr.st_mode & u32(C.S_IROTH))
				write: bool(attr.st_mode & u32(C.S_IWOTH))
				execute: bool(attr.st_mode & u32(C.S_IXOTH))
			}
		}
	}
}
