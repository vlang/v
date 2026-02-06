// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
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

// bitmask returns a 3 bit sequence in the order RWE, where the bit is set to 1 if the value is true or 0 otherwise.
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
// It supports Windows for regular files, but it doesn't matter if you use owner, group or others when checking permissions on Windows.
// if a symlink is targeted, it returns info on the link, not the target.
pub fn inode(path string) FileInfo {
	attr := lstat(path) or { Stat{} }
	fm := attr.get_mode()
	return FileInfo{
		typ:    fm.typ
		owner:  fm.owner
		group:  fm.group
		others: fm.others
		size:   attr.size
		mtime:  attr.mtime
	}
}
