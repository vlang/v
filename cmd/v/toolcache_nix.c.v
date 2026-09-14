// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

#include <fcntl.h>

#include <dirent.h>

#include <sys/stat.h>

#include <unistd.h>

fn C.open(const_path &char, flags i32, mode ...int) i32

fn C.openat(directory i32, const_path &char, flags i32, mode ...int) i32

fn C.close(fd i32) i32

fn C.dup(fd i32) i32

fn C.fdopendir(fd i32) &C.DIR

fn C.readdir(directory &C.DIR) &C.dirent

fn C.closedir(directory &C.DIR) i32

fn C.fstat(fd i32, information &C.stat) i32

fn C.fchmod(fd i32, mode u32) i32

fn C.renameat(old_directory i32, const_old_path &char, new_directory i32, const_new_path &char) i32

fn C.unlinkat(directory i32, const_path &char, flags i32) i32

// ToolCacheEntryDir pins the directory that receives a compiled tool. All publication and
// cleanup is relative to this descriptor, so replacing its pathname cannot redirect a write.
struct ToolCacheEntryDir {
	fd int
}

fn open_tool_cache_entry_dir(path string) !ToolCacheEntryDir {
	os.mkdir(path, mode: 0o700) or {}
	fd := C.open(&char(path.str), C.O_RDONLY | C.O_DIRECTORY | C.O_NOFOLLOW, 0)
	if fd < 0 {
		if os.is_link(path) {
			return error('the tool cache entry `${path}` is a symbolic link')
		}
		return error('cannot safely open the tool cache entry `${path}`')
	}
	mut information := C.stat{}
	if C.fstat(fd, &information) != 0 || u32(information.st_uid) != os.getuid()
		|| C.fchmod(fd, 0o700) != 0 {
		C.close(fd)
		return error('the tool cache entry `${path}` is not owned by the current user')
	}
	return ToolCacheEntryDir{
		fd: fd
	}
}

fn (entry ToolCacheEntryDir) close() {
	C.close(entry.fd)
}

fn (entry ToolCacheEntryDir) publish(source string, name string) bool {
	return C.renameat(C.AT_FDCWD, &char(source.str), entry.fd, &char(name.str)) == 0
}

fn (entry ToolCacheEntryDir) remove(name string) {
	C.unlinkat(entry.fd, &char(name.str), 0)
}

fn tool_cache_root_can_stage(path string) bool {
	root := os.real_path(path)
	information := os.stat(root) or { return false }
	if information.uid !in [os.getuid(), u32(0)] {
		return false
	}
	return information.mode & 0o022 == 0 || information.mode & 0o1000 != 0
}

// stage_parent returns a location on the entry's filesystem where another account cannot
// rename a mode-0700 child. Sticky shared roots such as /tmp are safe; an ordinary writable
// shared directory is not.
fn (entry ToolCacheEntryDir) stage_parent(path string) !string {
	parent := os.real_path(os.dir(path))
	if !tool_cache_root_can_stage(parent) {
		return error('the tool cache directory `${parent}` cannot safely hold staged files')
	}
	return parent
}

fn (entry ToolCacheEntryDir) child_names() []string {
	mut names := []string{}
	duplicate := C.dup(entry.fd)
	if duplicate < 0 {
		return names
	}
	directory := C.fdopendir(duplicate)
	if isnil(directory) {
		C.close(duplicate)
		return names
	}
	defer {
		C.closedir(directory)
	}
	mut directory_entry := &C.dirent(unsafe { nil })
	for {
		directory_entry = C.readdir(directory)
		if isnil(directory_entry) {
			break
		}
		name := unsafe { tos_clone(&u8(&directory_entry.d_name[0])) }
		if name != '.' && name != '..' {
			names << name
		}
	}
	return names
}

fn (entry ToolCacheEntryDir) remove_all_contents() {
	for name in entry.child_names() {
		child_fd := C.openat(entry.fd, &char(name.str), C.O_RDONLY | C.O_DIRECTORY | C.O_NOFOLLOW, 0)
		if child_fd < 0 {
			entry.remove(name)
			continue
		}
		child := ToolCacheEntryDir{
			fd: child_fd
		}
		child.remove_all_contents()
		child.close()
		C.unlinkat(entry.fd, &char(name.str), C.AT_REMOVEDIR)
	}
}

fn (entry ToolCacheEntryDir) prune_replaced_binaries() {
	for name in entry.child_names() {
		if name.contains(tool_cache_replaced_marker) {
			entry.remove(name)
		}
	}
}

// replace_file_atomically moves `source` onto `destination`, replacing it if it is already
// there. POSIX `rename(2)` is defined to do exactly that, atomically, and a process that is
// currently executing the replaced binary keeps running from its own open image.
fn replace_file_atomically(source string, destination string) bool {
	os.rename(source, destination) or { return false }
	return true
}
