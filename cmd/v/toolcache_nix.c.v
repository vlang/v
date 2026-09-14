// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

#include <fcntl.h>

#include <dirent.h>

#include <unistd.h>

fn C.open(const_path &char, flags i32, mode ...int) i32

fn C.close(fd i32) i32

fn C.dup(fd i32) i32

fn C.fdopendir(fd i32) &C.DIR

fn C.readdir(directory &C.DIR) &C.dirent

fn C.closedir(directory &C.DIR) i32

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

fn (entry ToolCacheEntryDir) prune_replaced_binaries() {
	duplicate := C.dup(entry.fd)
	if duplicate < 0 {
		return
	}
	directory := C.fdopendir(duplicate)
	if isnil(directory) {
		C.close(duplicate)
		return
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
