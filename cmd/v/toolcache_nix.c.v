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

@[c_extern]
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

// read_metadata opens a child without following links or blocking on FIFOs. The descriptor
// must name an owned regular file before any bytes are read, and remains pinned during reading.
fn (entry ToolCacheEntryDir) read_metadata(name string) ?string {
	fd := C.openat(entry.fd, &char(name.str), C.O_RDONLY | C.O_NOFOLLOW | C.O_NONBLOCK, 0)
	if fd < 0 {
		return none
	}
	defer {
		C.close(fd)
	}
	mut information := C.stat{}
	if C.fstat(fd, &information) != 0 || u32(information.st_uid) != os.getuid()
		|| information.st_mode & C.S_IFMT != C.S_IFREG {
		return none
	}
	mut chunks := []string{}
	for {
		chunk, count := os.fd_read(fd, 4096)
		if count < 0 {
			return none
		}
		if count == 0 {
			return chunks.join('')
		}
		chunks << chunk
	}
}

// ensure_tool_cache_lock_file creates the persistent inode used to serialize every cache key
// for one tool. It is intentionally never unlinked: releasing a lock while removing its
// pathname can split waiters across two independently locked inodes.
fn ensure_tool_cache_lock_file(path string) ! {
	fd := C.open(&char(path.str), C.O_WRONLY | C.O_CREAT | C.O_EXCL | C.O_NOFOLLOW, 0o600)
	if fd >= 0 {
		C.close(fd)
	}
	information := os.lstat(path) or {
		return error('cannot create the tool cache lock `${path}`')
	}
	if information.get_filetype() != .regular || information.uid != os.getuid() {
		return error('the tool cache lock `${path}` is not a safe file')
	}
	os.chmod(path, 0o600)!
}

fn tool_cache_root_can_stage(path string) bool {
	root := os.real_path(path)
	information := os.stat(root) or { return false }
	if information.uid !in [os.getuid(), u32(0)] {
		return false
	}
	return information.mode & 0o022 == 0 || information.mode & 0o1000 != 0
}

// tool_cache_parents_are_trusted reports whether `path` and every folder above it pass
// `tool_cache_root_can_stage`. If any of them is writable by others (without the sticky bit),
// someone else could rename the folder below it and put their own one in its place.
// The folders are checked both as written and with symlinks resolved: when `~/.cache/v` is a
// symlink, someone who can write to `~/.cache` could point it somewhere else at any time.
fn tool_cache_parents_are_trusted(path string) bool {
	return tool_cache_ancestors_can_stage(os.real_path(path))
		&& tool_cache_ancestors_can_stage(os.abs_path(path))
}

fn tool_cache_ancestors_can_stage(path string) bool {
	mut current := path
	for {
		if !tool_cache_root_can_stage(current) {
			return false
		}
		parent := os.dir(current)
		if parent == current {
			return true
		}
		current = parent
	}
	return true
}

// make_tool_cache_root_private makes sure that only the current user can write to a cache
// folder, so that `tool_cache_root_can_stage` accepts it. It only changes folders that the
// current user owns, and skips shared folders like `/tmp` that use the sticky bit.
fn make_tool_cache_root_private(path string) {
	// Open the folder once, and do both the check and the change through that descriptor.
	// Checking and changing by path would be two separate lookups: in between, someone who
	// can write to the parent folder could swap the folder for a symlink to another file, and
	// the change would then loosen that file's permissions instead. O_NOFOLLOW refuses a
	// symlink, and O_DIRECTORY refuses anything that is not a folder.
	fd := C.open(&char(path.str), C.O_RDONLY | C.O_DIRECTORY | C.O_NOFOLLOW, 0)
	if fd < 0 {
		return
	}
	defer {
		C.close(fd)
	}
	mut information := C.stat{}
	if C.fstat(fd, &information) != 0 {
		return
	}
	mode := u32(information.st_mode)
	// 0o1000 is the sticky bit: in such a shared folder, users can not touch each other's files.
	if u32(information.st_uid) != os.getuid() || mode & 0o1000 != 0 {
		return
	}
	// Masking with 0o7755 removes write access for the group and for others, e.g. 0775 -> 0755.
	C.fchmod(fd, mode & 0o7755)
}

// stage_parent uses the locked, mode-0700 entry itself, keeping compiler output on the cache
// filesystem and inaccessible to other accounts.
fn (entry ToolCacheEntryDir) stage_parent(path string) !string {
	parent := os.real_path(os.dir(path))
	if !tool_cache_root_can_stage(parent) {
		return error('the tool cache directory `${parent}` cannot safely hold staged files')
	}
	return path
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
	for {
		directory_entry := C.readdir(directory)
		if isnil(directory_entry) {
			break
		}
		// readdir owns and may reuse the d_name buffer on its next call, so convert the C
		// pointer and clone its bytes immediately while that borrowed buffer is still valid.
		name := unsafe { tos_clone(&u8(&directory_entry.d_name[0])) }
		if name != '.' && name != '..' {
			names << name
		}
	}
	return names
}

fn (entry ToolCacheEntryDir) remove_child(name string) {
	child_fd := C.openat(entry.fd, &char(name.str), C.O_RDONLY | C.O_DIRECTORY | C.O_NOFOLLOW, 0)
	if child_fd < 0 {
		entry.remove(name)
		return
	}
	child := ToolCacheEntryDir{
		fd: child_fd
	}
	child.remove_all_contents()
	child.close()
	C.unlinkat(entry.fd, &char(name.str), C.AT_REMOVEDIR)
}

fn (entry ToolCacheEntryDir) remove_all_contents() {
	for name in entry.child_names() {
		entry.remove_child(name)
	}
}

fn (entry ToolCacheEntryDir) prune_abandoned_stages() {
	for name in entry.child_names() {
		if name.starts_with(tool_cache_stage_prefix) {
			entry.remove_child(name)
		}
	}
}

fn (entry ToolCacheEntryDir) prune_replaced_binaries() {
	for name in entry.child_names() {
		if name.contains(tool_cache_replaced_marker) {
			entry.remove(name)
		}
	}
}
