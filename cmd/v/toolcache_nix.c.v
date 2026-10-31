// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

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
	return tool_cache_root_problem(path) == ''
}

// tool_cache_root_problem explains why `path` cannot hold the tool cache, or be a folder above
// it, and returns an empty string when it can. Such a folder has to belong to the current user
// or to root, and nobody else may write to it unless it has the sticky bit, which stops them from
// renaming what they do not own.
fn tool_cache_root_problem(path string) string {
	root := os.real_path(path)
	information := os.stat(root) or { return '`${path}` cannot be inspected: ${err.msg()}' }
	label := if root == path { '`${path}`' } else { '`${path}` (`${root}`)' }
	return tool_cache_folder_problem(label, information.mode, information.uid, u32(os.getuid()))
}

// tool_cache_folder_problem describes what is wrong with the folder `label`, given its mode and
// owner, for the user `uid`, or returns an empty string when nothing is.
fn tool_cache_folder_problem(label string, mode u32, owner u32, uid u32) string {
	mut problems := []string{}
	if owner !in [uid, u32(0)] {
		problems << 'is owned by user id ${owner}, not by the current user (id ${uid}) or root'
	}
	if mode & 0o022 != 0 && mode & 0o1000 == 0 {
		writers := match mode & 0o022 {
			0o020 { 'its group' }
			0o002 { 'others' }
			else { 'its group and others' }
		}
		problems << 'is writable by ${writers} without the sticky bit'
	}
	if problems.len == 0 {
		return ''
	}
	return '${label} (mode 0${mode & 0o7777:o}) ${problems.join(', and ')}'
}

// tool_cache_parents_are_trusted reports whether `path` and every folder above it pass
// `tool_cache_root_can_stage`. If any of them is writable by others (without the sticky bit),
// someone else could rename the folder below it and put their own one in its place. Folders
// above everything that `path` shares with the V executable are the one exception, see
// `tool_cache_folder_common_with_compiler`.
// The folders are checked both as written and with symlinks resolved: when `~/.cache/v` is a
// symlink, someone who can write to `~/.cache` could point it somewhere else at any time.
fn tool_cache_parents_are_trusted(path string) bool {
	return tool_cache_ancestors_can_stage(os.real_path(path))
		&& tool_cache_ancestors_can_stage(os.abs_path(path))
}

// tool_cache_ancestors_can_stage walks up from `path`. A folder that does not pass
// `tool_cache_root_can_stage` rejects it, unless the folder is above everything that `path`
// and the V executable have in common, see `tool_cache_folder_common_with_compiler`.
fn tool_cache_ancestors_can_stage(path string) bool {
	common := tool_cache_folder_common_with_compiler(path)
	mut current := path
	for {
		if !tool_cache_root_can_stage(current) && !tool_cache_folder_is_above(current, common) {
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

// tool_cache_folder_common_with_compiler returns the deepest folder that holds both `path` and
// the V executable that the user runs, or an empty string when that executable is unknown.
//
// Someone who can write to a folder above that common folder, or who owns it, can rename the
// entry in it that leads to the common folder, and put their own tree in its place. But that
// entry leads to the V executable as well: they could replace V itself in the same way, and the
// user runs V all the time. So such a folder does not make a cache below it any less safe than
// V already is, and it is tolerated. This keeps the cache working when, say, `/` is writable by
// everybody, while V and the cache are both in the home folder. The common folder itself, and
// any folder below it, is still checked: there, the cache and V are reached through different
// entries, so a change there could swap the cache alone.
fn tool_cache_folder_common_with_compiler(path string) string {
	executable := tool_cache_compiler_executable()
	if executable == '' {
		return ''
	}
	mut common := path
	for !tool_cache_path_is_inside(executable, common) {
		parent := os.dir(common)
		if parent == common {
			return ''
		}
		common = parent
	}
	return common
}

// tool_cache_compiler_executable returns the real path of the V executable that the user ran.
// For the ownership compiler, which runs from the tool cache, that is the one that started it.
fn tool_cache_compiler_executable() string {
	primary := os.getenv('V3_PRIMARY_VEXE')
	executable := os.real_path(if primary != '' { primary } else { os.executable() })
	if !os.is_abs_path(executable) || !os.is_file(executable) {
		return ''
	}
	return executable
}

// tool_cache_path_is_inside reports whether `path` is `folder`, or is somewhere below it.
fn tool_cache_path_is_inside(path string, folder string) bool {
	if path == folder {
		return true
	}
	return path.starts_with(if folder.ends_with('/') { folder } else { folder + '/' })
}

// tool_cache_parent_is_tolerated reports whether `tool_cache_ancestors_can_stage` accepts
// `folder`, a folder above `path`, whatever its permissions are, because it is above everything
// that `path` shares with the V executable. Checked on its own, `folder` is not tolerated: then
// it is itself the folder that it shares with the V executable.
fn tool_cache_parent_is_tolerated(path string, folder string) bool {
	return tool_cache_folder_is_above(folder, tool_cache_folder_common_with_compiler(path))
}

// tool_cache_folder_is_above reports whether `folder` is above `common`, and is not `common`.
fn tool_cache_folder_is_above(folder string, common string) bool {
	return common != '' && folder != common && tool_cache_path_is_inside(common, folder)
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
	problem := tool_cache_root_problem(parent)
	if problem != '' {
		return error('the tool cache directory `${parent}` cannot safely hold staged files: ${problem}')
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
