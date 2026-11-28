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

fn C.unlink(const_path &char) i32

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

fn C.lstat(const_path &char, information &C.stat) i32

fn C.fchdir(fd i32) i32

fn C.fchmodat(directory i32, const_path &char, mode u32, flags i32) i32

fn C.fchown(fd i32, owner u32, group u32) i32

// The helper `os.filelock` locks a file with, so that a wipe takes the very lock a build takes.
fn C.v_filelock_lock(fd i32, exclusive i32, immediate i32, start u64, len u64) i32

// ToolCacheWipeDir is a folder that `v wipe-cache` holds open. The wipe lists, inspects, locks and
// removes what is in it through this descriptor only. A build that moves the folder aside and
// puts a new one at its path meanwhile therefore cannot turn the wipe to the new folder: the wipe
// stays on the folder it opened, whose build locks it holds.
struct ToolCacheWipeDir {
	fd int
}

// open_tool_cache_wipe_dir opens the real folder at `path`. A link there is not followed. When
// nothing is at `path`, the error has the code `tool_cache_wipe_gone`.
fn open_tool_cache_wipe_dir(path string) !ToolCacheWipeDir {
	fd := C.open(&char(path.str), C.O_RDONLY | C.O_DIRECTORY | C.O_NOFOLLOW, 0)
	if fd < 0 {
		code := C.errno
		return error_with_code(os.posix_get_error_msg(code), if code == C.ENOENT {
			tool_cache_wipe_gone
		} else {
			0
		})
	}
	return ToolCacheWipeDir{
		fd: fd
	}
}

// close lets the folder go. Locks taken through it stay until they are released themselves.
fn (dir ToolCacheWipeDir) close() {
	C.close(dir.fd)
}

// owner_and_mode returns the id of the user who owns the folder, and its permission bits. A folder
// that cannot be inspected is reported as nobody's and writable by all, so nothing in it is taken
// for the wipe's own.
fn (dir ToolCacheWipeDir) owner_and_mode() (u32, u32) {
	mut information := C.stat{}
	if C.fstat(dir.fd, &information) != 0 {
		return u32(0xffffffff), u32(0o777)
	}
	return u32(information.st_uid), u32(information.st_mode) & 0o7777
}

// is_at reports whether `path` still names this folder, and fails when either of them cannot be
// looked at.
fn (dir ToolCacheWipeDir) is_at(path string) !bool {
	mut information := C.stat{}
	if C.fstat(dir.fd, &information) != 0 {
		return error(os.posix_get_error_msg(C.errno))
	}
	there := os.lstat(path) or {
		if err.code() in [os.error_code_noent, os.error_code_notdir] {
			return false
		}
		return err
	}
	return there.dev == u64(information.st_dev) && there.inode == u64(information.st_ino)
}

// stays_at reports whether this folder, which could not be removed, is still at `path`. It is when
// that cannot be told.
fn (dir ToolCacheWipeDir) stays_at(path string) bool {
	return dir.is_at(path) or { true }
}

// names lists the folder, sorted, without `.` and `..`. A folder that was removed meanwhile, by
// another wipe for example, has nothing in it. A listing that ends with an error fails.
fn (dir ToolCacheWipeDir) names() ![]string {
	// Opened anew through the folder for each listing: a duplicate of `dir.fd` would share its
	// position, which an earlier listing left at the end.
	listing := C.openat(dir.fd, c'.', C.O_RDONLY | C.O_DIRECTORY, 0)
	if listing < 0 {
		if C.errno == C.ENOENT {
			return []string{}
		}
		return error(os.posix_get_error_msg(C.errno))
	}
	directory := C.fdopendir(listing)
	if isnil(directory) {
		code := C.errno
		C.close(listing)
		return error(os.posix_get_error_msg(code))
	}
	defer {
		C.closedir(directory)
	}
	mut names := []string{}
	for {
		// `readdir` returns nothing at the end and when it fails, and only the error code tells which.
		C.errno = 0
		directory_entry := C.readdir(directory)
		if isnil(directory_entry) {
			code := C.errno
			if code != 0 && code != C.ENOENT {
				return error(os.posix_get_error_msg(code))
			}
			break
		}
		// readdir owns and may reuse the d_name buffer on its next call, so clone its bytes now.
		name := unsafe { tos_clone(&u8(&directory_entry.d_name[0])) }
		if name != '.' && name != '..' {
			names << name
		}
	}
	names.sort()
	return names
}

// tool_cache_wipe_item_from turns what `lstat` or `fstat` said about a name into its description.
fn tool_cache_wipe_item_from(information &C.stat) ToolCacheWipeItem {
	kind := match u32(information.st_mode) & 0o170000 {
		0o040000 { ToolCacheWipeKind.folder }
		0o100000 { ToolCacheWipeKind.file }
		0o120000 { ToolCacheWipeKind.link }
		else { ToolCacheWipeKind.other }
	}
	return ToolCacheWipeItem{
		kind:         kind
		owner:        u32(information.st_uid)
		links:        u64(information.st_nlink)
		shut_by_mode: u32(information.st_mode) & 0o600 != 0o600
	}
}

// item describes `name` in the folder. A link is described itself, not what it points to. A name
// that cannot be inspected (no file descriptor left, for example) is reported as `.unknown`, of
// nobody's.
//
// The name is inspected with `lstat` while the open folder is the working folder, which is put
// back at once, through a handle on it. `fstatat` would do without that, but needs
// `AT_SYMLINK_NOFOLLOW`, whose value the headerless compiler bootstrap does not carry.
// `v wipe-cache` runs on one thread, so nothing else sees the working folder change. Without a
// handle on the working folder there is no way back, and the name is not looked at: the wipe works
// from `/`, which gives one (see `tool_cache_wipe_leave_working_folder`), so that happens only when
// no descriptor is left. No other way of looking stands in: opening the name instead says nothing
// of a link, or of a file this user cannot read, and failed a wipe on every one of them.
fn (dir ToolCacheWipeDir) item(name string) ToolCacheWipeItem {
	unknown := ToolCacheWipeItem{
		kind:  .unknown
		owner: u32(0xffffffff)
	}
	mut information := C.stat{}
	back := C.open(c'.', C.O_RDONLY | C.O_DIRECTORY, 0)
	if back < 0 {
		return unknown
	}
	if C.fchdir(dir.fd) != 0 {
		C.close(back)
		return unknown
	}
	failed := C.lstat(&char(name.str), &information) != 0
	code := C.errno
	C.fchdir(back)
	C.close(back)
	if !failed {
		return tool_cache_wipe_item_from(&information)
	}
	return if code == C.ENOENT { ToolCacheWipeItem{} } else { unknown }
}

// tool_cache_wipe_leave_working_folder moves the process to `/` for the wipe, and returns the
// folder to come back to, or '' when that folder has no name (it was removed, or a folder above it
// may not be read). The wipe looks at every name from inside the folder it holds open, and comes
// back through a handle on the working folder (see `item`): `/` always gives one, while the folder
// the wipe was started from may not be readable (`sudo -u someone` started in a folder that is not
// theirs, for example), or may have been removed. Every setting has its one form by then (see
// `tool_cache_wipe_place`), so none needs the working folder any more.
fn tool_cache_wipe_leave_working_folder() string {
	back := os.getwd()
	os.chdir('/') or {}
	return back
}

// tool_cache_wipe_come_back returns to the folder the wipe was started from, when it has a name.
fn tool_cache_wipe_come_back(back string) {
	if back != '' {
		os.chdir(back) or {}
	}
}

// open_folder opens the real folder `name` in the folder. A link there is not followed. When
// nothing has that name, the error has the code `tool_cache_wipe_gone`.
fn (dir ToolCacheWipeDir) open_folder(name string) !ToolCacheWipeDir {
	fd := C.openat(dir.fd, &char(name.str), C.O_RDONLY | C.O_DIRECTORY | C.O_NOFOLLOW, 0)
	if fd < 0 {
		code := C.errno
		return error_with_code(os.posix_get_error_msg(code), if code == C.ENOENT {
			tool_cache_wipe_gone
		} else {
			0
		})
	}
	return ToolCacheWipeDir{
		fd: fd
	}
}

// remove_file removes `name`, which is no folder. A link is removed itself. A name that is gone
// already counts as removed.
fn (dir ToolCacheWipeDir) remove_file(name string) ! {
	if C.unlinkat(dir.fd, &char(name.str), 0) != 0 && C.errno != C.ENOENT {
		return error(os.posix_get_error_msg(C.errno))
	}
}

// tool_cache_unlink removes what is at `path` when it is no folder; a link is removed itself. It never
// removes a folder, unlike `os.rm`, which takes an empty one. Something that is gone already counts as
// removed.
fn tool_cache_unlink(path string) ! {
	if C.unlink(&char(path.str)) != 0 && C.errno != C.ENOENT {
		return error(os.posix_get_error_msg(C.errno))
	}
}

// remove_folder removes the folder `name` when it is empty. A name that is gone already counts as
// removed.
fn (dir ToolCacheWipeDir) remove_folder(name string) ! {
	if C.unlinkat(dir.fd, &char(name.str), C.AT_REMOVEDIR) != 0 && C.errno != C.ENOENT {
		return error(os.posix_get_error_msg(C.errno))
	}
}

// remove_tree removes `name` and, when it is a folder, all that is in it. Every item is attempted,
// and the first failure is returned afterwards, as `os.rmdir_all` does. A link is removed itself,
// at any depth, and never followed.
fn (dir ToolCacheWipeDir) remove_tree(name string) ! {
	child := dir.open_folder(name) or {
		opened := err.msg()
		if dir.item(name).kind == .folder {
			// A real folder that cannot be read can still go when it is empty. When it cannot, why it
			// could not be opened is what the user needs to know, not that it is not empty.
			dir.remove_folder(name) or { return error('${name}: ${opened}') }
			return
		}
		// A file, a link, or whatever else is no folder goes as it is.
		dir.remove_file(name) or { return error('${name}: ${err.msg()}') }
		return
	}
	mut first_error := ''
	inner_names := child.names() or {
		first_error = '${name}: ${err.msg()}'
		[]string{}
	}
	for inner in inner_names {
		child.remove_tree(inner) or {
			if first_error == '' {
				first_error = '${name}/${err.msg()}'
			}
		}
	}
	child.close()
	dir.remove_folder(name) or {
		if first_error == '' {
			first_error = '${name}: ${err.msg()}'
		}
	}
	if first_error != '' {
		return error(first_error)
	}
}

// ToolCacheWipeLock is a build lock that a wipe tried to take.
struct ToolCacheWipeLock {
	state ToolCacheWipeLockState
	// problem says what the system does not let the wipe do with the lock file, for a lock file that
	// cannot be opened and for an attempt that failed.
	problem string
mut:
	fd int = -1
}

// release lets a held lock go.
fn (mut held ToolCacheWipeLock) release() {
	if held.fd >= 0 {
		C.close(held.fd)
		held.fd = -1
	}
}

// tool_cache_refused_lock is what came of a lock that `fcntl` refused with the error `code`. POSIX has
// `EACCES` and `EAGAIN` mean that another process holds a lock on the file, and lets a system use
// either. Any other code says that the lock was not tried to the end, as `ENOLCK` does where the
// system has no locks left or a remote file system's locks fail, and nothing of other processes.
fn tool_cache_refused_lock(code int) ToolCacheWipeLock {
	if code == C.EACCES || code == C.EAGAIN {
		return ToolCacheWipeLock{
			state: .busy
		}
	}
	return ToolCacheWipeLock{
		state:   .failed
		problem: 'cannot be locked: ${os.posix_get_error_msg(code)}'
	}
}

// try_lock takes the build lock `name` in the folder without waiting, with the kind of lock
// `os.filelock` takes for a build. The file is opened through the folder, without following a link
// and without waiting on a FIFO. A process loses such a lock when it closes any descriptor of the
// file, so a caller must not try the same name again while it holds the lock.
fn (dir ToolCacheWipeDir) try_lock(name string) ToolCacheWipeLock {
	fd := C.openat(dir.fd, &char(name.str), C.O_RDWR | C.O_NOFOLLOW | C.O_NONBLOCK, 0)
	if fd < 0 {
		// Read before another call sets it.
		code := C.errno
		if code == C.ENOENT {
			return ToolCacheWipeLock{
				state: .absent
			}
		}
		// What is there decides: a regular file that cannot be opened for writing is a lock file no
		// build can lock either; anything else is not a lock file.
		return ToolCacheWipeLock{
			state:   match dir.item(name).kind {
				.file { ToolCacheWipeLockState.unopenable }
				.gone { ToolCacheWipeLockState.absent }
				.unknown { ToolCacheWipeLockState.failed }
				else { ToolCacheWipeLockState.unsafe_name }
			}
			problem: 'cannot be opened: ${os.posix_get_error_msg(code)}'
		}
	}
	mut information := C.stat{}
	if C.fstat(fd, &information) != 0 {
		code := C.errno
		C.close(fd)
		return ToolCacheWipeLock{
			state:   .failed
			problem: 'cannot be looked at: ${os.posix_get_error_msg(code)}'
		}
	}
	if u32(information.st_mode) & 0o170000 != 0o100000 {
		C.close(fd)
		return ToolCacheWipeLock{
			state: .unsafe_name
		}
	}
	if C.v_filelock_lock(fd, 1, 1, 0, 0) != 0 {
		// Read before the close can set it.
		code := C.errno
		C.close(fd)
		return tool_cache_refused_lock(code)
	}
	return ToolCacheWipeLock{
		state: .held
		fd:    fd
	}
}

// create_lock_file makes the lock file `name` for the user `owner` when nothing has that name, and
// fails with the reason when it does not. A link at that name is not followed. The superuser,
// wiping for another user, hands the file to that user, whose builds accept only a lock file of
// their own (see `ensure_tool_cache_lock_file`).
fn (dir ToolCacheWipeDir) create_lock_file(name string, owner u32) ! {
	fd := C.openat(dir.fd, &char(name.str), C.O_WRONLY | C.O_CREAT | C.O_EXCL | C.O_NOFOLLOW,
		0o600)
	if fd < 0 {
		return error(os.posix_get_error_msg(C.errno))
	}
	mut problem := ''
	if owner != u32(os.getuid()) && C.fchown(fd, owner, u32(0xffffffff)) != 0 {
		// A lock file that the user's builds would reject must not stay.
		problem = 'it could not be handed to user ${owner}: ${os.posix_get_error_msg(C.errno)}'
		C.unlinkat(dir.fd, &char(name.str), 0)
	}
	C.close(fd)
	if problem != '' {
		return error(problem)
	}
}

// repair_lock_file sets the lock file `name` to mode 0600, so that it can be locked again. A file
// that can still be opened for reading or for writing is changed through its own descriptor, once
// that is seen to be a regular file. One that cannot be opened at all is changed by name, which
// follows a link, so the caller checks first that `name` is a regular file that nobody else can
// swap (see `tool_cache_lock_is_repairable`).
fn (dir ToolCacheWipeDir) repair_lock_file(name string) ! {
	// FastC, which builds the compiler too, supports no loop over C constants without a type.
	for access in [i32(C.O_RDONLY), i32(C.O_WRONLY)] {
		fd := C.openat(dir.fd, &char(name.str), access | C.O_NOFOLLOW | C.O_NONBLOCK, 0)
		if fd < 0 {
			continue
		}
		mut information := C.stat{}
		mut problem := ''
		if C.fstat(fd, &information) != 0 {
			problem = os.posix_get_error_msg(C.errno)
		} else if u32(information.st_mode) & 0o170000 != 0o100000 {
			problem = 'it is no regular file'
		} else if C.fchmod(fd, 0o600) != 0 {
			problem = os.posix_get_error_msg(C.errno)
		}
		C.close(fd)
		if problem != '' {
			return error(problem)
		}
		return
	}
	if C.fchmodat(dir.fd, &char(name.str), 0o600, 0) != 0 {
		return error(os.posix_get_error_msg(C.errno))
	}
}
