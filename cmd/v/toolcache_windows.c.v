// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import v.util.winpath

#flag windows -l advapi32

#flag @VMODROOT/cmd/v/toolcache_windows_helpers.o

fn C.v_toolcache_move_file_ex_w(const_existing &u16, const_new &u16, flags u32) int

fn C.v_toolcache_create_file_w(const_path &u16, desired_access u32, share_mode u32,
	creation_disposition u32, flags_and_attributes u32) voidptr

fn C.v_toolcache_get_file_information(handle voidptr, information voidptr) int

fn C.v_toolcache_close_handle(handle voidptr) int

fn C.v_toolcache_file_is_disk(handle voidptr) int

fn C.v_toolcache_read_file(handle voidptr, buffer voidptr, size u32, read &u32) int

fn C.v_toolcache_root_is_private(const_path &u16) int

fn C.v_toolcache_listed_name(const_path &u16, const_want &u16, name &u16, size int) int

// The helpers `os.filelock` locks a file with, so that a wipe takes the very lock a build takes.
fn C.v_filelock_lock(voidptr, int, int, u64, u64) int

fn C.v_filelock_unlock(voidptr, u64, u64) int

struct WindowsToolCacheFileInformation {
	file_attributes       u32
	creation_time_low     u32
	creation_time_high    u32
	last_access_time_low  u32
	last_access_time_high u32
	last_write_time_low   u32
	last_write_time_high  u32
	volume_serial_number  u32
	file_size_high        u32
	file_size_low         u32
	number_of_links       u32
	file_index_high       u32
	file_index_low        u32
}

// MOVEFILE_REPLACE_EXISTING, the flag that makes `MoveFileExW` overwrite the destination
// instead of failing. `os.rename` cannot express this: on Windows it is `_wrename`, which
// fails outright whenever the destination already exists.
const movefile_replace_existing = u32(0x00000001)
const toolcache_windows_file_share_all = u32(0x00000001 | 0x00000002 | 0x00000004)
const toolcache_windows_file_share_read_write = u32(0x00000001 | 0x00000002)
const toolcache_windows_generic_read = u32(0x80000000)
const toolcache_windows_generic_write = u32(0x40000000)
const toolcache_windows_create_new = u32(1)
const toolcache_windows_open_existing = u32(3)
const toolcache_windows_file_attribute_normal = u32(0x00000080)
const toolcache_windows_file_attribute_directory = u32(0x00000010)
const toolcache_windows_file_attribute_reparse_point = u32(0x00000400)
const toolcache_windows_file_flag_open_reparse_point = u32(0x00200000)
const toolcache_windows_file_flag_backup_semantics = u32(0x02000000)

// ToolCacheEntryDir pins the directory that receives a compiled tool. Omitting
// FILE_SHARE_DELETE on a read handle keeps the pathname bound until the build is over;
// FILE_FLAG_OPEN_REPARSE_POINT makes the attributes below describe the link itself.
struct ToolCacheEntryDir {
	path   string
	handle voidptr
}

fn open_tool_cache_entry_dir(path string) !ToolCacheEntryDir {
	os.mkdir(path, mode: 0o700) or {}
	w_path := path.replace('/', '\\').to_wide()
	// to_wide allocates outside V's managed heap, and CreateFileW borrows that buffer, so
	// keep it alive through the final API use and release it explicitly afterwards.
	defer {
		unsafe { free(voidptr(w_path)) }
	}
	// A zero-access metadata handle does not participate in Windows share checks.
	// Request directory read access so omitting FILE_SHARE_DELETE prevents renames.
	handle := C.v_toolcache_create_file_w(w_path, toolcache_windows_generic_read, toolcache_windows_file_share_read_write, toolcache_windows_open_existing, toolcache_windows_file_flag_backup_semantics | toolcache_windows_file_flag_open_reparse_point)
	if handle == voidptr(-1) {
		return error('cannot safely open the tool cache entry `${path}`')
	}
	mut information := WindowsToolCacheFileInformation{}
	if C.v_toolcache_get_file_information(handle, voidptr(&information)) == 0
		|| information.file_attributes & toolcache_windows_file_attribute_directory == 0
		|| information.file_attributes & toolcache_windows_file_attribute_reparse_point != 0 {
		C.v_toolcache_close_handle(handle)
		if os.is_link(path) {
			return error('the tool cache entry `${path}` is a symbolic link')
		}
		return error('the tool cache entry `${path}` is not a safe directory')
	}
	return ToolCacheEntryDir{
		path:   path
		handle: handle
	}
}

fn (entry ToolCacheEntryDir) close() {
	C.v_toolcache_close_handle(entry.handle)
}

fn (entry ToolCacheEntryDir) publish(source string, name string) bool {
	return publish_atomically(source, os.join_path(entry.path, name))
}

fn (entry ToolCacheEntryDir) remove(name string) {
	os.rm(os.join_path(entry.path, name)) or {}
}

// read_metadata pins the child file, rejects reparse points and directories, and reads from
// that handle. The entry's handle prevents its parent pathname from being replaced.
fn (entry ToolCacheEntryDir) read_metadata(name string) ?string {
	w_path := os.join_path(entry.path, name).replace('/', '\\').to_wide()
	defer {
		unsafe { free(voidptr(w_path)) }
	}
	handle := C.v_toolcache_create_file_w(w_path, u32(0x80000000), toolcache_windows_file_share_read_write,
		toolcache_windows_open_existing, toolcache_windows_file_attribute_normal | toolcache_windows_file_flag_open_reparse_point)
	if handle == voidptr(-1) {
		return none
	}
	defer {
		C.v_toolcache_close_handle(handle)
	}
	mut information := WindowsToolCacheFileInformation{}
	if C.v_toolcache_get_file_information(handle, voidptr(&information)) == 0
		|| information.file_attributes & toolcache_windows_file_attribute_directory != 0
		|| information.file_attributes & toolcache_windows_file_attribute_reparse_point != 0
		|| C.v_toolcache_file_is_disk(handle) == 0 {
		return none
	}
	mut chunks := []string{}
	mut buffer := []u8{len: 4096}
	for {
		mut count := u32(0)
		if C.v_toolcache_read_file(handle, buffer.data, u32(buffer.len), &count) == 0 {
			return none
		}
		if count == 0 {
			return chunks.join('')
		}
		chunks << buffer[..int(count)].bytestr()
	}
}

// ensure_tool_cache_lock_file creates the persistent file used to serialize every cache key
// for one tool. It is never deleted, so a new owner cannot lock a different file while a
// previous waiter still holds the old file open.
fn ensure_tool_cache_lock_file(path string) ! {
	w_path := path.replace('/', '\\').to_wide()
	// to_wide allocates outside V's managed heap, so release its buffer after the final
	// CreateFileW call rather than retaining it with the persistent lock pathname.
	defer {
		unsafe { free(voidptr(w_path)) }
	}
	desired_access := u32(0x80000000) | u32(0x40000000)
	created := C.v_toolcache_create_file_w(w_path, desired_access, toolcache_windows_file_share_all, toolcache_windows_create_new, toolcache_windows_file_attribute_normal)
	if created != voidptr(-1) {
		C.v_toolcache_close_handle(created)
		return
	}
	handle := C.v_toolcache_create_file_w(w_path, desired_access, toolcache_windows_file_share_all, toolcache_windows_open_existing, toolcache_windows_file_attribute_normal | toolcache_windows_file_flag_open_reparse_point)
	if handle == voidptr(-1) {
		return error('cannot open the tool cache lock `${path}`')
	}
	defer {
		C.v_toolcache_close_handle(handle)
	}
	mut information := WindowsToolCacheFileInformation{}
	if C.v_toolcache_get_file_information(handle, voidptr(&information)) == 0
		|| information.file_attributes & toolcache_windows_file_attribute_directory != 0
		|| information.file_attributes & toolcache_windows_file_attribute_reparse_point != 0 {
		return error('the tool cache lock `${path}` is not a safe file')
	}
}

fn tool_cache_root_can_stage(path string) bool {
	w_path := path.replace('/', '\\').to_wide()
	// to_wide owns an unmanaged buffer that the ACL query borrows until it returns.
	defer {
		unsafe { free(voidptr(w_path)) }
	}
	return C.v_toolcache_root_is_private(w_path) != 0
}

// tool_cache_root_problem explains why `path` cannot hold the tool cache, and returns an empty
// string when it can.
fn tool_cache_root_problem(path string) string {
	if tool_cache_root_can_stage(path) {
		return ''
	}
	return '`${path}` is not private: it is not owned by the current user, or its access control list lets other accounts change it'
}

// tool_cache_parents_are_trusted always accepts on Windows. Folders like `C:\Users` are not
// owned by the current user, so the ACL check used for the cache folder would reject them.
fn tool_cache_parents_are_trusted(_ string) bool {
	return true
}

// tool_cache_parent_is_tolerated is never needed on Windows, where the folders above the cache
// are not checked.
fn tool_cache_parent_is_tolerated(_ string, _ string) bool {
	return false
}

// make_tool_cache_root_private does nothing on Windows. There, folder access is controlled by
// ACLs (per-user permission lists), not by Unix permission bits.
fn make_tool_cache_root_private(_ string) {}

fn (entry ToolCacheEntryDir) stage_parent(_ string) !string {
	// The entry handle was opened without FILE_SHARE_DELETE, so a staging child created
	// underneath this path cannot be redirected by replacing the entry directory.
	return entry.path
}

fn (entry ToolCacheEntryDir) remove_child(name string) {
	child_path := os.join_path(entry.path, name)
	child := open_tool_cache_entry_dir(child_path) or {
		// Removing a file or reparse point does not traverse its target. If it changed
		// into a directory after the open failed, both non-recursive removals are safe.
		os.rm(child_path) or {}
		os.rmdir(child_path) or {}
		return
	}
	child.remove_all_contents()
	child.close()
	os.rmdir(child_path) or {}
}

fn (entry ToolCacheEntryDir) remove_all_contents() {
	for name in os.ls(entry.path) or { [] } {
		entry.remove_child(name)
	}
}

fn (entry ToolCacheEntryDir) prune_abandoned_stages() {
	for name in os.ls(entry.path) or { [] } {
		if name.starts_with(tool_cache_stage_prefix) {
			entry.remove_child(name)
		}
	}
}

fn (entry ToolCacheEntryDir) prune_replaced_binaries() {
	for name in os.ls(entry.path) or { [] } {
		if name.contains(tool_cache_replaced_marker) {
			entry.remove(name)
		}
	}
}

// windows_binary_file_identity returns the stable identity Windows assigns to an open file.
// Unlike the CRT inode, the volume serial and 64-bit file index distinguish concurrently
// published binaries. The creation time also guards against a later reuse of the same index.
fn windows_binary_file_identity(path string) ?string {
	w_path := path.replace('/', '\\').to_wide()
	// `to_wide` allocates an unmanaged buffer. `CreateFileW` borrows it until it returns,
	// so keep the buffer alive through that call and free it afterward.
	defer {
		unsafe { free(voidptr(w_path)) }
	}
	handle := C.v_toolcache_create_file_w(w_path, 0, toolcache_windows_file_share_all, toolcache_windows_open_existing, toolcache_windows_file_attribute_normal)
	if handle == voidptr(-1) {
		return none
	}
	defer {
		C.v_toolcache_close_handle(handle)
	}
	mut information := WindowsToolCacheFileInformation{}
	if C.v_toolcache_get_file_information(handle, voidptr(&information)) == 0 {
		return none
	}
	index := (u64(information.file_index_high) << 32) | u64(information.file_index_low)
	if information.volume_serial_number == 0 || index == 0 {
		return none
	}
	creation := (u64(information.creation_time_high) << 32) | u64(information.creation_time_low)
	size := (u64(information.file_size_high) << 32) | u64(information.file_size_low)
	return '${information.volume_serial_number}:${index}:${creation}:${size}'
}

// publish_atomically moves `staged` over `destination` in a single step, so that a
// concurrently running V process either sees the previous file or the new one, but never
// a half written one, and never has the executable it is starting truncated underneath it.
// `destination` regularly already exists: a tool whose own sources and compiler are
// unchanged keeps its cache key, so a rebuild triggered by an imported vlib module lands
// in the very same slot. Replacing it is what `replace_file_atomically` is for; a plain
// `os.rename` would fail there on Windows.
fn publish_atomically(staged string, destination string) bool {
	if !replace_file_atomically(staged, destination) {
		os.rm(staged) or {}
		return false
	}
	return true
}

// replace_file_atomically moves `source` onto `destination`, replacing it if it is already
// there. A tool that only imports a vlib module which changed keeps its cache key, so a
// rebuild lands on the very same destination and has to be able to overwrite it.
fn replace_file_atomically(source string, destination string) bool {
	if move_file_replacing(source, destination) {
		return true
	}
	// Windows refuses to delete an executable while a process is still running it, so the
	// replacement above fails whenever another `v` is using the cached tool. Renaming the
	// old binary out of the way *is* permitted in that state, and is what makes a
	// self-replacing cache possible at all. The displaced file stays locked until that
	// process exits, so it is left for `prune_stale_tool_binaries_locked` to collect later.
	displaced := '${destination}${tool_cache_replaced_marker}${os.getpid()}'
	os.rm(displaced) or {}
	os.rename(destination, displaced) or { return false }
	if move_file_replacing(source, destination) {
		os.rm(displaced) or {}
		return true
	}
	// Nothing was installed, so put the previous binary back rather than leaving the
	// cache slot empty.
	os.rename(displaced, destination) or {}
	return false
}

fn move_file_replacing(source string, destination string) bool {
	w_source := source.replace('/', '\\').to_wide()
	w_destination := destination.replace('/', '\\').to_wide()
	// to_wide owns both unmanaged buffers, and MoveFileExW only borrows them for this call.
	defer {
		unsafe {
			free(voidptr(w_source))
			free(voidptr(w_destination))
		}
	}
	return C.v_toolcache_move_file_ex_w(w_source, w_destination, movefile_replace_existing) != 0
}

// ToolCacheWipeDir is a folder that `v wipe-cache` works in. No build moves a tool cache folder
// aside on Windows (see `prepare_default_tool_cache_root`), so the folder is known by its path
// there, and privacy is decided by its ACL, which the tool cache checks when it picks a folder.
struct ToolCacheWipeDir {
	path string
mut:
	// before_acting is for tests: `remove_tree` calls it with the path of a folder it is about to
	// remove, once it has emptied it, and the step 'rmdir'.
	before_acting fn (path string, step string) = unsafe { nil }
}

// open_tool_cache_wipe_dir takes the real folder at `path`. A link there is not followed. When
// nothing is at `path`, the error has the code `tool_cache_wipe_gone`.
fn open_tool_cache_wipe_dir(path string) !ToolCacheWipeDir {
	if os.is_dir(path) && !os.is_link(path) {
		return ToolCacheWipeDir{
			path: path
		}
	}
	// Whether it is gone is asked last: another wipe can remove the folder between two looks, and
	// a folder that went after the look above is gone, not something else.
	if !os.exists(path) && !os.is_link(path) {
		return error_with_code('it is gone', tool_cache_wipe_gone)
	}
	return error('it is not a folder')
}

// close lets the folder go.
fn (dir ToolCacheWipeDir) close() {}

// owner_and_mode reports the folder as this user's own, and private.
fn (dir ToolCacheWipeDir) owner_and_mode() (u32, u32) {
	return u32(os.getuid()), u32(0o700)
}

// is_at reports whether `path` still names this folder, which it always does on Windows.
fn (dir ToolCacheWipeDir) is_at(_ string) !bool {
	return true
}

// stays_at reports whether something is still at `path`, the path of this folder, which could not be
// removed.
fn (dir ToolCacheWipeDir) stays_at(path string) bool {
	return os.exists(path) || os.is_link(path)
}

// names lists the folder, sorted. A folder that was removed meanwhile, by another wipe for
// example, has nothing in it.
fn (dir ToolCacheWipeDir) names() ![]string {
	mut names := os.ls(dir.path) or {
		if !os.exists(dir.path) && !os.is_link(dir.path) {
			return []string{}
		}
		return err
	}
	names.sort()
	return names
}

// item describes `name` in the folder. A link is described itself, not what it points to. A name
// that no path reaches (see `tool_cache_wipe_unreachable`) cannot be looked at.
fn (dir ToolCacheWipeDir) item(name string) ToolCacheWipeItem {
	path := os.join_path(dir.path, name)
	kind := if tool_cache_wipe_unreachable(dir.path, name) != '' {
		ToolCacheWipeKind.unknown
	} else if os.is_link(path) {
		ToolCacheWipeKind.link
	} else if os.is_dir(path) {
		ToolCacheWipeKind.folder
	} else if os.is_file(path) {
		ToolCacheWipeKind.file
	} else if os.exists(path) {
		ToolCacheWipeKind.other
	} else {
		ToolCacheWipeKind.gone
	}
	return ToolCacheWipeItem{
		kind:         kind
		owner:        u32(os.getuid())
		links:        1
		shut_by_mode: kind == .file && !os.is_writable(path)
	}
}

// tool_cache_wipe_leave_working_folder leaves the working folder as it is on Windows, where the wipe
// looks at a name by its path and needs no handle on the working folder. It returns ''.
fn tool_cache_wipe_leave_working_folder() string {
	return ''
}

// tool_cache_wipe_come_back does nothing on Windows (see `tool_cache_wipe_leave_working_folder`).
fn tool_cache_wipe_come_back(_ string) {}

// tool_cache_listed_name gives the name under which its folder lists what `path` reaches, without
// following a link there, which `os.real_path` does: a path reaches an item in any case, and by
// its short name, such as `TOOLCA~1`. The folder search behind it takes the last name in `path` as a
// pattern, so what it finds counts only when its long or its short name is that last name, ignoring
// case; a last name with `*`, `?`, `<`, `>` or `"`, which a search takes as a pattern and no file
// name holds, is not searched at all. It gives none when nothing is there.
fn tool_cache_listed_name(path string) ?string {
	want := os.file_name(path)
	if want == '' || want.contains_any('*?<>"') {
		return none
	}
	return tool_cache_searched_name(path, want)
}

// tool_cache_searched_name gives the name under which its folder lists what a folder search for
// `path` finds, when its long or its short name is `want`, ignoring case, and none otherwise.
fn tool_cache_searched_name(path string, want string) ?string {
	mut name := [261]u16{}
	if C.v_toolcache_listed_name(path.to_wide(), want.to_wide(), &name[0], name.len) == 0 {
		return none
	}
	// The helper wrote a name that ends in a zero into `name`, which `string_from_wide` reads up to.
	return unsafe { string_from_wide(&name[0]) }
}

// tool_cache_wipe_full_path gives the absolute path that Windows makes of `path` (see
// `winpath.absolute_path`).
fn tool_cache_wipe_full_path(path string) string {
	return winpath.absolute_path(path)
}

// tool_cache_wipe_unreachable says why the path made of the folder at `folder` and `name`, a name
// that the folder lists, does not reach that item, or gives '' when it does. Windows makes a path
// into another before it acts on it (see `winpath.full_path`): it drops the dots and spaces at the
// end of each name, so `tools.` reaches `tools`, and takes some names as devices, such as `nul`,
// which names depending on the version of Windows. The wipe asks Windows itself, so it holds for
// the version it runs on. WSL and `\\?\` paths make such names. The wipe leaves such an item as it
// is, and fails.
fn tool_cache_wipe_unreachable(folder string, name string) string {
	full := winpath.full_path(os.join_path_single(folder, name))
	if full.starts_with('\\\\.\\') {
		return 'its name is that of a device, which a Windows path reaches instead'
	}
	reached := os.file_name(full)
	if reached == name {
		return ''
	}
	if reached == name.trim_right('. ') {
		return 'its name ends in a dot or a space, which a Windows path drops'
	}
	return 'a Windows path with this name reaches `${reached}` instead'
}

// tool_cache_wipe_first_unreachable gives the first item at or below `name` in the folder at
// `folder` that no path reaches (see `tool_cache_wipe_unreachable`), as `remove_tree` names it when
// it fails on it: the names from `name` down, and why. It gives '' when there is none. A link is not
// followed. A dry run asks it, so that it fails where the removal would.
fn tool_cache_wipe_first_unreachable(folder string, name string) string {
	reason := tool_cache_wipe_unreachable(folder, name)
	if reason != '' {
		return '${name}: ${reason}'
	}
	path := os.join_path_single(folder, name)
	if !os.is_dir(path) || os.is_link(path) {
		return ''
	}
	mut names := os.ls(path) or { return '' }
	names.sort()
	for inner in names {
		found := tool_cache_wipe_first_unreachable(path, inner)
		if found != '' {
			return '${name}${os.path_separator}${found}'
		}
	}
	return ''
}

// open_folder takes the real folder `name` in the folder. A link there is not followed.
fn (dir ToolCacheWipeDir) open_folder(name string) !ToolCacheWipeDir {
	unreachable := tool_cache_wipe_unreachable(dir.path, name)
	if unreachable != '' {
		return error(unreachable)
	}
	return open_tool_cache_wipe_dir(os.join_path(dir.path, name))
}

// remove_file removes `name`, which is no folder. A link is removed itself, also one to a folder,
// which `os.rm` does not go through. A name that is gone already counts as removed.
fn (dir ToolCacheWipeDir) remove_file(name string) ! {
	unreachable := tool_cache_wipe_unreachable(dir.path, name)
	if unreachable != '' {
		return error(unreachable)
	}
	path := os.join_path(dir.path, name)
	os.rm(path) or {
		if os.exists(path) || os.is_link(path) {
			return err
		}
	}
}

// tool_cache_unlink removes what is at `path` when it is no folder; a link is removed itself, also
// one to a folder. `os.rm` removes no real folder on Windows. Something that is gone already counts
// as removed.
fn tool_cache_unlink(path string) ! {
	os.rm(path) or {
		if os.exists(path) || os.is_link(path) {
			return err
		}
	}
}

// remove_folder removes the folder `name` when it is empty. A name that is gone already counts as
// removed.
fn (dir ToolCacheWipeDir) remove_folder(name string) ! {
	unreachable := tool_cache_wipe_unreachable(dir.path, name)
	if unreachable != '' {
		return error(unreachable)
	}
	path := os.join_path(dir.path, name)
	os.rmdir(path) or {
		if os.exists(path) || os.is_link(path) {
			return err
		}
	}
}

// remove_tree removes `name` and, when it is a folder, all that is in it. Every item is attempted,
// and the first failure is returned afterwards, as `os.rmdir_all` does. A link it finds is removed
// itself, at any depth, and not followed; since everything here goes by path, a link that someone
// swaps in for a folder after the look at it is followed. A name that is gone already counts as
// removed at any depth, since another wipe can be at work in the same folder: `os.rmdir_all` fails
// on a file that went after it listed the folder. A removal that fails is not tried again: the
// wipe holds the build locks of the folder meanwhile (see `run_windows_cached_tool` for the one
// wait it needs).
fn (dir ToolCacheWipeDir) remove_tree(name string) ! {
	child := dir.open_folder(name) or {
		if err.code() == tool_cache_wipe_gone {
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
				first_error = '${name}${os.path_separator}${err.msg()}'
			}
		}
	}
	child.close()
	$if test {
		if dir.before_acting != unsafe { nil } {
			dir.before_acting(os.join_path(dir.path, name), 'rmdir')
		}
	}
	dir.remove_folder(name) or {
		if first_error == '' {
			first_error = '${name}: ${err.msg()}'
		}
	}
	if first_error != '' {
		// A folder that another wipe removed meanwhile is removed, whatever failed in it here.
		path := os.join_path(dir.path, name)
		if os.exists(path) || os.is_link(path) {
			return error(first_error)
		}
	}
}

// ToolCacheWipeLock is a build lock that a wipe tried to take.
struct ToolCacheWipeLock {
	state ToolCacheWipeLockState
	// problem says what the system does not let the wipe do with the lock file, for a lock file that
	// cannot be opened and for an attempt that failed.
	problem string
mut:
	handle voidptr = unsafe { nil }
}

// release lets a held lock go.
fn (mut held ToolCacheWipeLock) release() {
	if held.handle != unsafe { nil } {
		C.v_filelock_unlock(held.handle, 0, 0)
		C.v_toolcache_close_handle(held.handle)
		held.handle = unsafe { nil }
	}
}

// toolcache_windows_error_lock_violation is `ERROR_LOCK_VIOLATION`: another process has locked a
// part of the file.
const toolcache_windows_error_lock_violation = 33

// tool_cache_refused_lock is what came of a lock that Windows refused with the error `code`. Only
// `ERROR_LOCK_VIOLATION` means that another process holds it. Any other code, such as
// `ERROR_INVALID_FUNCTION` on a share of WSL, which has no locks, says nothing of other processes.
fn tool_cache_refused_lock(code int) ToolCacheWipeLock {
	if code == toolcache_windows_error_lock_violation {
		return ToolCacheWipeLock{
			state: .busy
		}
	}
	return ToolCacheWipeLock{
		state:   .failed
		problem: 'cannot be locked: ${os.get_error_msg(code)}'
	}
}

// try_lock takes the build lock `name` in the folder without waiting, with the kind of lock
// `os.filelock` takes for a build. `os.filelock` does not tell why an attempt failed, so the file is
// opened and locked here.
fn (dir ToolCacheWipeDir) try_lock(name string) ToolCacheWipeLock {
	path := os.join_path(dir.path, name)
	if os.is_link(path) || os.is_dir(path) {
		return ToolCacheWipeLock{
			state: .unsafe_name
		}
	}
	if !os.exists(path) {
		return ToolCacheWipeLock{
			state: .absent
		}
	}
	handle := C.v_toolcache_create_file_w(path.to_wide(),
		toolcache_windows_generic_read | toolcache_windows_generic_write, toolcache_windows_file_share_all,
		toolcache_windows_open_existing, toolcache_windows_file_attribute_normal)
	if handle == voidptr(-1) {
		// Read before another call sets it.
		code := int(C.GetLastError())
		// A file that another wipe removed meanwhile is no lock file any more.
		return ToolCacheWipeLock{
			state:   if os.exists(path) { ToolCacheWipeLockState.unopenable } else { .absent }
			problem: 'cannot be opened: ${os.get_error_msg(code)}'
		}
	}
	if C.v_filelock_lock(handle, 1, 1, 0, 0) != 0 {
		// Read before the close can set it.
		code := int(C.GetLastError())
		C.v_toolcache_close_handle(handle)
		return tool_cache_refused_lock(code)
	}
	return ToolCacheWipeLock{
		state:  .held
		handle: handle
	}
}

// create_lock_file makes the lock file `name` when nothing has that name, and fails with the reason
// when it does not.
fn (dir ToolCacheWipeDir) create_lock_file(name string, _ u32) ! {
	path := os.join_path(dir.path, name)
	if os.exists(path) || os.is_link(path) {
		return error('something has that name already')
	}
	ensure_tool_cache_lock_file(path)!
}

// repair_lock_file makes the lock file `name` writable again, so that it can be locked.
fn (dir ToolCacheWipeDir) repair_lock_file(name string) ! {
	os.chmod(os.join_path(dir.path, name), 0o600)!
}
