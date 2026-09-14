// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

#flag windows -l advapi32

#include "@VMODROOT/cmd/v/toolcache_windows_helpers.h"

fn C.v_toolcache_move_file_ex_w(const_existing &u16, const_new &u16, flags u32) int

fn C.v_toolcache_create_file_w(const_path &u16, desired_access u32, share_mode u32,
	creation_disposition u32, flags_and_attributes u32) voidptr

fn C.v_toolcache_get_file_information(handle voidptr, information voidptr) int

fn C.v_toolcache_close_handle(handle voidptr) int

fn C.v_toolcache_root_is_private(const_path &u16) int

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
const toolcache_windows_create_new = u32(1)
const toolcache_windows_open_existing = u32(3)
const toolcache_windows_file_attribute_normal = u32(0x00000080)
const toolcache_windows_file_attribute_directory = u32(0x00000010)
const toolcache_windows_file_attribute_reparse_point = u32(0x00000400)
const toolcache_windows_file_flag_open_reparse_point = u32(0x00200000)
const toolcache_windows_file_flag_backup_semantics = u32(0x02000000)

// ToolCacheEntryDir pins the directory that receives a compiled tool. Omitting
// FILE_SHARE_DELETE keeps the pathname bound to this directory until the build is over;
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
	handle := C.v_toolcache_create_file_w(w_path, 0, toolcache_windows_file_share_read_write, toolcache_windows_open_existing, toolcache_windows_file_flag_backup_semantics | toolcache_windows_file_flag_open_reparse_point)
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
	// to_wide owns an unmanaged buffer that CreateFileW borrows until it returns.
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
	// process exits, so it is left for `prune_stale_tool_binaries` to collect later.
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
