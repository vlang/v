// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

fn C.MoveFileExW(existing &u16, new &u16, flags u32) i32

fn C.CreateFileW(const_path &u16, desired_access u32, share_mode u32, security_attributes &u16,
	creation_disposition u32, flags_and_attributes u32, template_file voidptr) voidptr

fn C.GetFileInformationByHandle(handle voidptr, information voidptr) bool

fn C.CloseHandle(handle voidptr) bool

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
const toolcache_windows_open_existing = u32(3)
const toolcache_windows_file_attribute_normal = u32(0x00000080)

// windows_binary_file_identity returns the stable identity Windows assigns to an open file.
// Unlike the CRT inode, the volume serial and 64-bit file index distinguish concurrently
// published binaries. The creation time also guards against a later reuse of the same index.
fn windows_binary_file_identity(path string) ?string {
	w_path := path.replace('/', '\\').to_wide()
	defer {
		unsafe { free(voidptr(w_path)) }
	}
	handle := C.CreateFileW(w_path, 0, toolcache_windows_file_share_all, unsafe { nil }, toolcache_windows_open_existing, toolcache_windows_file_attribute_normal, unsafe { nil })
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return none
	}
	defer {
		C.CloseHandle(handle)
	}
	mut information := WindowsToolCacheFileInformation{}
	if !C.GetFileInformationByHandle(handle, voidptr(&information)) {
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
	w_source := source.replace('/', '\\')
	w_destination := destination.replace('/', '\\')
	return C.MoveFileExW(w_source.to_wide(), w_destination.to_wide(), movefile_replace_existing) != 0
}
