module bin

import os

#insert "@DIR/provenance_native_nix.h"

@[typedef]
struct C.tccbin_native_stat_snapshot {
	volume     u64
	index      u64
	links      u64
	size       u64
	mode       u32
	mtime_sec  i64
	mtime_nsec i64
	ctime_sec  i64
	ctime_nsec i64
	regular    int
}

fn C.tccbin_lstat_snapshot(path &char, snapshot &C.tccbin_native_stat_snapshot) int
fn C.tccbin_fstat_snapshot(fd int, snapshot &C.tccbin_native_stat_snapshot) int
fn C.tccbin_fstatat_snapshot(parent_fd int, name &char,
	snapshot &C.tccbin_native_stat_snapshot) int
fn C.tccbin_open_document_no_follow(path &char) int
fn C.tccbin_open_directory_no_follow(path &char) int
fn C.tccbin_openat_no_follow(parent_fd int, name &char, directory int) int
fn C.tccbin_open_directory_enumerator(directory_fd int) voidptr
fn C.tccbin_read_directory_entry(directory voidptr, buffer &char, capacity u64) int
fn C.tccbin_close_directory_enumerator(directory voidptr) int
fn C.tccbin_read_document(fd int, buffer voidptr, length u64) i64
fn C.tccbin_close_document(fd int) int

fn native_snapshot_from_posix(snapshot C.tccbin_native_stat_snapshot) NativeFileSnapshot {
	return NativeFileSnapshot{
		identity:   NativeFileIdentity{
			volume:   snapshot.volume
			index:    snapshot.index
			nlink:    snapshot.links
			reliable: snapshot.index != 0
		}
		mode:       snapshot.mode
		size:       snapshot.size
		mtime_sec:  snapshot.mtime_sec
		mtime_nsec: snapshot.mtime_nsec
		ctime_sec:  snapshot.ctime_sec
		ctime_nsec: snapshot.ctime_nsec
		regular:    snapshot.regular == 1
	}
}

fn posix_path_file_snapshot(path string) !NativeFileSnapshot {
	mut snapshot := C.tccbin_native_stat_snapshot{}
	if C.tccbin_lstat_snapshot(&char(path.str), &snapshot) != 0 {
		return error('POSIX path identity query failed')
	}
	return native_snapshot_from_posix(snapshot)
}

fn posix_fd_snapshot(fd int) !NativeFileSnapshot {
	mut snapshot := C.tccbin_native_stat_snapshot{}
	if C.tccbin_fstat_snapshot(fd, &snapshot) != 0 {
		return error('POSIX file handle identity query failed')
	}
	return native_snapshot_from_posix(snapshot)
}

fn posix_file_snapshot(file &os.File) !NativeFileSnapshot {
	return posix_fd_snapshot(file.fd)!
}

fn native_open_toolchain_document(path string) !(NativeToolchainDocument, NativeFileSnapshot) {
	fd := C.tccbin_open_document_no_follow(&char(path.str))
	if fd < 0 {
		return error('native toolchain document cannot be opened without following links')
	}
	snapshot := posix_fd_snapshot(fd) or {
		C.tccbin_close_document(fd)
		return err
	}
	if !snapshot.regular {
		C.tccbin_close_document(fd)
		return error('native toolchain document is not a physical regular file')
	}
	return NativeToolchainDocument{
		fd:     fd
		opened: true
	}, snapshot
}

fn native_toolchain_document_snapshot(document &NativeToolchainDocument) !NativeFileSnapshot {
	if !document.opened || document.fd < 0 {
		return error('native toolchain document handle is closed')
	}
	return posix_fd_snapshot(document.fd)!
}

fn native_read_toolchain_document(document &NativeToolchainDocument, mut buffer []u8) !int {
	if !document.opened || document.fd < 0 {
		return error('native toolchain document handle is closed')
	}
	read := C.tccbin_read_document(document.fd, buffer.data, u64(buffer.len))
	if read < 0 || read > buffer.len {
		return error('native toolchain document read failed')
	}
	return int(read)
}

fn native_close_toolchain_document(mut document NativeToolchainDocument) {
	if document.opened && document.fd >= 0 {
		C.tccbin_close_document(document.fd)
	}
	document.fd = -1
	document.opened = false
}

fn native_open_directory(path string) !(NativeDirectoryHandle, NativeFileSnapshot) {
	fd := C.tccbin_open_directory_no_follow(&char(path.str))
	if fd < 0 {
		return error('native directory cannot be opened without following links')
	}
	snapshot := posix_fd_snapshot(fd) or {
		C.tccbin_close_document(fd)
		return err
	}
	if snapshot.regular {
		C.tccbin_close_document(fd)
		return error('native directory handle is not a physical directory')
	}
	return NativeDirectoryHandle{
		fd:     fd
		opened: true
	}, snapshot
}

fn native_directory_path_snapshot(path string) !NativeFileSnapshot {
	return posix_path_file_snapshot(path)!
}

fn native_open_child_directory(parent &NativeDirectoryHandle,
	name string) !(NativeDirectoryHandle, NativeFileSnapshot) {
	if !parent.opened || parent.fd < 0 {
		return error('native parent directory handle is closed')
	}
	fd := C.tccbin_openat_no_follow(parent.fd, &char(name.str), 1)
	if fd < 0 {
		return error('native child directory cannot be opened without following links')
	}
	snapshot := posix_fd_snapshot(fd) or {
		C.tccbin_close_document(fd)
		return err
	}
	if snapshot.regular {
		C.tccbin_close_document(fd)
		return error('native child directory handle is not a physical directory')
	}
	return NativeDirectoryHandle{
		fd:     fd
		opened: true
	}, snapshot
}

fn native_open_child_document(parent &NativeDirectoryHandle,
	name string) !(NativeToolchainDocument, NativeFileSnapshot) {
	if !parent.opened || parent.fd < 0 {
		return error('native parent directory handle is closed')
	}
	fd := C.tccbin_openat_no_follow(parent.fd, &char(name.str), 0)
	if fd < 0 {
		return error('native child document cannot be opened without following links')
	}
	snapshot := posix_fd_snapshot(fd) or {
		C.tccbin_close_document(fd)
		return err
	}
	if !snapshot.regular {
		C.tccbin_close_document(fd)
		return error('native child document is not a physical regular file')
	}
	return NativeToolchainDocument{
		fd:     fd
		opened: true
	}, snapshot
}

fn native_directory_snapshot(directory &NativeDirectoryHandle) !NativeFileSnapshot {
	if !directory.opened || directory.fd < 0 {
		return error('native directory handle is closed')
	}
	return posix_fd_snapshot(directory.fd)!
}

fn native_child_snapshot(parent &NativeDirectoryHandle, name string) !NativeFileSnapshot {
	if !parent.opened || parent.fd < 0 {
		return error('native parent directory handle is closed')
	}
	mut snapshot := C.tccbin_native_stat_snapshot{}
	if C.tccbin_fstatat_snapshot(parent.fd, &char(name.str), &snapshot) != 0 {
		return error('native child entry identity query failed')
	}
	return native_snapshot_from_posix(snapshot)
}

fn native_directory_entries_bounded(directory &NativeDirectoryHandle,
	maximum int) ![]string {
	if !directory.opened || directory.fd < 0 || maximum < 0
		|| maximum > native_validation_walker_max_entries {
		return error('native directory enumeration request is invalid')
	}
	enumerator := C.tccbin_open_directory_enumerator(directory.fd)
	if enumerator == unsafe { nil } {
		return error('native directory cannot be enumerated from its open handle')
	}
	defer {
		C.tccbin_close_directory_enumerator(enumerator)
	}
	mut buffer := []u8{len: 4096}
	mut entries := []string{cap: maximum}
	for {
		read := C.tccbin_read_directory_entry(enumerator, &char(buffer.data), u64(buffer.len))
		if read == 0 {
			break
		}
		if read < 0 {
			return error('native directory enumeration failed')
		}
		if entries.len >= maximum {
			return error('native directory contains more entries than its strict bound')
		}
		entries << unsafe { cstring_to_vstring(&char(buffer.data)) }
	}
	return entries
}

fn native_close_directory(mut directory NativeDirectoryHandle) {
	if directory.opened && directory.fd >= 0 {
		C.tccbin_close_document(directory.fd)
	}
	directory.fd = -1
	directory.opened = false
}
