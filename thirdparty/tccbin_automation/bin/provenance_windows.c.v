module bin

import os

#insert "@DIR/provenance_native_nix.h"

struct WindowsByHandleFileInformation {
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

struct WindowsFileBasicInformation {
	creation_time    i64
	last_access_time i64
	last_write_time  i64
	change_time      i64
	file_attributes  u32
}

fn C._get_osfhandle(fd int) voidptr
fn C.GetFileInformationByHandle(handle voidptr, information voidptr) bool
fn C.GetFileInformationByHandleEx(handle voidptr, information_class int, information voidptr,
	information_size u32) bool
fn C.GetFileType(handle voidptr) u32
fn C.ReadFile(handle voidptr, buffer voidptr, bytes_to_read u32, bytes_read &u32,
	overlapped voidptr) bool
fn C.CreateFileW(path &u16, desired_access u32, share_mode u32, security_attributes voidptr,
	creation_disposition u32, flags_and_attributes u32, template_file voidptr) voidptr
fn C.CloseHandle(handle voidptr) bool
fn C.tccbin_windows_open_directory_path_no_follow(path &u16) voidptr
fn C.tccbin_windows_open_child_no_follow(parent voidptr, name &u16, mode int) voidptr
fn C.tccbin_windows_open_directory_enumerator(parent voidptr) voidptr
fn C.tccbin_windows_read_directory_entry(enumerator voidptr, buffer &u16, capacity u64) int
fn C.tccbin_windows_close_directory_enumerator(enumerator voidptr) int

const windows_file_share_all = u32(0x00000001 | 0x00000002 | 0x00000004)
const windows_generic_read = u32(0x80000000)
const windows_open_existing = u32(3)
const windows_file_attribute_normal = u32(0x00000080)
const windows_file_attribute_directory = u32(0x00000010)
const windows_file_attribute_reparse_point = u32(0x00000400)
const windows_file_flag_open_reparse_point = u32(0x00200000)
const windows_file_flag_sequential_scan = u32(0x08000000)
const windows_file_type_disk = u32(0x00000001)
const windows_file_basic_info_class = 0

fn windows_snapshot_from_handle(handle voidptr) !NativeFileSnapshot {
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('Windows payload file handle identity is unavailable')
	}
	mut information := WindowsByHandleFileInformation{}
	mut basic := WindowsFileBasicInformation{}
	if !C.GetFileInformationByHandle(handle, voidptr(&information))
		|| !C.GetFileInformationByHandleEx(handle, windows_file_basic_info_class, voidptr(&basic), u32(sizeof(WindowsFileBasicInformation)))
		|| information.file_attributes & windows_file_attribute_reparse_point != 0
		|| basic.file_attributes & windows_file_attribute_reparse_point != 0 {
		return error('Windows payload file identity query failed or found a reparse point')
	}
	index := (u64(information.file_index_high) << 32) | u64(information.file_index_low)
	if information.volume_serial_number == 0 || index == 0 || information.number_of_links == 0 {
		return error('Windows payload file identity is not reliable')
	}
	return NativeFileSnapshot{
		identity:            NativeFileIdentity{
			volume:   information.volume_serial_number
			index:    index
			nlink:    information.number_of_links
			reliable: true
		}
		mode:                basic.file_attributes
		size:                (u64(information.file_size_high) << 32) | u64(information.file_size_low)
		mtime_windows_ticks: basic.last_write_time
		ctime_windows_ticks: basic.change_time
		regular:             C.GetFileType(handle) == windows_file_type_disk
			&& basic.file_attributes & windows_file_attribute_directory == 0
	}
}

fn windows_identity_from_handle(handle voidptr) !NativeFileIdentity {
	return windows_snapshot_from_handle(handle)!.identity
}

fn windows_file_identity(file &os.File) !NativeFileIdentity {
	handle := C._get_osfhandle(file.fd)
	return windows_identity_from_handle(handle)!
}

fn windows_file_snapshot(file &os.File) !NativeFileSnapshot {
	handle := C._get_osfhandle(file.fd)
	return windows_snapshot_from_handle(handle)!
}

fn windows_path_file_identity(path string) !NativeFileIdentity {
	return windows_path_file_snapshot(path)!.identity
}

fn windows_path_file_snapshot(path string) !NativeFileSnapshot {
	wide_path := path.to_wide()
	defer {
		unsafe { free(voidptr(wide_path)) }
	}
	handle := C.CreateFileW(wide_path, 0, windows_file_share_all, unsafe { nil },
		windows_open_existing,
		windows_file_attribute_normal | windows_file_flag_open_reparse_point, unsafe { nil })
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('Windows payload path identity cannot be opened')
	}
	defer {
		C.CloseHandle(handle)
	}
	return windows_snapshot_from_handle(handle)!
}

fn native_open_toolchain_document(path string) !(NativeToolchainDocument, NativeFileSnapshot) {
	wide_path := path.to_wide()
	defer {
		unsafe { free(voidptr(wide_path)) }
	}
	handle := C.CreateFileW(wide_path, windows_generic_read, windows_file_share_all,
		unsafe { nil }, windows_open_existing,
		windows_file_attribute_normal | windows_file_flag_open_reparse_point | windows_file_flag_sequential_scan,
		unsafe { nil })
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('native toolchain document cannot be opened without following links')
	}
	snapshot := windows_snapshot_from_handle(handle) or {
		C.CloseHandle(handle)
		return err
	}
	if !snapshot.regular {
		C.CloseHandle(handle)
		return error('native toolchain document is not a physical regular file')
	}
	return NativeToolchainDocument{
		handle: handle
		fd:     -1
		opened: true
	}, snapshot
}

fn native_toolchain_document_snapshot(document &NativeToolchainDocument) !NativeFileSnapshot {
	if !document.opened || document.handle == voidptr(-1) || document.handle == unsafe { nil } {
		return error('native toolchain document handle is closed')
	}
	return windows_snapshot_from_handle(document.handle)!
}

fn native_read_toolchain_document(document &NativeToolchainDocument, mut buffer []u8) !int {
	if !document.opened || document.handle == voidptr(-1) || document.handle == unsafe { nil } {
		return error('native toolchain document handle is closed')
	}
	mut bytes_read := u32(0)
	if !C.ReadFile(document.handle, buffer.data, u32(buffer.len), &bytes_read, unsafe { nil })
		|| bytes_read > u32(buffer.len) {
		return error('native toolchain document read failed')
	}
	return int(bytes_read)
}

fn native_close_toolchain_document(mut document NativeToolchainDocument) {
	if document.opened && document.handle != voidptr(-1) && document.handle != unsafe { nil } {
		C.CloseHandle(document.handle)
	}
	document.handle = unsafe { nil }
	document.opened = false
}

fn native_open_directory(path string) !(NativeDirectoryHandle, NativeFileSnapshot) {
	wide_path := path.to_wide()
	defer {
		unsafe { free(voidptr(wide_path)) }
	}
	handle := C.tccbin_windows_open_directory_path_no_follow(wide_path)
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('native directory cannot be opened without following links')
	}
	snapshot := windows_snapshot_from_handle(handle) or {
		C.CloseHandle(handle)
		return err
	}
	if snapshot.regular || snapshot.mode & windows_file_attribute_directory == 0 {
		C.CloseHandle(handle)
		return error('native directory handle is not a physical directory')
	}
	return NativeDirectoryHandle{
		handle: handle
		fd:     -1
		opened: true
	}, snapshot
}

fn native_directory_path_snapshot(path string) !NativeFileSnapshot {
	wide_path := path.to_wide()
	defer {
		unsafe { free(voidptr(wide_path)) }
	}
	handle := C.tccbin_windows_open_directory_path_no_follow(wide_path)
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('native directory path cannot be opened without following links')
	}
	defer {
		C.CloseHandle(handle)
	}
	snapshot := windows_snapshot_from_handle(handle)!
	if snapshot.regular || snapshot.mode & windows_file_attribute_directory == 0 {
		return error('native directory path is not a physical directory')
	}
	return snapshot
}

fn native_open_child_directory(parent &NativeDirectoryHandle,
	name string) !(NativeDirectoryHandle, NativeFileSnapshot) {
	if !parent.opened || parent.handle == voidptr(-1) || parent.handle == unsafe { nil } {
		return error('native parent directory handle is closed')
	}
	wide_name := name.to_wide()
	defer {
		unsafe { free(voidptr(wide_name)) }
	}
	handle := C.tccbin_windows_open_child_no_follow(parent.handle, wide_name, 1)
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('native child directory cannot be opened without following links')
	}
	snapshot := windows_snapshot_from_handle(handle) or {
		C.CloseHandle(handle)
		return err
	}
	if snapshot.regular || snapshot.mode & windows_file_attribute_directory == 0 {
		C.CloseHandle(handle)
		return error('native child directory handle is not a physical directory')
	}
	return NativeDirectoryHandle{
		handle: handle
		fd:     -1
		opened: true
	}, snapshot
}

fn native_open_child_document(parent &NativeDirectoryHandle,
	name string) !(NativeToolchainDocument, NativeFileSnapshot) {
	if !parent.opened || parent.handle == voidptr(-1) || parent.handle == unsafe { nil } {
		return error('native parent directory handle is closed')
	}
	wide_name := name.to_wide()
	defer {
		unsafe { free(voidptr(wide_name)) }
	}
	handle := C.tccbin_windows_open_child_no_follow(parent.handle, wide_name, 0)
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('native child document cannot be opened without following links')
	}
	snapshot := windows_snapshot_from_handle(handle) or {
		C.CloseHandle(handle)
		return err
	}
	if !snapshot.regular {
		C.CloseHandle(handle)
		return error('native child document is not a physical regular file')
	}
	return NativeToolchainDocument{
		handle: handle
		fd:     -1
		opened: true
	}, snapshot
}

fn native_directory_snapshot(directory &NativeDirectoryHandle) !NativeFileSnapshot {
	if !directory.opened || directory.handle == voidptr(-1) || directory.handle == unsafe { nil } {
		return error('native directory handle is closed')
	}
	return windows_snapshot_from_handle(directory.handle)!
}

fn native_child_snapshot(parent &NativeDirectoryHandle, name string) !NativeFileSnapshot {
	if !parent.opened || parent.handle == voidptr(-1) || parent.handle == unsafe { nil } {
		return error('native parent directory handle is closed')
	}
	wide_name := name.to_wide()
	defer {
		unsafe { free(voidptr(wide_name)) }
	}
	handle := C.tccbin_windows_open_child_no_follow(parent.handle, wide_name, 2)
	if handle == voidptr(-1) || handle == unsafe { nil } {
		return error('native child entry identity query failed')
	}
	defer {
		C.CloseHandle(handle)
	}
	return windows_snapshot_from_handle(handle)!
}

fn native_directory_entries_bounded(directory &NativeDirectoryHandle,
	maximum int) ![]string {
	if !directory.opened || directory.handle == voidptr(-1) || directory.handle == unsafe { nil }
		|| maximum < 0 || maximum > native_validation_walker_max_entries {
		return error('native directory enumeration request is invalid')
	}
	enumerator := C.tccbin_windows_open_directory_enumerator(directory.handle)
	if enumerator == unsafe { nil } {
		return error('native directory cannot be enumerated from its open handle')
	}
	defer {
		C.tccbin_windows_close_directory_enumerator(enumerator)
	}
	mut buffer := []u16{len: 1024}
	mut entries := []string{cap: maximum}
	for {
		read := C.tccbin_windows_read_directory_entry(enumerator, buffer.data, u64(buffer.len))
		if read == 0 {
			break
		}
		if read < 0 {
			return error('native directory enumeration failed')
		}
		if entries.len >= maximum {
			return error('native directory contains more entries than its strict bound')
		}
		entries << unsafe { string_from_wide2(buffer.data, read) }
	}
	return entries
}

fn native_close_directory(mut directory NativeDirectoryHandle) {
	if directory.opened && directory.handle != voidptr(-1) && directory.handle != unsafe { nil } {
		C.CloseHandle(directory.handle)
	}
	directory.handle = unsafe { nil }
	directory.fd = -1
	directory.opened = false
}
