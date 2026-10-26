module os

fn C.CreateFileMappingW(h voidptr, attrs voidptr, protect u32, maxhi u32, maxlo u32, name &u16) voidptr
fn C.FlushFileBuffers(h voidptr) bool
fn C.MapViewOfFile(h voidptr, access u32, offhi u32, offlo u32, nbytes uint) voidptr
fn C.UnmapViewOfFile(addr voidptr) i32
fn C.FlushViewOfFile(addr voidptr, nbytes uint) i32

const mmap_generic_read = u32(0x80000000)
const mmap_generic_write = u32(0x40000000)
const mmap_file_share_read_write = u32(3)
const mmap_open_existing = u32(3)
const mmap_create_always = u32(2)
const mmap_attr_normal = u32(0x80)
const mmap_page_readonly = u32(0x02)
const mmap_page_readwrite = u32(0x04)
const mmap_map_read = u32(0x04)
const mmap_map_write = u32(0x02)

// mmap_map_windows opens path, creates a file-mapping object and maps a view
// of len bytes, returning the address and the mapping handle (which must stay
// open until unmap; the file handle is retained for sync).
fn mmap_map_windows(path string, len u64, write bool, create bool) !(voidptr, voidptr, voidptr) {
	access := if write { mmap_generic_read | mmap_generic_write } else { mmap_generic_read }
	creation := if create { mmap_create_always } else { mmap_open_existing }
	h_file := C.CreateFileW(path.to_wide(), access, mmap_file_share_read_write, unsafe { nil },
		creation, mmap_attr_normal, unsafe { nil })
	if h_file == voidptr(-1) {
		return error('os: cannot open `${path}` for mapping')
	}
	protect := if write { mmap_page_readwrite } else { mmap_page_readonly }
	h_map := C.CreateFileMappingW(h_file, unsafe { nil }, protect, u32(len >> 32), u32(len), unsafe { nil })
	if h_map == unsafe { nil } {
		C.CloseHandle(h_file)
		return error('os: cannot create file mapping for `${path}`')
	}
	view_access := if write { mmap_map_read | mmap_map_write } else { mmap_map_read }
	addr := C.MapViewOfFile(h_map, view_access, 0, 0, uint(len))
	if addr == unsafe { nil } {
		C.CloseHandle(h_map)
		C.CloseHandle(h_file)
		return error('os: cannot map view of `${path}`')
	}
	if create {
		// File-backed mapping extensions are not guaranteed to contain zero bytes.
		unsafe { vmemset(addr, 0, int(len)) }
	}
	return addr, h_map, h_file
}

// mmap_unmap_windows releases the view and closes the mapping handle.
fn mmap_unmap_windows(addr voidptr, mapping voidptr, file_handle voidptr) {
	C.UnmapViewOfFile(addr)
	C.CloseHandle(mapping)
	C.CloseHandle(file_handle)
}

// mmap_sync_windows flushes the view to the file.
fn mmap_sync_windows(addr voidptr, len u64, file_handle voidptr) ! {
	if C.FlushViewOfFile(addr, uint(len)) == 0 {
		return error('os: FlushViewOfFile failed')
	}
	if !C.FlushFileBuffers(file_handle) { return error('os: FlushFileBuffers failed') }
}

// mmap_sync_platform flushes a mapping (windows side).
fn mmap_sync_platform(addr voidptr, len u64, file_handle voidptr) ! {
	return mmap_sync_windows(addr, len, file_handle)
}

// mmap_unmap_platform releases a mapping (windows side).
fn mmap_unmap_platform(addr voidptr, len u64, mapping voidptr, file_handle voidptr) {
	_ = len
	mmap_unmap_windows(addr, mapping, file_handle)
}
