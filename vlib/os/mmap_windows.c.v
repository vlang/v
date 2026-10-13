module os

fn C.CreateFileA(name &char, access u32, share u32, security voidptr, creation u32, flags u32, template voidptr) voidptr
fn C.CreateFileMappingA(h voidptr, attrs voidptr, protect u32, maxhi u32, maxlo u32, name voidptr) voidptr
fn C.MapViewOfFile(h voidptr, access u32, offhi u32, offlo u32, nbytes usize) voidptr
fn C.UnmapViewOfFile(addr voidptr) i32
fn C.FlushViewOfFile(addr voidptr, nbytes usize) i32
fn C.CloseHandle(voidptr) bool
fn C.SetFilePointer(h voidptr, lo i32, hi &u32, method u32) u32
fn C.SetEndOfFile(h voidptr) i32

const mmap_generic_read = u32(0x80000000)
const mmap_generic_write = u32(0x40000000)
const mmap_file_share_read = u32(1)
const mmap_open_existing = u32(3)
const mmap_create_always = u32(2)
const mmap_attr_normal = u32(0x80)
const mmap_page_readonly = u32(0x02)
const mmap_page_readwrite = u32(0x04)
const mmap_map_read = u32(0x04)
const mmap_map_write = u32(0x02)

// mmap_map_windows opens path, creates a file-mapping object and maps a view
// of len bytes, returning the address and the mapping handle (which must stay
// open until unmap; the file handle is closed here).
fn mmap_map_windows(path string, len u64, write bool, create bool) !(voidptr, voidptr) {
	access := if write { mmap_generic_read | mmap_generic_write } else { mmap_generic_read }
	creation := if create { mmap_create_always } else { mmap_open_existing }
	h_file := C.CreateFileA(&char(path.str), access, mmap_file_share_read, unsafe { nil },
		creation, mmap_attr_normal, unsafe { nil })
	if h_file == voidptr(-1) {
		return error('os: cannot open `${path}` for mapping')
	}
	if create {
		// Size the file up front (split u64 length for SetFilePointer).
		mut hi := u32(len >> 32)
		C.SetFilePointer(h_file, i32(u32(len)), &hi, 0)
		if C.SetEndOfFile(h_file) == 0 {
			C.CloseHandle(h_file)
			return error('os: cannot size `${path}` for mapping')
		}
	}
	protect := if write { mmap_page_readwrite } else { mmap_page_readonly }
	h_map := C.CreateFileMappingA(h_file, unsafe { nil }, protect, 0, 0, unsafe { nil })
	if h_map == unsafe { nil } {
		C.CloseHandle(h_file)
		return error('os: cannot create file mapping for `${path}`')
	}
	view_access := if write { mmap_map_read | mmap_map_write } else { mmap_map_read }
	addr := C.MapViewOfFile(h_map, view_access, 0, 0, 0)
	C.CloseHandle(h_file)
	if addr == unsafe { nil } {
		C.CloseHandle(h_map)
		return error('os: cannot map view of `${path}`')
	}
	return addr, h_map
}

// mmap_unmap_windows releases the view and closes the mapping handle.
fn mmap_unmap_windows(addr voidptr, mapping voidptr) {
	C.UnmapViewOfFile(addr)
	C.CloseHandle(mapping)
}

// mmap_sync_windows flushes the view to the file.
fn mmap_sync_windows(addr voidptr, len u64) ! {
	if C.FlushViewOfFile(addr, usize(len)) == 0 {
		return error('os: FlushViewOfFile failed')
	}
}

// mmap_sync_platform flushes a mapping (windows side).
fn mmap_sync_platform(addr voidptr, len u64) ! {
	return mmap_sync_windows(addr, len)
}

// mmap_unmap_platform releases a mapping (windows side).
fn mmap_unmap_platform(addr voidptr, len u64, mapping voidptr) {
	_ = len
	mmap_unmap_windows(addr, mapping)
}
