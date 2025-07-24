module closure

struct ClosureMutex {
}

@[inline]
fn closure_alloc_platform() &u8 {
	mut p := &u8(unsafe { nil })
	return p
}

@[inline]
fn closure_memory_protect_platform(ptr voidptr, size isize, attr MemoryProtectAtrr) {
}

@[inline]
fn get_page_size_platform() int {
	// Determine system page size
	mut page_size := 0x4000
	return page_size
}

@[inline]
fn closure_mtx_lock_init_platform() {
}

@[inline]
fn closure_mtx_lock_platform() {
}

@[inline]
fn closure_mtx_unlock_platform() {
}
