module closure

#include <synchapi.h>

struct ClosureMutex {
	closure_mtx C.SRWLOCK
}

@[inline]
fn closure_alloc_platform() &u8 {
	p := &u8(C.VirtualAlloc(0, g_closure.v_page_size * 2, C.MEM_COMMIT | C.MEM_RESERVE,
		C.PAGE_READWRITE))
	return p
}

@[inline]
fn closure_memory_protect_platform(ptr voidptr, size isize, attr MemoryProtectAtrr) {
	mut tmp := u32(0)
	match attr {
		.read_exec {
			_ := C.VirtualProtect(ptr, size, C.PAGE_EXECUTE_READ, &tmp)
		}
		.read_write {
			_ := C.VirtualProtect(ptr, size, C.PAGE_READWRITE, &tmp)
		}
	}
}

@[inline]
fn get_page_size_platform() int {
	// Determine system page size
	mut si := C.SYSTEM_INFO{}
	C.GetNativeSystemInfo(&si)

	// Calculate required allocation size
	page_size := int(si.dwPageSize) * (((assumed_page_size - 1) / int(si.dwPageSize)) + 1)
	return page_size
}

@[inline]
fn closure_mtx_lock_init_platform() {
	C.InitializeSRWLock(&g_closure.closure_mtx)
}

@[inline]
fn closure_mtx_lock_platform() {
	C.AcquireSRWLockExclusive(&g_closure.closure_mtx)
}

@[inline]
fn closure_mtx_unlock_platform() {
	C.ReleaseSRWLockExclusive(&g_closure.closure_mtx)
}
