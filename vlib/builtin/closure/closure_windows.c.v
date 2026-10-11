module closure

#include <synchapi.h>

// The lock and the flag of the one-time setup are globals of this module, with C
// initializers, rather than the file-static storage of a C header: every
// translation unit that includes such a header gets its own copy of them.
@[cinit]
__global g_closure_once_lock C.SRWLOCK = C.SRWLOCK_INIT

@[cinit]
__global g_closure_once_done = false

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
	mut tmp := C.DWORD(0)
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

@[inline]
fn closure_current_thread_id_platform() u64 {
	return u64(C.GetCurrentThreadId())
}

@[inline]
fn closure_init_once_platform() {
	C.AcquireSRWLockExclusive(&g_closure_once_lock)
	if !g_closure_once_done {
		closure_init_body()
		g_closure_once_done = true
	}
	C.ReleaseSRWLockExclusive(&g_closure_once_lock)
}
