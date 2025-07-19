module closure

$if !freestanding {
	#include <sys/mman.h>
}

@[typedef]
pub struct C.pthread_mutex_t {}

struct ClosureMutex {
	closure_mtx C.pthread_mutex_t
}

@[inline]
fn closure_alloc_platform() &u8 {
	mut p := &u8(unsafe { nil })
	$if freestanding {
		// Freestanding environments (no OS) use simple malloc
		p = unsafe { malloc(g_closure.v_page_size * 2) }
		if isnil(p) {
			return unsafe { nil }
		}
	} $else {
		// Main OS environments use mmap to get aligned pages
		p = unsafe {
			C.mmap(0, g_closure.v_page_size * 2, C.PROT_READ | C.PROT_WRITE, C.MAP_ANONYMOUS | C.MAP_PRIVATE,
				-1, 0)
		}
		if p == &u8(C.MAP_FAILED) {
			return unsafe { nil }
		}
	}
	return p
}

@[inline]
fn closure_memory_protect_platform(ptr voidptr, size isize, attr MemoryProtectAtrr) {
	$if freestanding {
		// No memory protection in freestanding mode
	} $else {
		match attr {
			.read_exec {
				unsafe { C.mprotect(ptr, size, C.PROT_READ | C.PROT_EXEC) }
			}
			.read_write {
				unsafe { C.mprotect(ptr, size, C.PROT_READ | C.PROT_WRITE) }
			}
		}
	}
}

@[inline]
fn get_page_size_platform() int {
	// Determine system page size
	mut page_size := 0x4000
	$if !freestanding {
		// Query actual page size in OS environments
		page_size = unsafe { int(C.sysconf(C._SC_PAGESIZE)) }
	}
	// Calculate required allocation size
	page_size = page_size * (((assumed_page_size - 1) / page_size) + 1)
	return page_size
}

@[inline]
fn closure_mtx_lock_init_platform() {
	$if !freestanding {
		C.pthread_mutex_init(&g_closure.closure_mtx, 0)
	}
}

@[inline]
fn closure_mtx_lock_platform() {
	$if !freestanding {
		C.pthread_mutex_lock(&g_closure.closure_mtx)
	}
}

@[inline]
fn closure_mtx_unlock_platform() {
	$if !freestanding {
		C.pthread_mutex_unlock(&g_closure.closure_mtx)
	}
}
