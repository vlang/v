module closure

$if !freestanding && !vinix {
	#include <sys/mman.h>
	#include <pthread.h>

	@[typedef]
	struct C.pthread_mutex_t {}

	// The lock and the flag of the one-time setup are globals of this module, with C
	// initializers, rather than the file-static storage of a C header: every
	// translation unit that includes such a header gets its own copy of them.
	@[cinit]
	__global g_closure_once_mutex C.pthread_mutex_t = C.PTHREAD_MUTEX_INITIALIZER

	@[cinit]
	__global g_closure_once_done = false
}

struct ClosureMutex {
	closure_mtx [128]u8
}

@[inline]
fn closure_mtx_ptr_platform() voidptr {
	return unsafe { voidptr(&g_closure.closure_mtx[0]) }
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
			C.mmap(0, g_closure.v_page_size * 2, C.PROT_READ | C.PROT_WRITE,
				C.MAP_ANONYMOUS | C.MAP_PRIVATE, -1, 0)
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
	$if !freestanding || vinix {
		C.pthread_mutex_init(closure_mtx_ptr_platform(), 0)
	}
}

@[inline]
fn closure_mtx_lock_platform() {
	$if race ? {
		// Like Go's runtime, the closure allocator is invisible to the race detector.
		// Otherwise its mutex would order the threads that create and destroy closures,
		// hiding races between them, and reused closure slots would look like races.
		racedisable()
	}
	$if !freestanding || vinix {
		C.pthread_mutex_lock(closure_mtx_ptr_platform())
	}
}

@[inline]
fn closure_mtx_unlock_platform() {
	$if !freestanding || vinix {
		C.pthread_mutex_unlock(closure_mtx_ptr_platform())
	}
	$if race ? {
		raceenable()
	}
}

@[inline]
fn closure_current_thread_id_platform() u64 {
	$if !freestanding {
		return u64(C.pthread_self())
	}
	return u64(0)
}

@[inline]
fn closure_init_once_platform() {
	$if freestanding || vinix {
		if isnil(g_closure.closure_ptr) {
			closure_init_body()
		}
	} $else {
		C.pthread_mutex_lock(&g_closure_once_mutex)
		if !g_closure_once_done {
			closure_init_body()
			g_closure_once_done = true
		}
		C.pthread_mutex_unlock(&g_closure_once_mutex)
	}
}
