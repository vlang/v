module builtin

#include <sys/mman.h>

struct ClosureMutex {
	closure_mtx C.pthread_mutex_t
}

// closure_alloc allocates executable memory pages for closures(INTERNAL COMPILER USE ONLY).
fn closure_alloc() {
	mut p := &u8(unsafe { nil })
	$if freestanding {
		// Freestanding environments (no OS) use simple malloc
		p = unsafe { malloc(g_closure.v_page_size * 2) }
		if isnil(p) {
			return
		}
	} $else {
		// Main OS environments use mmap to get aligned pages
		p = unsafe {
			C.mmap(0, g_closure.v_page_size * 2, C.PROT_READ | C.PROT_WRITE, C.MAP_ANONYMOUS | C.MAP_PRIVATE,
				-1, 0)
		}
		if p == &u8(C.MAP_FAILED) {
			return
		}
	}
	// Setup executable and guard pages
	x := unsafe { p + g_closure.v_page_size } // End of guard page
	mut remaining := g_closure.v_page_size / closure_size // Calculate slot count
	g_closure.closure_ptr = x // Current allocation pointer
	g_closure.closure_cap = remaining // Remaining slot count

	// Fill page with closure templates
	for remaining > 0 {
		unsafe { vmemcpy(x, closure_thunk.data, closure_thunk.len) } // Copy template
		remaining--
		unsafe {
			x += closure_size // Move to next slot
		}
	}
	$if freestanding {
		// No memory protection in freestanding mode
	} $else {
		// Make closure pages executable
		unsafe { C.mprotect(g_closure.closure_ptr, g_closure.v_page_size, C.PROT_READ | C.PROT_EXEC) }
	}
}

// closure_init initializes global closure subsystem(INTERNAL COMPILER USE ONLY).
fn closure_init() {
	// Determine system page size
	mut page_size := 0x4000
	$if !freestanding {
		// Query actual page size in OS environments
		page_size = unsafe { int(C.sysconf(C._SC_PAGESIZE)) }
	}
	// Calculate required allocation size
	page_size = page_size * (((assumed_page_size - 1) / page_size) + 1)
	g_closure.v_page_size = page_size // Store calculated size

	// Initialize thread-safety lock
	C.pthread_mutex_init(&g_closure.closure_mtx, 0)

	// Initial memory allocation
	closure_alloc()

	// Install closure handler template
	unsafe {
		// Temporarily enable write access to executable memory
		C.mprotect(g_closure.closure_ptr, page_size, C.PROT_READ | C.PROT_WRITE)
		// Copy closure entry stub code
		vmemcpy(g_closure.closure_ptr, closure_get_data_bytes.data, closure_get_data_bytes.len)
		// Re-enormalize execution protection
		C.mprotect(g_closure.closure_ptr, page_size, C.PROT_READ | C.PROT_EXEC)
	}
	// Setup global closure handler pointer
	g_closure.closure_get_data = g_closure.closure_ptr

	// Advance allocation pointer past header
	unsafe {
		g_closure.closure_ptr = &u8(g_closure.closure_ptr) + closure_size
	}
	g_closure.closure_cap-- // Account for header slot
}

// closure_create creates closure objects at compile-time(INTERNAL COMPILER USE ONLY).
@[direct_array_access]
fn closure_create(func voidptr, data voidptr) voidptr {
	$if !freestanding {
		// Lock thread-safety mutex in OS environments
		C.pthread_mutex_lock(&g_closure.closure_mtx)
	}

	// Handle memory exhaustion
	if g_closure.closure_cap == 0 {
		closure_alloc() // Allocate new memory page
	}
	g_closure.closure_cap-- // Decrement slot counter

	// Claim current closure slot
	mut closure := g_closure.closure_ptr
	unsafe {
		// Move to next available slot
		g_closure.closure_ptr = &u8(g_closure.closure_ptr) + closure_size

		// Write closure metadata (data + function pointer)
		mut p := &voidptr(&u8(closure) - assumed_page_size)
		p[0] = data // Stored closure context
		p[1] = func // Target function to execute
	}
	$if !freestanding {
		// Release lock in OS environments
		C.pthread_mutex_unlock(&g_closure.closure_mtx)
	}

	// Return executable closure object
	return closure
}
