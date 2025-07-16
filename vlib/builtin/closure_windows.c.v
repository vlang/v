module builtin

struct ClosureMutex {
	closure_mtx C.SRWLOCK
}

// closure_alloc allocates executable memory pages for closures(INTERNAL COMPILER USE ONLY).
fn closure_alloc() {
	p := &u8(C.VirtualAlloc(0, g_closure.v_page_size * 2, C.MEM_COMMIT | C.MEM_RESERVE,
		C.PAGE_READWRITE))
	if isnil(p) {
		return
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

	// Make closure pages executable
	tmp := u32(0)
	_ := C.VirtualProtect(g_closure.closure_ptr, g_closure.v_page_size, C.PAGE_EXECUTE_READ,
		&tmp)
}

// closure_init initializes global closure subsystem(INTERNAL COMPILER USE ONLY).
fn closure_init() {
	// Determine system page size
	mut si := C.SYSTEM_INFO{}
	C.GetNativeSystemInfo(&si)

	// Calculate required allocation size
	page_size := int(si.dwPageSize) * (((assumed_page_size - 1) / int(si.dwPageSize)) + 1)
	g_closure.v_page_size = page_size // Store calculated size

	// Initialize thread-safety lock
	C.InitializeSRWLock(&g_closure.closure_mtx)

	// Initial memory allocation
	closure_alloc()

	// Install closure handler template

	// Temporarily enable write access to executable memory
	tmp := u32(0)
	_ := C.VirtualProtect(g_closure.closure_ptr, page_size, C.PAGE_READWRITE, &tmp)
	// Copy closure entry stub code
	unsafe { vmemcpy(g_closure.closure_ptr, closure_get_data_bytes.data, closure_get_data_bytes.len) }
	// Re-enormalize execution protection
	_ := C.VirtualProtect(g_closure.closure_ptr, page_size, C.PAGE_EXECUTE_READ, &tmp)

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
	C.AcquireSRWLockExclusive(&g_closure.closure_mtx)

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
	C.ReleaseSRWLockExclusive(&g_closure.closure_mtx)

	// Return executable closure object
	return closure
}
