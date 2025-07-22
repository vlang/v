@[has_globals]
module closure

// Inspired from Chris Wellons's work
// https://nullprogram.com/blog/2017/01/08/

const assumed_page_size = int(0x4000)

@[heap]
struct Closure {
	ClosureMutex
mut:
	closure_ptr      voidptr
	closure_get_data fn () voidptr = unsafe { nil }
	closure_cap      int
	v_page_size      int = int(0x4000)
}

__global g_closure = Closure{}

enum MemoryProtectAtrr {
	read_exec
	read_write
}

// refer to https://godbolt.org/z/r7P3EYv6c for a complete assembly
// vfmt off
pub const closure_thunk = $if amd64 {
    [
    u8(0xF3), 0x44, 0x0F, 0x7E, 0x3D, 0xF7, 0xBF, 0xFF, 0xFF,  // movq  xmm15, QWORD PTR [rip - userdata]
        0xFF, 0x25, 0xF9, 0xBF, 0xFF, 0xFF                     // jmp  QWORD PTR [rip - fn]
    ]
} $else $if i386 {
    [
    u8(0xe8), 0x00, 0x00, 0x00, 0x00,        // call here
        // here:
        0x59,                                // pop  ecx
        0x66, 0x0F, 0x6E, 0xF9,              // movd xmm7, ecx
        0xff, 0xA1, 0xff, 0xbf, 0xff, 0xff,  // jmp  DWORD PTR [ecx - 0x4001] # <fn>
    ]
} $else $if arm64 {
    [
    u8(0x11), 0x00, 0xFE, 0x5C,  // ldr d17, userdata
        0x30, 0x00, 0xFE, 0x58,  // ldr x16, fn
        0x00, 0x02, 0x1F, 0xD6   // br  x16
    ]
} $else $if arm32 {
    [
    u8(0x04), 0xC0, 0x4F, 0xE2,  // adr ip, here
        // here:
        0x01, 0xC9, 0x4C, 0xE2,  // sub  ip, ip, #0x4000
        0x90, 0xCA, 0x07, 0xEE,  // vmov s15, ip
        0x00, 0xC0, 0x9C, 0xE5,  // ldr  ip, [ip, 0]
        0x1C, 0xFF, 0x2F, 0xE1   // bx   ip
    ]
} $else $if rv64 {
    [
    u8(0x97), 0xCF, 0xFF, 0xFF,  // auipc t6, 0xffffc
        0x03, 0xBF, 0x8F, 0x00,  // ld    t5, 8(t6)
        0x07, 0xB3, 0x0F, 0x00,  // fld   ft6, 0(t6)
        0x67, 0x00, 0x0F, 0x00,  // jr    t5
    ]
} $else $if rv32 {
    [
    u8(0x97), 0xCF, 0xFF, 0xFF,  // auipc t6, 0xffffc
        0x03, 0xAF, 0x4F, 0x00,  // lw    t5, 4(t6)
        0x07, 0xAB, 0x0F, 0x00,  // flw   fs6, 0(t6)
        0x67, 0x00, 0x0F, 0x00   // jr    t5
    ]
} $else $if s390x {
    [
    u8(0xC0), 0x70, 0xFF, 0xFF, 0xE0, 0x00,  // larl %r7, -16384
        0x68, 0xF0, 0x70, 0x00,              // ld   %f15, 0(%r7)
        0xE3, 0x70, 0x70, 0x08, 0x00, 0x04,  // lg   %r7, 8(%r7)
        0x07, 0xF7,                          // br   %r7
    ]
} $else $if ppc64le {
    [
    u8(0xa6), 0x02, 0x08, 0x7c,	// mflr   %r0
        0x05, 0x00, 0x00, 0x48,	// bl     here
        0xa6, 0x02, 0xc8, 0x7d,	// here:  mflr %r14
        0xf8, 0xbf, 0xce, 0x39,	// addi   %r14, %r14, -16392
        0x00, 0x00, 0xce, 0xc9,	// lfd    %f14, 0(%r14)
        0x08, 0x00, 0xce, 0xe9,	// ld     %r14, 8(%r14)
        0xa6, 0x03, 0x08, 0x7c,	// mtlr   %r0
        0xa6, 0x03, 0xc9, 0x7d,	// mtctr  %r14
        0x20, 0x04, 0x80, 0x4e,	// bctr
    ]
} $else $if loongarch64 {
    [
    u8(0x92), 0xFF, 0xFF, 0x1D,  // pcaddu12i t6, -4
        0x48, 0x02, 0x80, 0x2B,  // fld.d     f8, t6, 0
        0x51, 0x22, 0xC0, 0x28,  // ld.d      t5, t6, 8
        0x20, 0x02, 0x00, 0x4C,  // jr        t5
    ]
} $else {
    []u8{}
}

const closure_get_data_bytes = $if amd64 {
    [
    u8(0x66), 0x4C, 0x0F, 0x7E, 0xF8,  // movq rax, xmm15
        0xC3                           // ret
    ]
} $else $if i386 {
    [
    u8(0x66), 0x0F, 0x7E, 0xF8,              // movd eax, xmm7
        0x8B, 0x80, 0xFB, 0xBF, 0xFF, 0xFF,  // mov eax, DWORD PTR [eax - 0x4005]
        0xc3                                 // ret
    ]
} $else $if arm64 {
    [
    u8(0x20), 0x02, 0x66, 0x9E,  // fmov x0, d17
        0xC0, 0x03, 0x5F, 0xD6   // ret
    ]
} $else $if arm32 {
    [
    u8(0x90), 0x0A, 0x17, 0xEE,  // vmov r0, s15
        0x04, 0x00, 0x10, 0xE5,  // ldr r0, [r0, #-4]
        0x1E, 0xFF, 0x2F, 0xE1   // bx lr
    ]
} $else $if rv64 {
    [
    u8(0x53), 0x05, 0x03, 0xE2,  // fmv.x.d a0, ft6
        0x67, 0x80, 0x00, 0x00,  // ret
    ]
} $else $if rv32 {
    [
    u8(0x53), 0x05, 0x0B, 0xE0,  // fmv.x.w a0, fs6
        0x67, 0x80, 0x00, 0x00   // ret
    ]
} $else $if s390x {
	[
    u8(0xB3), 0xCD, 0x00, 0x2F,	 // lgdr %r2, %f15
        0x07, 0xFE,			     // br   %r14
    ]
} $else $if ppc64le {
    [
    u8(0x66), 0x00, 0xc3, 0x7d,	// mfvsrd %r3, %f14
        0x20, 0x00, 0x80, 0x4e,	// blr
    ]
} $else $if loongarch64 {
    [
    u8(0x04), 0xB9, 0x14, 0x01,  // movfr2gr.d a0, f8
        0x20, 0x00, 0x00, 0x4C,  // ret
    ]
} $else {
    []u8{}
}
// vfmt on
fn get_closure_size() int {
	mut new_closure_size := if 2 * u32(sizeof(voidptr)) > u32(closure_thunk.len) {
		2 * u32(sizeof(voidptr))
	} else {
		u32(closure_thunk.len) + u32(sizeof(voidptr)) - 1
	}
	new_closure_size = new_closure_size & ~(u32(sizeof(voidptr)) - 1)
	return int(new_closure_size)
}

// closure_alloc allocates executable memory pages for closures(INTERNAL COMPILER USE ONLY).
fn closure_alloc() {
	p := closure_alloc_platform()
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
		unsafe { vmemcpy(x, &closure_thunk[0], closure_thunk.len) } // Copy template
		remaining--
		unsafe {
			x += closure_size // Move to next slot
		}
	}
	closure_memory_protect_platform(g_closure.closure_ptr, g_closure.v_page_size, .read_exec)
}

// closure_init initializes global closure subsystem(INTERNAL COMPILER USE ONLY).
fn closure_init() {
	// Determine system page size
	mut page_size := get_page_size_platform()
	g_closure.v_page_size = page_size // Store calculated size

	// Initialize thread-safety lock
	closure_mtx_lock_init_platform()

	// Initial memory allocation
	closure_alloc()

	// Install closure handler template
	unsafe {
		// Temporarily enable write access to executable memory
		closure_memory_protect_platform(g_closure.closure_ptr, page_size, .read_write)
		// Copy closure entry stub code
		vmemcpy(g_closure.closure_ptr, &closure_get_data_bytes[0], closure_get_data_bytes.len)
		// Re-enormalize execution protection
		closure_memory_protect_platform(g_closure.closure_ptr, page_size, .read_exec)
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
	closure_mtx_lock_platform()

	// Handle memory exhaustion
	if g_closure.closure_cap == 0 {
		closure_alloc() // Allocate new memory page
	}
	g_closure.closure_cap-- // Decrement slot counter

	// Claim current closure slot
	mut curr_closure := g_closure.closure_ptr
	unsafe {
		// Move to next available slot
		g_closure.closure_ptr = &u8(g_closure.closure_ptr) + closure_size

		// Write closure metadata (data + function pointer)
		mut p := &voidptr(&u8(curr_closure) - assumed_page_size)
		p[0] = data // Stored closure context
		p[1] = func // Target function to execute
	}
	closure_mtx_unlock_platform()

	// Return executable closure object
	return curr_closure
}

// equal to `max(2*sizeof(void*), sizeof(__closure_thunk))`, rounded up to the next multiple of `sizeof(void*)`
const closure_size = get_closure_size()
