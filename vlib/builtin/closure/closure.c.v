@[has_globals]
module closure

// Inspired from Chris Wellons's work
// https://nullprogram.com/blog/2017/01/08/

const assumed_page_size = int(0x4000)
const ppc64_architecture = int(11)

type ClosureGetDataFn = fn () voidptr

struct ClosurePage {
mut:
	next            &ClosurePage = unsafe { nil }
	exec_page_start voidptr
}

@[heap]
struct Closure {
	ClosureMutex
mut:
	closure_ptr      voidptr
	closure_get_data ClosureGetDataFn = unsafe { nil }
	closure_cap      int
	free_closure_ptr voidptr
	pages            &ClosurePage = unsafe { nil }
	v_page_size      int          = int(0x4000)
}

__global g_closure = Closure{}

enum MemoryProtectAtrr {
	read_exec
	read_write
}

// Keep this runtime check bootstrap-compatible. Older compilers can not parse `$if ppc64` yet.
@[inline]
fn is_ppc64() bool {
	$if big_endian {
		return C.__V_architecture == ppc64_architecture
	} $else {
		return false
	}
}

// refer to https://godbolt.org/z/r7P3EYv6c for a complete assembly
//
// NOTE: Keep the first branch as the longest byte sequence. In translated/bootstrap C mode
// (`vc/v.c`), V emits a fixed C array whose size is inferred from the first branch.
// The final `big_endian` branch maps to ppc64 here, since the supported big-endian
// closure targets handled above are s390x and sparc64.
// vfmt off
pub const closure_thunk = $if ppc64le {
    [
    u8(0xa6), 0x02, 0x08, 0x7c,	// mflr   %r0
        0x05, 0x00, 0x00, 0x48,	// bl     here
        0xa6, 0x02, 0xc8, 0x7d,	// here:  mflr %r14
        0xf8, 0xbf, 0xce, 0x39,	// addi   %r14, %r14, -16392
        0x00, 0x00, 0xce, 0xc9,	// lfd    %f14, 0(%r14)
        0x08, 0x00, 0xce, 0xe9,	// ld     %r14, 8(%r14)
        0x78, 0x73, 0xcc, 0x7d,	// mr     %r12, %r14
        0xa6, 0x03, 0x08, 0x7c,	// mtlr   %r0
        0xa6, 0x03, 0xc9, 0x7d,	// mtctr  %r14
        0x20, 0x04, 0x80, 0x4e,	// bctr
    ]!
} $else $if !ppc64le && !amd64 && !i386 && !arm64 && !arm32 && !rv64 && !rv32 && !s390x && !loongarch64 {
    // ppc (32-bit PowerPC) - expressed as negation of all other arches for bootstrap compat
    [
    u8(0x7c), 0x08, 0x02, 0xa6,	// mflr   %r0
        0x48, 0x00, 0x00, 0x05,	// bl     here
        0x7d, 0x88, 0x02, 0xa6,	// here:  mflr %r12
        0x39, 0x8c, 0xbf, 0xf8,	// addi   %r12, %r12, -16392
        0xc9, 0xcc, 0x00, 0x00,	// lfd    %f14, 0(%r12)
        0x81, 0x8c, 0x00, 0x04,	// lwz    %r12, 4(%r12)
        0x7c, 0x08, 0x03, 0xa6,	// mtlr   %r0
        0x7d, 0x89, 0x03, 0xa6,	// mtctr  %r12
        0x4e, 0x80, 0x04, 0x20,	// bctr
    ]!
} $else $if amd64 {
    [
    u8(0xF3), 0x44, 0x0F, 0x7E, 0x3D, 0xF7, 0xBF, 0xFF, 0xFF,  // movq  xmm15, QWORD PTR [rip - userdata]
        0xFF, 0x25, 0xF9, 0xBF, 0xFF, 0xFF                     // jmp  QWORD PTR [rip - fn]
    ]!
} $else $if i386 {
    [
    u8(0xe8), 0x00, 0x00, 0x00, 0x00,        // call here
        // here:
        0x59,                                // pop  ecx
        0x66, 0x0F, 0x6E, 0xF9,              // movd xmm7, ecx
        0xff, 0xA1, 0xff, 0xbf, 0xff, 0xff,  // jmp  DWORD PTR [ecx - 0x4001] # <fn>
    ]!
} $else $if arm64 {
    [
    u8(0x11), 0x00, 0xFE, 0x5C,  // ldr d17, userdata
        0x30, 0x00, 0xFE, 0x58,  // ldr x16, fn
        0x00, 0x02, 0x1F, 0xD6   // br  x16
    ]!
} $else $if arm32 {
    [
    u8(0x04), 0xC0, 0x4F, 0xE2,  // adr ip, here
        // here:
        0x01, 0xC9, 0x4C, 0xE2,  // sub  ip, ip, #0x4000
        0x90, 0xCA, 0x07, 0xEE,  // vmov s15, ip
        0x00, 0xC0, 0x9C, 0xE5,  // ldr  ip, [ip, 0]
        0x1C, 0xFF, 0x2F, 0xE1   // bx   ip
    ]!
} $else $if rv64 {
    [
    u8(0x97), 0xCF, 0xFF, 0xFF,  // auipc t6, 0xffffc
        0x03, 0xBF, 0x8F, 0x00,  // ld    t5, 8(t6)
        0x07, 0xB3, 0x0F, 0x00,  // fld   ft6, 0(t6)
        0x67, 0x00, 0x0F, 0x00,  // jr    t5
    ]!
} $else $if rv32 {
    [
    u8(0x97), 0xCF, 0xFF, 0xFF,  // auipc t6, 0xffffc
        0x03, 0xAF, 0x4F, 0x00,  // lw    t5, 4(t6)
        0x07, 0xAB, 0x0F, 0x00,  // flw   fs6, 0(t6)
        0x67, 0x00, 0x0F, 0x00   // jr    t5
    ]!
} $else $if s390x {
    [
    u8(0xC0), 0x10, 0xFF, 0xFF, 0xE0, 0x00,  // larl %r1, -16384
        0x68, 0xF0, 0x10, 0x00,              // ld   %f15, 0(%r1)
        0xE3, 0x10, 0x10, 0x08, 0x00, 0x04,  // lg   %r1, 8(%r1)
        0x07, 0xF1,                          // br   %r1
    ]!
} $else $if loongarch64 {
    [
    u8(0x92), 0xFF, 0xFF, 0x1D,  // pcaddu12i t6, -4
        0x48, 0x02, 0x80, 0x2B,  // fld.d     f8, t6, 0
        0x51, 0x22, 0xC0, 0x28,  // ld.d      t5, t6, 8
        0x20, 0x02, 0x00, 0x4C,  // jr        t5
    ]!
} $else $if sparc64 {
    [
    u8(0x83), 0x41, 0x40, 0x00,  // rd  %pc, %g1
        0x05, 0x00, 0x00, 0x10,  // sethi  %hi(0x4000), %g2
        0x84, 0x10, 0xa0, 0x00,  // mov  %g2, %g2   ! 4000 <main>
        0x82, 0x20, 0x40, 0x02,  // sub  %g1, %g2, %g1
        0xff, 0x18, 0x60, 0x00,  // ldd  [ %l1 ], %d62
        0xc2, 0x58, 0x60, 0x08,  // ldx  [ %g1 + 8 ], %g1
        0x81, 0xc0, 0x40, 0x00,  // jmp  %g1
        0x01, 0x00, 0x00, 0x00   // nop
    ]!
} $else $if big_endian {
    [
    u8(0x7C), 0x08, 0x02, 0xA6,  // mflr   %r0
        0x48, 0x00, 0x00, 0x05,  // bl     here
        0x7D, 0xC8, 0x02, 0xA6,  // here:  mflr %r14
        0x39, 0xCE, 0xC0, 0x08,  // addi   %r14, %r14, -16376
        0xC9, 0xCE, 0x00, 0x00,  // lfd    %f14, 0(%r14)      // userdata
        0xE9, 0xCE, 0x00, 0x08,  // ld     %r14, 8(%r14)      // func descriptor ptr
        0xE9, 0x8E, 0x00, 0x00,  // ld     %r12, 0(%r14)      // code addr from descriptor
        0xE8, 0x4E, 0x00, 0x08,  // ld     %r2,  8(%r14)      // TOC from descriptor
        0x7C, 0x08, 0x03, 0xA6,  // mtlr   %r0
        0x7D, 0x89, 0x03, 0xA6,  // mtctr  %r12
        0x4E, 0x80, 0x04, 0x20,  // bctr
    ]!
} $else {
    [u8(0)]!
}

// NOTE: Keep the first branch as the longest byte sequence. In translated/bootstrap C mode
// (`vc/v.c`), V emits a fixed C array whose size is inferred from the first branch.
const closure_get_data_bytes = $if !ppc64le && !amd64 && !i386 && !arm64 && !arm32 && !rv64 && !rv32 && !s390x && !loongarch64 {
    // ppc (32-bit PowerPC) - expressed as negation of all other arches for bootstrap compat
    [
    u8(0x94), 0x21, 0xff, 0xf0,	// stwu   %r1, -16(%r1)
        0xd9, 0xc1, 0x00, 0x08,	// stfd   %f14, 8(%r1)
        0x80, 0x61, 0x00, 0x08,	// lwz    %r3, 8(%r1)
        0x38, 0x21, 0x00, 0x10,	// addi   %r1, %r1, 16
        0x4e, 0x80, 0x00, 0x20,	// blr
    ]!
} $else $if arm32 {
    [
    u8(0x90), 0x0A, 0x17, 0xEE,  // vmov r0, s15
        0x04, 0x00, 0x10, 0xE5,  // ldr r0, [r0, #-4]
        0x1E, 0xFF, 0x2F, 0xE1   // bx lr
    ]!
} $else $if amd64 {
    [
    u8(0x66), 0x4C, 0x0F, 0x7E, 0xF8,  // movq rax, xmm15
        0xC3                           // ret
    ]!
} $else $if i386 {
    [
    u8(0x66), 0x0F, 0x7E, 0xF8,              // movd eax, xmm7
        0x8B, 0x80, 0xFB, 0xBF, 0xFF, 0xFF,  // mov eax, DWORD PTR [eax - 0x4005]
        0xc3                                 // ret
    ]!
} $else $if arm64 {
    [
    u8(0x20), 0x02, 0x66, 0x9E,  // fmov x0, d17
        0xC0, 0x03, 0x5F, 0xD6   // ret
    ]!
} $else $if rv64 {
    [
    u8(0x53), 0x05, 0x03, 0xE2,  // fmv.x.d a0, ft6
        0x67, 0x80, 0x00, 0x00,  // ret
    ]!
} $else $if rv32 {
    [
    u8(0x53), 0x05, 0x0B, 0xE0,  // fmv.x.w a0, fs6
        0x67, 0x80, 0x00, 0x00   // ret
    ]!
} $else $if s390x {
	[
    u8(0xB3), 0xCD, 0x00, 0x2F,	 // lgdr %r2, %f15
        0x07, 0xFE,			     // br   %r14
    ]!
} $else $if ppc64le {
    [
    u8(0x66), 0x00, 0xc3, 0x7d,	// mfvsrd %r3, %f14
        0x20, 0x00, 0x80, 0x4e,	// blr
    ]!
} $else $if loongarch64 {
    [
    u8(0x04), 0xB9, 0x14, 0x01,  // movfr2gr.d a0, f8
        0x20, 0x00, 0x00, 0x4C,  // ret
    ]!
} $else $if sparc64 {
    [
    u8(0x91), 0xb0, 0x22, 0x1f,  // movdtox %f62, %o0
        0x81, 0xc3, 0xe0, 0x08,  // retl
        0x01, 0x00, 0x00, 0x00   // nop
    ]!
} $else $if big_endian {
    [
    u8(0x7d), 0xc3, 0x00, 0x66,  // mfvsrd %r3, %f14
        0x4e, 0x80, 0x00, 0x20   // blr
    ]!
} $else {
    [u8(0)]!
}

// vfmt on

// equal to `max(2*sizeof(void*), sizeof(__closure_thunk))`, rounded up to the next multiple of `sizeof(void*)`
// NOTE: This is a workaround for `-usecache` bug, as it can't include `fn get_closure_size()` needed by `const closure_size` in `build-module` mode.
const closure_size_1 = if 2 * u32(sizeof(voidptr)) > u32(closure_thunk.len) {
	2 * u32(sizeof(voidptr))
} else {
	u32(closure_thunk.len) + u32(sizeof(voidptr)) - 1
}
const closure_size = int(closure_size_1 & ~(u32(sizeof(voidptr)) - 1))

@[inline]
fn closure_exec_ptr(closure voidptr) voidptr {
	if is_ppc64() {
		return unsafe { &u8(closure) + assumed_page_size }
	}
	return closure
}

@[inline]
fn closure_return_ptr(exec_ptr voidptr) voidptr {
	if is_ppc64() {
		return unsafe { &u8(exec_ptr) - assumed_page_size }
	}
	return exec_ptr
}

@[inline]
fn closure_slot_meta(exec_ptr voidptr) &voidptr {
	return unsafe { &voidptr(&u8(exec_ptr) - assumed_page_size) }
}

fn closure_register_page(exec_page_start voidptr) {
	unsafe {
		node := &ClosurePage(malloc(sizeof(ClosurePage)))
		*node = ClosurePage{
			next:            g_closure.pages
			exec_page_start: exec_page_start
		}
		g_closure.pages = node
	}
}

fn closure_is_managed(exec_ptr voidptr) bool {
	if isnil(exec_ptr) {
		return false
	}
	exec_addr := unsafe { usize(exec_ptr) }
	mut page := g_closure.pages
	for page != unsafe { nil } {
		page_addr := unsafe { usize(page.exec_page_start) }
		if exec_addr >= page_addr && exec_addr < page_addr + usize(g_closure.v_page_size) {
			slot_offset := exec_addr - page_addr
			return slot_offset >= usize(closure_size) && slot_offset % usize(closure_size) == 0
		}
		page = page.next
	}
	return false
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
	closure_register_page(x)
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
		// Re-normalize execution protection
		closure_memory_protect_platform(g_closure.closure_ptr, page_size, .read_exec)
	}
	// Setup global closure handler pointer
	if is_ppc64() {
		mut desc := unsafe { &voidptr(&u8(g_closure.closure_ptr) - assumed_page_size) }
		unsafe {
			desc[0] = g_closure.closure_ptr
			desc[1] = nil
		}
		g_closure.closure_get_data = unsafe { ClosureGetDataFn(desc) }
	} else {
		g_closure.closure_get_data = g_closure.closure_ptr
	}

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

	mut curr_closure := g_closure.free_closure_ptr
	if !isnil(curr_closure) {
		unsafe {
			mut p := closure_slot_meta(curr_closure)
			g_closure.free_closure_ptr = p[0]
		}
	} else {
		// Handle memory exhaustion
		if g_closure.closure_cap == 0 {
			closure_alloc() // Allocate new memory page
		}
		g_closure.closure_cap-- // Decrement slot counter

		// Claim current closure slot
		curr_closure = g_closure.closure_ptr
		unsafe {
			// Move to next available slot
			g_closure.closure_ptr = &u8(g_closure.closure_ptr) + closure_size
		}
	}
	unsafe {
		// Write closure metadata (data + function pointer)
		mut p := closure_slot_meta(curr_closure)
		if is_ppc64() {
			// ELFv1: guard page layout per slot:
			//   [0] desc[0] = thunk code address  <- returned as ELFv1 function pointer
			//   [1] desc[1] = nil (TOC unused; thunk loads real TOC from func descriptor)
			//   [2] userdata
			//   [3] func (V function descriptor pointer into .opd)
			p[0] = curr_closure
			p[1] = nil
			p[2] = data
			p[3] = func
		} else {
			p[0] = data // Stored closure context
			p[1] = func // Target function to execute
		}
	}
	closure_mtx_unlock_platform()

	// Return executable closure object
	return closure_return_ptr(curr_closure)
}

// closure_data returns the userdata pointer associated with a closure object.
@[direct_array_access]
fn closure_data(closure voidptr) voidptr {
	unsafe {
		mut p := closure_slot_meta(closure_exec_ptr(closure))
		$if ppc64 {
			return p[2]
		} $else {
			return p[0]
		}
	}
}

// closure_try_destroy frees a managed closure slot and its context when the closure is known to be temporary.
@[direct_array_access]
fn closure_try_destroy(closure voidptr) {
	if isnil(closure) {
		return
	}
	exec_ptr := closure_exec_ptr(closure)
	closure_mtx_lock_platform()
	if !closure_is_managed(exec_ptr) {
		closure_mtx_unlock_platform()
		return
	}
	unsafe {
		mut p := closure_slot_meta(exec_ptr)
		mut data := nil
		if is_ppc64() {
			data = p[2]
		} else {
			data = p[0]
		}
		if !isnil(data) {
			free(data)
		}
		p[0] = g_closure.free_closure_ptr
		if is_ppc64() {
			p[1] = nil
			p[2] = nil
			p[3] = nil
		} else {
			p[1] = nil
		}
		g_closure.free_closure_ptr = exec_ptr
	}
	closure_mtx_unlock_platform()
}
