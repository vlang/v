@[has_globals]
module closure

#include "@VEXEROOT/vlib/builtin/closure/closure_freestanding.h"

fn C.v_closure_map(uint) voidptr
fn C.v_closure_protect(voidptr, uint, i32) i32
fn C.v_closure_page_size() uint
fn C.v_closure_flush_code(voidptr, uint)
fn C.v_closure_map_static_templates(uint) voidptr

$if (linux && !native && (amd64 || arm64)) || (macos && amd64) {
	import sync

	@[cinit]
	__global g_closure_bare_once = sync.Mutex{}
	@[cinit]
	__global g_closure_bare_mutex = sync.Mutex{}
	@[cinit]
	__global g_closure_bare_once_done = false

	fn closure_bare_lock_init() {
		g_closure_bare_mutex.init()
	}

	fn closure_bare_lock() {
		g_closure_bare_mutex.lock()
	}

	fn closure_bare_unlock() {
		g_closure_bare_mutex.unlock()
	}

	fn closure_bare_init_once() {
		g_closure_bare_once.lock()
		if !g_closure_bare_once_done {
			closure_init_body()
			g_closure_bare_once_done = true
		}
		g_closure_bare_once.unlock()
	}
}

$if macos && amd64 {
	__global C.v_closure_static_templates u8

	fn closure_bare_map_static_templates() &u8 {
		page_size := u64(g_closure.v_page_size)
		if page_size != 16384 { panic('closure: unsupported signed template geometry') }
		base, allocation_status := bf_mach_vm_allocate(0, 2 * page_size, 1)
		if allocation_status != 0 { panic('closure: could not allocate trampoline metadata') }
		target := base + page_size
		source := unsafe { u64(&C.v_closure_static_templates) }
		// VM_FLAGS_FIXED | VM_FLAGS_OVERWRITE aliases compiler-signed text beside
		// RW metadata. The code never becomes writable or gains new permissions.
		code, protection, _, remap_status := bf_mach_vm_remap(source, page_size, target,
			i32(0x4000), false)
		if remap_status != 0 || code != target || (protection & 5) != 5 || (protection & 2) != 0 {
			bf_mach_vm_deallocate(base, 2 * page_size)
			panic('closure: could not map signed trampoline templates')
		}
		return unsafe { &u8(base) }
	}
}
