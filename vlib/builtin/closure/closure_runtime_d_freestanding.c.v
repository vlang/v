@[has_globals]
module closure

import sync

#include "@VEXEROOT/vlib/builtin/closure/closure_freestanding.h"

fn C.v_closure_map(uint) voidptr
fn C.v_closure_protect(voidptr, uint, i32) i32
fn C.v_closure_page_size() uint
fn C.v_closure_flush_code(voidptr, uint)
fn C.v_closure_map_static_templates(uint) voidptr

$if linux && !native && (amd64 || arm64) {
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
