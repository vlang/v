@[has_globals]
module builtin

$if amd64 && !native && !v_bare_no_startup ? {
	#include <stdlib.h>

	struct BareMacExitEntry {
	mut:
		callback FnExitCb
		next     &BareMacExitEntry = unsafe { nil }
		dynamic  bool
	}

	// The first 32 registrations, including compiler cleanup hooks, need no heap.
	// Overflow uses the persistent bare heap, never a caller's scoped arena.
	__global g_bare_macos_exit_inline [32]BareMacExitEntry
	__global g_bare_macos_exit_inline_used uint
	__global g_bare_macos_exit_head &BareMacExitEntry
	__global g_bare_macos_exit_registry_lock i32
	__global g_bare_macos_exit_registry_closed bool

	fn C.atexit(callback FnExitCb) i32

	@[no_profile; no_trace]
	fn bare_macos_exit_lock() {
		for C.v_prealloc_atomic_cas_i32(&g_bare_macos_exit_registry_lock, 0, 1) == 0 {
		}
	}

	@[no_profile; no_trace]
	fn bare_macos_exit_unlock() {
		C.v_prealloc_atomic_store_i32(&g_bare_macos_exit_registry_lock, 0)
	}

	@[export: 'atexit'; no_profile; no_trace]
	fn bare_macos_atexit(callback FnExitCb) i32 {
		if callback == unsafe { nil } { return -1 }
		bare_macos_exit_lock()
		if g_bare_macos_exit_registry_closed {
			bare_macos_exit_unlock()
			return -1
		}
		if g_bare_macos_exit_inline_used < g_bare_macos_exit_inline.len {
			// The registry lock owns both the index and each unpublished entry.
			unsafe {
				mut entry := &g_bare_macos_exit_inline[g_bare_macos_exit_inline_used]
				g_bare_macos_exit_inline_used++
				entry.callback = callback
				entry.next = g_bare_macos_exit_head
				entry.dynamic = false
				g_bare_macos_exit_head = entry
			}
			bare_macos_exit_unlock()
			return 0
		}
		bare_macos_exit_unlock()

		// Allocation and user callbacks never run while the registry is locked.
		mut entry := unsafe { &BareMacExitEntry(__malloc(sizeof(BareMacExitEntry))) }
		if entry == unsafe { nil } { return -1 }
		bare_macos_exit_lock()
		if g_bare_macos_exit_registry_closed {
			bare_macos_exit_unlock()
			unsafe { __free(entry) }
			return -1
		}
		entry.callback = callback
		entry.next = g_bare_macos_exit_head
		entry.dynamic = true
		g_bare_macos_exit_head = entry
		bare_macos_exit_unlock()
		return 0
	}

	@[no_profile; no_trace]
	fn bare_macos_drain_atexit(close_registry bool) {
		for {
			bare_macos_exit_lock()
			mut entry := g_bare_macos_exit_head
			if entry == unsafe { nil } {
				if close_registry { g_bare_macos_exit_registry_closed = true }
				bare_macos_exit_unlock()
				return
			}
			g_bare_macos_exit_head = entry.next
			callback := entry.callback
			dynamic := entry.dynamic
			bare_macos_exit_unlock()
			// Release before calling user code: callbacks may register another
			// callback, terminate immediately, or re-enter normal process exit.
			if dynamic { unsafe { __free(entry) } }
			callback()
		}
	}
}
