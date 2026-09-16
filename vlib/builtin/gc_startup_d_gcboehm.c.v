module builtin

fn C.GC_INIT()
fn C.GC_is_init_called() int
fn C.GC_set_find_leak(int)
fn C.GC_set_pages_executable(int)
fn C.GC_set_free_space_divisor(usize)
fn C.GC_allow_register_threads()
fn C.GC_enable_incremental()

// gc_runtime_init mirrors the Boehm startup sequence emitted by the V1 C backend.
// V3 selects the collector through compile-time defines, so keep the runtime setup
// in builtin where both the regular and shared-library startup paths can use it.
fn gc_runtime_init() {
	$if gcboehm_leak ? {
		C.GC_set_find_leak(1)
	}
	mut debugger_workaround := false
	$if linux {
		debugger_workaround = gc_prepare_for_debugger_init()
	}
	C.GC_set_pages_executable(0)
	$if gcboehm_opt ? {
		// Preserve an already-initialized host collector's process-wide tuning.
		// GC_INIT() below is a no-op in that case and cannot re-read the env var.
		if C.GC_is_init_called() == 0 {
			C.GC_set_free_space_divisor(1)
		}
	}
	C.GC_INIT()
	// V arrays keep an interior pointer one pointer-width past the allocation
	// header. Register that displacement so Boehm retains the allocation.
	C.GC_register_displacement(sizeof(voidptr))
	$if linux {
		gc_restore_roots_after_debugger_init(debugger_workaround)
	}
	$if gcboehm_incr ? {
		C.GC_enable_incremental()
	}
}
