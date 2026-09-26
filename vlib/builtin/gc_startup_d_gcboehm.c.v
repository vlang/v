module builtin

fn C.GC_INIT()
fn C.GC_is_init_called() int
fn C.GC_set_find_leak(int)
fn C.GC_set_all_interior_pointers(int)
fn C.GC_set_pages_executable(int)
fn C.GC_set_free_space_divisor(usize)
fn C.GC_enable_incremental()

// v3_gcboehm_runtime_init mirrors the Boehm startup sequence emitted by the V1 C backend.
// V3 selects the collector through compile-time defines, so perform the runtime setup
// from builtin before the rest of the V3 module initializers run.
fn v3_gcboehm_runtime_init() {
	$if gcboehm_leak ? {
		C.GC_set_find_leak(1)
	}
	debugger_workaround := gc_prepare_for_debugger_init()
	C.GC_set_pages_executable(0)
	host_initialized_gc := C.GC_is_init_called() != 0
	if !host_initialized_gc {
		// V reaches array data through pointers into the middle of a block (past
		// the array header, and anywhere for slices), including from heap
		// objects, so Boehm must recognise interior pointers. Every libgc V builds
		// from source enables this; the prebuilt one linked on Windows with tcc
		// does not, and it then frees blocks that live arrays still use.
		C.GC_set_all_interior_pointers(1)
	}
	$if gcboehm_opt ? {
		// Preserve an already-initialized host collector's process-wide tuning.
		// GC_INIT() below is a no-op in that case and cannot re-read the env var.
		if C.GC_is_init_called() == 0 {
			C.GC_set_free_space_divisor(1)
		}
	}
	C.GC_INIT()
	// V arrays keep an interior pointer one pointer-width past the allocation
	// header. Register that displacement so Boehm retains the allocation even
	// when interior pointers are off (a host that initialized the collector
	// first). Use the macro: under `-gc boehm_leak` (GC_DEBUG) every object also
	// starts after Boehm's debug header, and only GC_REGISTER_DISPLACEMENT
	// registers the offset that header adds.
	C.GC_REGISTER_DISPLACEMENT(sizeof(voidptr))
	$if windows {
		// Leave a host collector's abort handler alone. Installed after GC_INIT,
		// because the setter takes Boehm's allocator lock.
		if !host_initialized_gc {
			gc_report_fatal_errors_on_stderr()
		}
	}
	gc_restore_roots_after_debugger_init(debugger_workaround)
	$if gcboehm_incr ? {
		C.GC_enable_incremental()
	}
}
