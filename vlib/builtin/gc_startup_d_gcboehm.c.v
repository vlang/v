@[has_globals]
module builtin

fn C.GC_INIT()
fn C.GC_is_init_called() int
fn C.GC_set_find_leak(int)
fn C.GC_set_pages_executable(int)
fn C.GC_set_free_space_divisor(usize)
fn C.GC_enable_incremental()

// g_gc_default_abort_func is Boehm's own fatal error handler, which
// gc_abort_without_message_box replaces on Windows.
__global g_gc_default_abort_func voidptr

// v3_gcboehm_runtime_init mirrors the Boehm startup sequence emitted by the V1 C backend.
// V3 selects the collector through compile-time defines, so perform the runtime setup
// from builtin before the rest of the V3 module initializers run.
fn v3_gcboehm_runtime_init() {
	$if gcboehm_leak ? {
		C.GC_set_find_leak(1)
	}
	debugger_workaround := gc_prepare_for_debugger_init()
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
	// With `-gc boehm_leak` (`GC_DEBUG`), objects also start with Boehm's debug
	// header, so the macro then registers the offset past that header too.
	// Without it, a libgc built without `ALL_INTERIOR_POINTERS` (like the bundled
	// Windows tcc one) treats live array buffers as leaks, and frees them.
	C.GC_REGISTER_DISPLACEMENT(sizeof(voidptr))
	$if windows {
		g_gc_default_abort_func = C.v_gc_get_abort_func()
		C.v_gc_set_abort_func(gc_abort_without_message_box)
	}
	gc_restore_roots_after_debugger_init(debugger_workaround)
	$if gcboehm_incr ? {
		C.GC_enable_incremental()
	}
}

// gc_abort_without_message_box is Boehm's fatal error handler on Windows.
// The default one shows a modal "Fatal error in GC" message box there, and waits
// for a click, which hangs unattended runs, like `v test` or CI jobs.
// This one prints the message to stderr, and exits with a non-zero code instead.
fn gc_abort_without_message_box(msg &char) {
	$if windows {
		if msg == unsafe { nil } {
			// Boehm calls it right before `exit(1)`; the default shows no message box then.
			default_abort := unsafe { FnGC_AbortCB(g_gc_default_abort_func) }
			default_abort(msg)
			return
		}
		// Write without allocating, and without the CRT stdio locks: the heap may be
		// corrupted, and stopped threads may hold those locks.
		prefix := 'Fatal error in GC: '
		newline := '\n'
		write_buf_to_fd_kernel32(2, prefix.str, prefix.len)
		write_buf_to_fd_kernel32(2, &u8(msg), vstrlen_char(msg))
		write_buf_to_fd_kernel32(2, newline.str, newline.len)
		if C.IsDebuggerPresent() {
			// Return, so that Boehm's `DebugBreak()` stops in the attached debugger.
			return
		}
		$if tinyc {
			// Keep the backtrace that tcc's own handler printed for Boehm's `DebugBreak()`.
			print_backtrace()
		}
		// Like Boehm's own abort in builds without debugging, skip the at-exit handlers.
		C._exit(1)
	}
}
