@[has_globals]
module trace_calls

@[markused]
__global g_stack_base &u8

// These are set before _vinit; runtime initializers would reset the trace.
@[markused]
__global g_start_time u64

// Supported user-space TLS targets keep the formatter out of its own trace.
// Other freestanding targets do not provide a native TLS runtime.
$if !vinix && (!freestanding || ((linux || macos) && (amd64 || arm64))) {
	@[markused]
	__global g_trace_in_hook bool
}

fn enter_hook() bool {
	$if !vinix && (!freestanding || ((linux || macos) && (amd64 || arm64))) {
		if g_trace_in_hook { return false }
		g_trace_in_hook = true
	}
	return true
}

fn leave_hook() {
	$if !vinix && (!freestanding || ((linux || macos) && (amd64 || arm64))) {
		g_trace_in_hook = false
	}
}

@[markused]
pub fn on_call(fname string) {
	if !enter_hook() { return }
	defer { leave_hook() }
	// Formatting may call V helpers or fail on a closed stream. Trace output
	// must not change the native error observed by the traced operation.
	saved_errno := C.errno
	defer { C.errno = saved_errno }

	mut volatile pfbase := &u8(unsafe { nil })
	volatile fbase := u8(0)
	ns := current_time() - g_start_time
	mut ssize := u64(0)
	mut tid := u32(0)
	unsafe {
		$if windows {
			tid = C.GetCurrentThreadId()
		} $else $if freestanding && (linux || (macos && amd64)) && !native && !i386 {
			tid = u32(bf_gettid())
		} $else $if no_gettid ? {
			tid = u32(C.pthread_self())
		} $else $if linux && !musl ? {
			tid = C.gettid()
		} $else {
			tid = u32(C.pthread_self())
		}
		pfbase = &fbase
		ssize = u64(g_stack_base) - u64(pfbase)
	}
	$if freestanding && macos && amd64 && !native {
		write_raw_trace_row(tid, i64(ns), i64(ssize), fname)
	} $else $if x64 {
		C.fprintf(C.stderr, c'> trace %8u %8ld %8ld %s\n', tid, ns, ssize, fname.str)
	} $else {
		C.fprintf(C.stderr, c'> trace %8u %8lld %8lld %s\n', tid, ns, ssize, fname.str)
	}
	$if !(freestanding && macos && amd64 && !native) {
		C.fflush(C.stderr)
	}
}

@[inline]
fn current_time() u64 {
	unsafe {
		$if windows {
			tm := u64(0)
			C.QueryPerformanceCounter(&tm)
			return tm
		} $else {
			ts := C.timespec{}
			C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
			return u64(ts.tv_sec) * 1000000000 + u64(ts.tv_nsec)
		}
	}
}

fn initialize_trace_clock_and_header() {
	if !enter_hook() { return }
	defer { leave_hook() }
	saved_errno := C.errno
	defer { C.errno = saved_errno }
	g_start_time = current_time()
	//                    > trace  2128896   714640    28148 fn
	$if freestanding && macos && amd64 && !native {
		eprintln('#          tid       ns      ssize name')
	} $else {
		C.fprintf(C.stderr, c'#          tid       ns      ssize name\n')
		C.fflush(C.stderr)
	}
}

@[markused]
pub fn on_c_main(should_trace_c_main bool) {
	initialize_trace_clock_and_header()
	if should_trace_c_main {
		on_call('C.main')
	}
}
