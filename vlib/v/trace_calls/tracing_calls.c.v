@[has_globals]
module trace_calls

@[markused]
__global g_stack_base = &u8(unsafe { nil })
__global g_start_time = u64(0)

@[markused]
pub fn on_call(fname string) {
	mut volatile pfbase := &u8(unsafe { nil })
	volatile fbase := u8(0)
	ns := current_time() - g_start_time
	mut ssize := u64(0)
	mut tid := u32(0)
	unsafe {
		$if windows {
			tid = C.GetCurrentThreadId()
		} $else $if linux {
			tid = C.gettid()
		} $else {
			tid = u32(C.pthread_self())
		}
		pfbase = &fbase
		ssize = u64(g_stack_base) - u64(pfbase)
	}
	$if x64 {
		C.fprintf(C.stderr, c'> trace %8d %8ld %8ld %s\n', tid, ns, ssize, fname.str)
	} $else {
		C.fprintf(C.stderr, c'> trace %8d %8lld %8lld %s\n', tid, ns, ssize, fname.str)
	}
	C.fflush(C.stderr)
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

@[markused]
pub fn on_c_main(should_trace_c_main bool) {
	g_start_time = current_time()
	//                    > trace  2128896   714640    28148 fn
	C.fprintf(C.stderr, c'#          tid       ns      ssize name\n')
	C.fflush(C.stderr)
	if should_trace_c_main {
		on_call('C.main')
	}
}
