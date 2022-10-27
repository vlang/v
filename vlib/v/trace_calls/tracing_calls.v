[has_globals]
module trace_calls

[markused]
__global g_stack_base = &u8(0)
__global g_start_time = u64(0)

pub const is_used = 1

// unix:
pub struct C.timespec {
mut:
	tv_sec  i64
	tv_nsec i64
}

fn C.gettid() u32
fn C.clock_gettime(int, &C.timespec)
fn C.pthread_self() u64

// windows:
fn C.GetCurrentThreadId() u32
fn C.QueryPerformanceCounter(&u64) C.BOOL

[markused]
pub fn on_call(fname string) {
	mut volatile pfbase := unsafe { &u8(0) }
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
	C.fprintf(C.stderr, c'> trace %8d %8ld %8d %s\n', tid, ns, ssize, fname.str)
	C.fflush(C.stderr)
}

[inline]
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

[markused]
pub fn on_c_main() {
	g_start_time = current_time()
	C.fprintf(C.stderr, c'#          tid       ns      ssize name\n')
	C.fflush(C.stderr)
	on_call('C.main')
	//                    > trace  2128896   714640    28148 fn
}
