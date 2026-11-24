@[has_globals]
module os

$if !(macos && freestanding && amd64 && !native) {
	#flag -lpthread

	fn C.pthread_self() voidptr
}

// g_main_thread_id and is_main_thread can be used to determine if the current thread is the main thread.
// if need to get the tid of the main thread, can use the global variable g_main_thread_id
// instead of using thread_id() every time.
__global g_main_thread_id = signal_thread_id()

@[inline]
fn signal_thread_id() u64 {
	$if macos && freestanding && amd64 && !native {
		return bf_gettid()
	} $else {
		return u64(C.pthread_self())
	}
}

// is_main_thread returns whether the current thread is the main thread.
pub fn is_main_thread() bool {
	return g_main_thread_id == signal_thread_id()
}

fn C.sigaddset(set &u32, signum i32) i32
fn C.sigemptyset(set &u32)
fn C.sigprocmask(how i32, set &u32, oldset &u32) i32

$if macos && freestanding && amd64 && !native {
	fn C.v_macos_kernel_bsd_call6(u32, u64, u64, u64, u64, u64, u64, &u64) i32
}

fn signal_ignore_internal(args ...Signal) {
	mask1 := u32(0)
	C.sigemptyset(&mask1)
	for arg in args {
		C.sigaddset(&mask1, int(arg))
	}
	$if macos && freestanding && amd64 && !native {
		// BSD329 changes only the calling thread's mask, including raw V workers.
		C.v_macos_kernel_bsd_call6(329, C.SIG_BLOCK, u64(&mask1), 0, 0, 0, 0, unsafe { nil })
	} $else {
		C.sigprocmask(C.SIG_BLOCK, &mask1, unsafe { nil })
	}
}
