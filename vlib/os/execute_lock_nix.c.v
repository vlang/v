@[has_globals]
module os

// g_v_os_execute_mutex_storage backs a pthread_mutex_t used to serialize
// pipe()+spawn from os.execute(). On macOS arm64 GitHub Actions runners,
// two threads racing through pipe()+posix_spawn produced empty captured
// output even with FD_CLOEXEC on the pipe ends. 128 bytes is comfortably
// larger than sizeof(pthread_mutex_t) on macOS (64) and Linux (40-48),
// so we never need to know the actual layout — only the C functions
// declared in builtin/cfns.c.v which take voidptr.
// Use words to align the native mutex's atomic operations. A byte array can
// otherwise land at an unaligned address and trap on targets such as s390x.
__global g_v_os_execute_mutex_storage = [16]u64{}

fn v_os_execute_mutex_ptr() voidptr {
	return unsafe { voidptr(&g_v_os_execute_mutex_storage[0]) }
}

fn init() {
	C.pthread_mutex_init(v_os_execute_mutex_ptr(), unsafe { nil })
}

fn v_os_execute_lock() {
	C.pthread_mutex_lock(v_os_execute_mutex_ptr())
}

fn v_os_execute_unlock() {
	C.pthread_mutex_unlock(v_os_execute_mutex_ptr())
}
