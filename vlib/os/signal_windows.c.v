@[has_globals]
module os

fn C.GetCurrentThreadId() u32

// g_main_thread_id and is_main_thread can be used to determine if the current thread is the main thread.
// if need to get the tid of the main thread, can use the global variable g_main_thread_id
// instead of using thread_id() every time.
__global g_main_thread_id = u64(C.GetCurrentThreadId())

// is_main_thread returns whether the current thread is the main thread.
pub fn is_main_thread() bool {
	return g_main_thread_id == u64(C.GetCurrentThreadId())
}

// The windows platform does not need to be handled.
fn signal_ignore_internal(_args ...Signal) {
}
