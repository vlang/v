@[has_globals]
module os

#flag -lpthread

fn C.pthread_self() usize

// g_main_thread_id and is_main_thread can be used to determine if the current thread is the main thread.
// if need to get the tid of the main thread, can use the global variable g_main_thread_id
// instead of using thread_id() every time.
__global g_main_thread_id = u64(C.pthread_self())

// is_main_thread returns whether the current thread is the main thread.
pub fn is_main_thread() bool {
	return g_main_thread_id == u64(C.pthread_self())
}

fn signal_ignore_internal(args ...Signal) {
	// just do nothing by default, since that is most portable ¯\_(ツ)_/¯,
	// especially on platforms like OpenBSD etc
}
