@[has_globals]
module os

#flag -lpthread
#include <signal.h>

fn C.pthread_self() usize

// g_main_thread_id and is_main_thread can be used to determine if the current thread is the main thread.
// if need to get the tid of the main thread, can use the global variable g_main_thread_id
// instead of using thread_id() every time.
__global g_main_thread_id = u64(C.pthread_self())

// is_main_thread returns whether the current thread is the main thread.
pub fn is_main_thread() bool {
	return g_main_thread_id == u64(C.pthread_self())
}

fn C.sigaddset(set &u32, signum int) int
fn C.sigemptyset(set &u32)
fn C.sigprocmask(how int, set &u32, oldset &u32) int

fn signal_ignore_internal(args ...Signal) {
	mask1 := u32(0)
	C.sigemptyset(&mask1)
	for arg in args {
		C.sigaddset(&mask1, int(arg))
	}
	C.sigprocmask(C.SIG_BLOCK, &mask1, unsafe { nil })
}
