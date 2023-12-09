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

@[typedef]
struct C.sigset_t {}

fn C.sigaddset(set &C.sigset_t, signum int) int
fn C.sigemptyset(set &C.sigset_t)
fn C.sigprocmask(how int, set &C.sigset_t, oldset &C.sigset_t) int

fn signal_ignore_internal(args ...Signal) {
	$if !android && !macos && !openbsd {
		mask1 := C.sigset_t{}
		C.sigemptyset(&mask1)
		for arg in args {
			C.sigaddset(&mask1, int(arg))
		}
		C.sigprocmask(C.SIG_BLOCK, &mask1, unsafe { nil })
	}
}
