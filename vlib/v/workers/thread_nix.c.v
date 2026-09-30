module workers

#include "@VMODROOT/vlib/v/pthread_helper.h"

$if linux {
	#include <sys/prctl.h>
}

// C.pthread_t is the platform pthread handle type.
@[typedef]
struct C.pthread_t {}

fn C.pthread_join(worker C.pthread_t, retval voidptr) int
fn C.prctl(option int, arg2 voidptr, arg3 u64, arg4 u64, arg5 u64) int
fn C.v3_pthread_zero() C.pthread_t
fn C.v3_pthread_create(worker &C.pthread_t, stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.v3_pthread_is_current(worker C.pthread_t) int

// WorkerThread is one joinable persistent pool worker.
struct WorkerThread {
	id C.pthread_t
}

// worker_thread_create starts a joinable worker with an explicit stack
// reservation. A non-zero result is the pthread error code.
fn worker_thread_create(stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr) (WorkerThread, int) {
	mut thread_id := C.v3_pthread_zero()
	rc := C.v3_pthread_create(&thread_id, stack_size, start_routine, arg)
	return WorkerThread{
		id: thread_id
	}, rc
}

// worker_thread_join waits for the worker to exit. A non-zero result is the
// pthread error code.
fn worker_thread_join(worker WorkerThread) int {
	return C.pthread_join(worker.id, unsafe { nil })
}

// worker_thread_is_current reports whether the calling thread is `worker`.
fn worker_thread_is_current(worker WorkerThread) bool {
	return C.v3_pthread_is_current(worker.id) != 0
}

// name_worker_thread names the calling pool worker `v3-pool`: a diagnostics
// server forks only when every other thread of the process is one of these.
fn name_worker_thread() {
	$if linux {
		C.prctl(C.PR_SET_NAME, c'v3-pool', 0, 0, 0)
	}
}
