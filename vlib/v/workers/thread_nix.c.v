module workers

#include "@VMODROOT/vlib/v/pthread_helper.h"

// C.pthread_t is the platform pthread handle type.
@[typedef]
struct C.pthread_t {}

fn C.pthread_join(thread C.pthread_t, retval voidptr) int
fn C.v3_pthread_zero() C.pthread_t
fn C.v3_pthread_create(thread &C.pthread_t, stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr) int

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
fn worker_thread_join(thread WorkerThread) int {
	return C.pthread_join(thread.id, unsafe { nil })
}
