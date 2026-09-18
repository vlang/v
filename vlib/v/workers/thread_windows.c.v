module workers

#include "@VMODROOT/vlib/v/winthread_helper.h"

fn C.v3_win_thread_create(stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr) voidptr
fn C.v3_win_thread_join(handle voidptr) int

// WorkerThread is one joinable persistent pool worker.
struct WorkerThread {
	handle voidptr
}

// worker_thread_create starts a joinable worker whose stack reservation is
// `stack_size`, matching the POSIX pool's pthread_attr_setstacksize. The
// callback runs behind a WINAPI thunk, so its C calling convention never has
// to match the thread start routine's. A non-zero result means the thread
// (or its thunk context) could not be created.
fn worker_thread_create(stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr) (WorkerThread, int) {
	handle := C.v3_win_thread_create(stack_size, start_routine, arg)
	if handle == unsafe { nil } {
		return WorkerThread{}, 1
	}
	return WorkerThread{
		handle: handle
	}, 0
}

// worker_thread_join waits for the worker to exit and releases its handle.
fn worker_thread_join(thread WorkerThread) int {
	return C.v3_win_thread_join(thread.handle)
}
