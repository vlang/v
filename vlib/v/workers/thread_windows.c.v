module workers

#include "@VMODROOT/vlib/v/winthread_helper.h"

fn C.v3_win_thread_create(stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr, thread_id &u32) voidptr
fn C.v3_win_thread_join(handle voidptr) int
fn C.v3_win_thread_is_current(thread_id u32) int

// WorkerThread is one joinable persistent pool worker.
struct WorkerThread {
	handle voidptr
	// id is recorded at creation: the kernel32 import list bundled with tcc
	// has no GetThreadId to recover it from the handle.
	id u32
}

// worker_thread_create starts a joinable worker whose stack reservation is
// `stack_size`, matching the POSIX pool's pthread_attr_setstacksize. The
// callback runs behind a WINAPI thunk, so its C calling convention never has
// to match the thread start routine's. A non-zero result means the thread
// (or its thunk context) could not be created.
fn worker_thread_create(stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr) (WorkerThread, int) {
	mut id := u32(0)
	handle := C.v3_win_thread_create(stack_size, start_routine, arg, &id)
	if handle == unsafe { nil } {
		return WorkerThread{}, 1
	}
	return WorkerThread{
		handle: handle
		id:     id
	}, 0
}

// worker_thread_join waits for the worker to exit and releases its handle.
fn worker_thread_join(worker WorkerThread) int {
	return C.v3_win_thread_join(worker.handle)
}

// worker_thread_is_current reports whether the calling thread is `worker`.
fn worker_thread_is_current(worker WorkerThread) bool {
	return C.v3_win_thread_is_current(worker.id) != 0
}
