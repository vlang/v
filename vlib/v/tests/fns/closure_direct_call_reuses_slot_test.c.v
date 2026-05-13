@[has_globals]
module main

$if windows {
	#include <synchapi.h>

	struct C.builtin__closure__ClosureMutex {
		closure_mtx C.SRWLOCK
	}
} $else {
	#include <pthread.h>

	@[typedef]
	pub struct C.pthread_mutex_t {}

	struct C.builtin__closure__ClosureMutex {
		closure_mtx C.pthread_mutex_t
	}
}

struct C.builtin__closure__Closure {
	ClosureMutex     C.builtin__closure__ClosureMutex
	closure_ptr      voidptr
	closure_get_data voidptr
	closure_cap      int
	v_page_size      int
	free_closure_ptr voidptr
}

__global C.g_closure C.builtin__closure__Closure

fn foo(i int) fn () int {
	return fn [i] () int {
		return i
	}
}

fn test_direct_call_of_returned_closure_reuses_closure_slot() {
	assert foo(-1)() == -1
	start_closure_cap := C.g_closure.closure_cap
	mut sum := 0
	for i in 0 .. 128 {
		sum += foo(i)()
	}
	assert sum == 8128
	assert C.g_closure.closure_cap == start_closure_cap
}
