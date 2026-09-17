#ifndef V3_WINTHREAD_HELPER_H
#define V3_WINTHREAD_HELPER_H

#include <windows.h>
#include <stddef.h>

/* Compiler workers run deeply recursive phases, so the requested stack size
 * is a reservation (like pthread_attr_setstacksize), not just the initial
 * commit; without STACK_SIZE_PARAM_IS_A_RESERVATION the reserve would stay at
 * the PE default and the workers would overflow on the same inputs the POSIX
 * pool handles. */
static inline void *v3_win_thread_create(size_t stack_size,
	void *(*start_routine)(void *), void *arg) {
	return (void *)CreateThread(NULL, stack_size, (LPTHREAD_START_ROUTINE)start_routine,
		arg, STACK_SIZE_PARAM_IS_A_RESERVATION, NULL);
}

static inline int v3_win_thread_join(void *handle) {
	if (WaitForSingleObject((HANDLE)handle, INFINITE) != WAIT_OBJECT_0) {
		return 1;
	}
	if (!CloseHandle((HANDLE)handle)) {
		return 2;
	}
	return 0;
}

#endif
