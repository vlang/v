#ifndef V3_WINTHREAD_HELPER_H
#define V3_WINTHREAD_HELPER_H

#include <windows.h>
#include <stddef.h>

/* The pool callback has the default C calling convention, while CreateThread
 * needs a WINAPI (stdcall on x86) routine returning DWORD. Casting between the
 * two corrupts the stack on 32-bit Windows, so the callback runs behind a real
 * WINAPI thunk, like the __v_windows_thread_start thunk the C generator emits
 * for `spawn`. The context comes from the process heap, not malloc, so the
 * -prealloc arena of the creating thread never has to outlive the worker. */
typedef struct {
	void *(*start_routine)(void *);
	void *arg;
} v3_win_thread_context;

static DWORD WINAPI v3_win_thread_thunk(LPVOID raw_context) {
	v3_win_thread_context *context = (v3_win_thread_context *)raw_context;
	void *(*start_routine)(void *) = context->start_routine;
	void *arg = context->arg;
	HeapFree(GetProcessHeap(), 0, context);
	start_routine(arg);
	return 0;
}

/* Compiler workers run deeply recursive phases, so the requested stack size
 * is a reservation (like pthread_attr_setstacksize), not just the initial
 * commit; without STACK_SIZE_PARAM_IS_A_RESERVATION the reserve would stay at
 * the PE default and the workers would overflow on the same inputs the POSIX
 * pool handles. Returns NULL when the context or the thread cannot be
 * created; the pool then counts a launch failure. */
static inline void *v3_win_thread_create(size_t stack_size,
	void *(*start_routine)(void *), void *arg) {
	v3_win_thread_context *context = (v3_win_thread_context *)HeapAlloc(
		GetProcessHeap(), 0, sizeof(v3_win_thread_context));
	if (context == NULL) {
		return NULL;
	}
	context->start_routine = start_routine;
	context->arg = arg;
	HANDLE handle = CreateThread(NULL, stack_size, v3_win_thread_thunk, context,
		STACK_SIZE_PARAM_IS_A_RESERVATION, NULL);
	if (handle == NULL) {
		HeapFree(GetProcessHeap(), 0, context);
	}
	return (void *)handle;
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
