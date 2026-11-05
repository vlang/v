#if defined(_WIN32) && defined(V_GCBOEHM_NATIVE)
#include <windows.h>
#ifndef GC_H
#include <gc.h>
#endif

typedef struct {
	LPTHREAD_START_ROUTINE start;
	void *arg;
} v_sync_test_pending;

static v_sync_test_pending *v_sync_test_queue;
static int v_sync_test_finalized;
static int v_sync_test_called;
static int v_sync_test_launch_error;

static HANDLE v_sync_test_start_launcher(LPTHREAD_START_ROUTINE start) {
	return GC_CreateThread(NULL, 0, start, NULL, 0, NULL);
}

static HANDLE WINAPI v_sync_test_defer_thread(LPSECURITY_ATTRIBUTES attributes,
	SIZE_T stack_size, LPTHREAD_START_ROUTINE start, LPVOID arg, DWORD flags, LPDWORD id) {
	(void)attributes;
	(void)stack_size;
	(void)flags;
	(void)id;
	HANDLE handle = CreateEventW(NULL, TRUE, FALSE, NULL);
	if (handle == NULL) return NULL;
	/* GC_CreateThread uses this kind of traced holder for its callback argument.
	 * Delay the callback until the launching thread exits and collections finish. */
	v_sync_test_queue = (v_sync_test_pending *)GC_malloc_uncollectable(sizeof(v_sync_test_pending));
	if (v_sync_test_queue == NULL) {
		CloseHandle(handle);
		SetLastError(ERROR_NOT_ENOUGH_MEMORY);
		return NULL;
	}
	v_sync_test_queue->start = start;
	v_sync_test_queue->arg = arg;
	return handle;
}

#undef GC_CreateThread
#define GC_CreateThread v_sync_test_defer_thread
#include "../thread_helper.h"
#undef GC_CreateThread

static void GC_CALLBACK v_sync_test_finalizer(void *object, void *data) {
	(void)object;
	(void)data;
	v_sync_test_finalized++;
}

static unsigned int v_sync_test_callback(void *arg) {
	v_sync_test_called++;
	unsigned int result = *(unsigned int *)arg;
	GC_register_finalizer_no_order(arg, NULL, NULL, NULL, NULL);
	GC_free(arg);
	return result;
}

static DWORD WINAPI v_sync_test_launch(void *unused) {
	(void)unused;
	unsigned int *arg = (unsigned int *)GC_malloc(2048);
	if (arg == NULL) return ERROR_NOT_ENOUGH_MEMORY;
	*arg = 42;
	GC_register_finalizer_no_order(arg, v_sync_test_finalizer, NULL, NULL, NULL);
	v_sync_test_launch_error = v_sync_thread_create_detached((void *)v_sync_test_callback, arg);
	return 0;
}

int v_sync_test_pending_gc_context(void) {
	v_sync_test_queue = NULL;
	v_sync_test_finalized = 0;
	v_sync_test_called = 0;
	v_sync_test_launch_error = 0;
	/* The launcher exits before collection, leaving no conservative stack roots
	 * for the argument besides the queued callback's context. */
	HANDLE launcher = v_sync_test_start_launcher(v_sync_test_launch);
	if (launcher == NULL) return 1;
	DWORD wait_result = WaitForSingleObject(launcher, INFINITE);
	CloseHandle(launcher);
	if (wait_result != WAIT_OBJECT_0 || v_sync_test_launch_error != 0 || v_sync_test_queue == NULL) return 2;
	for (int i = 0; i < 3; i++) {
		GC_gcollect();
		GC_invoke_finalizers();
	}
	int result;
	if (v_sync_test_finalized != 0) {
		/* Do not invoke a callback with an argument the collector already reclaimed. */
		if (GC_base(v_sync_test_queue->arg) != NULL) {
			GC_free(v_sync_test_queue->arg);
		} else {
			HeapFree(GetProcessHeap(), 0, v_sync_test_queue->arg);
		}
		result = 3;
	} else {
		DWORD callback_result = v_sync_test_queue->start(v_sync_test_queue->arg);
		result = callback_result == 42 && v_sync_test_called == 1 ? 0 : 4;
	}
	GC_free(v_sync_test_queue);
	v_sync_test_queue = NULL;
	return result;
}
#else
int v_sync_test_pending_gc_context(void) { return 0; }
#endif
