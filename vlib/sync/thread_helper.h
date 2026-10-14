#ifndef V_SYNC_THREAD_HELPER_H
#define V_SYNC_THREAD_HELPER_H

#ifndef V_SYNC_THREAD_SCOPE
#define V_SYNC_THREAD_SCOPE static inline
#endif

#ifndef V_THREAD_STACK_SIZE
#define V_THREAD_STACK_SIZE 0
#endif

#ifdef _WIN32
#include <windows.h>

typedef struct {
	unsigned int (*start)(void *);
	void *arg;
} v_sync_windows_thread_context;

static DWORD WINAPI v_sync_windows_thread_start(void *raw_context) {
	v_sync_windows_thread_context *context = (v_sync_windows_thread_context *)raw_context;
	unsigned int (*start)(void *) = context->start;
	void *arg = context->arg;
	HeapFree(GetProcessHeap(), 0, context);
	return (DWORD)start(arg);
}

V_SYNC_THREAD_SCOPE int v_sync_thread_create_detached_with_stack(void *start, void *arg, size_t stack_size) {
	v_sync_windows_thread_context *context = (v_sync_windows_thread_context *)HeapAlloc(
		GetProcessHeap(), 0, sizeof(v_sync_windows_thread_context));
	if (context == NULL) {
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	context->start = (unsigned int (*)(void *))start;
	context->arg = arg;
	HANDLE handle = CreateThread(NULL, stack_size, v_sync_windows_thread_start, context, 0, NULL);
	if (handle == NULL) {
		DWORD error = GetLastError();
		HeapFree(GetProcessHeap(), 0, context);
		return (int)error;
	}
	CloseHandle(handle);
	return 0;
}
#else
#ifndef V_FASTC_NO_HEADERS
#include <pthread.h>
#include <stdlib.h>
#endif

V_SYNC_THREAD_SCOPE int v_sync_thread_create_detached_with_stack(void *start, void *arg, size_t stack_size) {
	pthread_attr_t attr;
	int rc = pthread_attr_init(&attr);
	if (rc != 0) {
		return rc;
	}
	if (stack_size != 0) {
		rc = pthread_attr_setstacksize(&attr, stack_size);
	}
	if (rc == 0) {
		rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	}
	if (rc == 0) {
		pthread_t thread;
		rc = pthread_create(&thread, &attr, (void *(*)(void *))start, arg);
	}
	if (pthread_attr_destroy(&attr) != 0) {
		abort();
	}
	return rc;
}
#endif

V_SYNC_THREAD_SCOPE int v_sync_thread_create_detached(void *start, void *arg) {
	return v_sync_thread_create_detached_with_stack(start, arg, V_THREAD_STACK_SIZE);
}

#endif
