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

V_SYNC_THREAD_SCOPE int v_sync_thread_create_detached_with_stack(void *start, void *arg, size_t stack_size) {
	HANDLE handle = CreateThread(NULL, stack_size, (LPTHREAD_START_ROUTINE)start,
		arg, 0, NULL);
	if (handle == NULL) {
		return (int)GetLastError();
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

static inline int v_sync_thread_create_detached(void *start, void *arg) {
	return v_sync_thread_create_detached_with_stack(start, arg, V_THREAD_STACK_SIZE);
}

#endif
