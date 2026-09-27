#ifndef V_SYNC_THREAD_HELPER_H
#define V_SYNC_THREAD_HELPER_H

#ifndef V_THREAD_STACK_SIZE
#define V_THREAD_STACK_SIZE 0
#endif

#ifdef _WIN32
#include <windows.h>

static inline int v_sync_thread_create_detached(void *start, void *arg) {
	HANDLE handle = CreateThread(NULL, V_THREAD_STACK_SIZE, (LPTHREAD_START_ROUTINE)start,
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

static inline int v_sync_thread_create_detached(void *start, void *arg) {
	pthread_attr_t attr;
	int rc = pthread_attr_init(&attr);
	if (rc != 0) {
		return rc;
	}
	if (V_THREAD_STACK_SIZE != 0) {
		rc = pthread_attr_setstacksize(&attr, V_THREAD_STACK_SIZE);
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

#endif
