#ifndef V3_PTHREAD_HELPER_H
#define V3_PTHREAD_HELPER_H

#include <pthread.h>
#include <stddef.h>
#include <stdlib.h>

/* V3 can inline this header into its headerless C output. Keep the one POSIX
 * declaration used only by the helper available after includes are flattened. */
extern int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stack_size);

static inline int v3_pthread_create(pthread_t *thread, size_t stack_size,
	void *(*start_routine)(void *), void *arg) {
	pthread_attr_t attr;
	int rc = pthread_attr_init(&attr);
	if (rc != 0) {
		return rc;
	}
	rc = pthread_attr_setstacksize(&attr, stack_size);
	if (rc == 0) {
		rc = pthread_create(thread, &attr, start_routine, arg);
	}
	if (pthread_attr_destroy(&attr) != 0) {
		abort();
	}
	return rc;
}

#endif
