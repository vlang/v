#ifndef V3_PTHREAD_HELPER_H
#define V3_PTHREAD_HELPER_H

#include <pthread.h>
#include <stddef.h>
#include <stdlib.h>

/* V3 can inline this header into its headerless C output. Keep the one POSIX
 * declaration used only by the helper available after includes are flattened. */
extern int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stack_size);

static inline pthread_t v3_pthread_zero(void) {
	return (pthread_t)0;
}

static inline int v3_pthread_create(pthread_t *thread, size_t stack_size,
	void *(*start_routine)(void *), void *arg) {
	pthread_attr_t attr;
	int rc = pthread_attr_init(&attr);
	if (rc != 0) {
		return rc;
	}
#ifdef __APPLE__
	/* Compiler workers are latency-sensitive foreground work. An explicit
	 * user-initiated QoS keeps custom pthreads on the same scheduling tier as
	 * the invoking compiler instead of leaving them at the default tier. */
	rc = pthread_attr_set_qos_class_np(&attr, QOS_CLASS_USER_INITIATED, 0);
#endif
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
