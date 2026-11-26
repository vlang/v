#ifndef VLIBC_HOSTED_ABI_H
#define VLIBC_HOSTED_ABI_H
#include <pthread.h>
#include <stdint.h>

/* Only native pthread_t representation and callback types cross this boundary. */
_Static_assert(sizeof(pthread_t) <= sizeof(uintptr_t), "pthread_t carrier is too small");
#ifdef V_GCBOEHM_NATIVE
int v_gc_pthread_create(void *, void *, void *, void *);
int v_gc_pthread_join(pthread_t, void *);
#endif
static inline int v_hosted_vlibc_create(uintptr_t *output, void *(*entry)(void *), void *argument) {
    pthread_t thread;
#ifdef V_GCBOEHM_NATIVE
    int result = v_gc_pthread_create(&thread, NULL, (void *)entry, argument);
#else
    int result = pthread_create(&thread, NULL, entry, argument);
#endif
    if (!result) *output = (uintptr_t)thread;
    return result;
}
static inline int v_hosted_vlibc_join(uintptr_t thread) {
#ifdef V_GCBOEHM_NATIVE
    return v_gc_pthread_join((pthread_t)thread, NULL);
#else
    return pthread_join((pthread_t)thread, NULL);
#endif
}
#endif
