/*
    Compability header for stdatomic.h that works for all compilers supported
    by V. For TCC the atomic features missing are implemented using mutex locks

*/
#ifndef __cplusplus
// If C just use stdatomic.h
#ifndef __TINYC__
#include <stdatomic.h>
#endif
#else
// CPP wrapper for atomic operations that are compatible with C
#include "atomic_cpp.h"
#endif

#ifdef __TINYC__
#include <pthread.h>

typedef intptr_t atomic_llong;
typedef intptr_t atomic_ullong;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

/*
    Wrapper for TCC to use mutex locks since it lacks the atomic functions
*/
static inline intptr_t atomic_fetch_add_explicit(intptr_t *x, size_t offset, int mo)
{
    pthread_mutex_lock(&lock);

    intptr_t old_value = *x;
    *x = *x + offset;

    pthread_mutex_unlock(&lock);

    return old_value;
}

/*
    Wrapper for TCC to use mutex locks since it lacks the atomic functions
*/
static inline intptr_t atomic_fetch_sub_explicit(intptr_t *x, size_t offset, int mo)
{
    pthread_mutex_lock(&lock);

    intptr_t old_value = *x;
    *x = *x - offset;

    pthread_mutex_unlock(&lock);

    return old_value;
}

#endif
