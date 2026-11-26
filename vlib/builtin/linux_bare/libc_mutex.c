#include "libc_compat.h"
#include "libc_cancel_abi.h"
#include <limits.h>
#include <pthread.h>
#include <string.h>
#include <time.h>

/* Storage is owned by this runtime, including attributes created by its API.
 * The standard all-zero static initializer denotes a private normal mutex. */
struct v_mutex {
    int lock, owner;
    unsigned int depth, attributes;
};
#define V_MUTEX_SHARED 4u
#define V_MUTEX_DESTROYED 8u
#define V_MUTEX_TYPE 3u
_Static_assert(sizeof(pthread_mutex_t) >= sizeof(struct v_mutex), "bare mutex storage");
_Static_assert(_Alignof(pthread_mutex_t) >= _Alignof(struct v_mutex), "bare mutex alignment");
_Static_assert(sizeof(pthread_mutexattr_t) >= sizeof(unsigned int), "bare mutex attributes");
extern void v_native_mutex_initializer(uintptr_t,int32_t,int32_t,int32_t);

int pthread_mutexattr_init(pthread_mutexattr_t *attributes) {
    memset(attributes, 0, sizeof *attributes);
    return 0;
}
int pthread_mutexattr_destroy(pthread_mutexattr_t *attributes) {
    (void)attributes;
    return 0;
}
int pthread_mutexattr_gettype(const pthread_mutexattr_t *attributes, int *type) {
    *type = (int)(*(const unsigned int *)(const void *)attributes & V_MUTEX_TYPE);
    return 0;
}
int pthread_mutexattr_settype(pthread_mutexattr_t *attributes, int type) {
    if (type != PTHREAD_MUTEX_NORMAL && type != PTHREAD_MUTEX_RECURSIVE &&
        type != PTHREAD_MUTEX_ERRORCHECK && type != PTHREAD_MUTEX_DEFAULT)
        return EINVAL;
    unsigned int *bits = (void *)attributes;
    *bits = (*bits & ~V_MUTEX_TYPE) | (unsigned int)type;
    return 0;
}
int pthread_mutexattr_getpshared(const pthread_mutexattr_t *attributes, int *shared) {
    *shared = *(const unsigned int *)(const void *)attributes & V_MUTEX_SHARED
                  ? PTHREAD_PROCESS_SHARED
                  : PTHREAD_PROCESS_PRIVATE;
    return 0;
}
int pthread_mutexattr_setpshared(pthread_mutexattr_t *attributes, int shared) {
    if (shared != PTHREAD_PROCESS_PRIVATE && shared != PTHREAD_PROCESS_SHARED)
        return EINVAL;
    unsigned int *bits = (void *)attributes;
    *bits = (*bits & ~V_MUTEX_SHARED) | (shared == PTHREAD_PROCESS_SHARED ? V_MUTEX_SHARED : 0);
    return 0;
}
int pthread_mutexattr_getprotocol(const pthread_mutexattr_t *attributes, int *protocol) {
    (void)attributes;
    *protocol = PTHREAD_PRIO_NONE;
    return 0;
}
int pthread_mutexattr_setprotocol(pthread_mutexattr_t *attributes, int protocol) {
    (void)attributes;
    if (protocol == PTHREAD_PRIO_NONE)
        return 0;
    return protocol == PTHREAD_PRIO_INHERIT || protocol == PTHREAD_PRIO_PROTECT ? ENOTSUP : EINVAL;
}
int pthread_mutexattr_getrobust(const pthread_mutexattr_t *attributes, int *robust) {
    (void)attributes;
    *robust = PTHREAD_MUTEX_STALLED;
    return 0;
}
int pthread_mutexattr_setrobust(pthread_mutexattr_t *attributes, int robust) {
    (void)attributes;
    return robust == PTHREAD_MUTEX_STALLED ? 0 : robust == PTHREAD_MUTEX_ROBUST ? ENOTSUP : EINVAL;
}
int pthread_mutex_init(pthread_mutex_t *storage, const pthread_mutexattr_t *attributes) {
    unsigned int flags = attributes ? *(const unsigned int *)(const void *)attributes : 0;
    if (flags & ~(V_MUTEX_TYPE | V_MUTEX_SHARED))
        return EINVAL;
    memset(storage, 0, sizeof *storage);
    ((struct v_mutex *)(void *)storage)->attributes = flags;
    return 0;
}
int pthread_mutex_destroy(pthread_mutex_t *storage) {
    struct v_mutex *mutex = (void *)storage;
    if (__atomic_load_n(&mutex->lock, __ATOMIC_ACQUIRE))
        return EBUSY;
    mutex->attributes |= V_MUTEX_DESTROYED;
    return 0;
}
static int v_mutex_acquire(pthread_mutex_t *storage, int nonblocking, clockid_t clock,
                           const struct timespec *deadline) {
    struct v_mutex *mutex = (void *)storage;
#if defined(__GLIBC__)
    v_native_mutex_initializer((uintptr_t)&mutex->attributes,storage->__data.__kind,
                               PTHREAD_MUTEX_RECURSIVE,PTHREAD_MUTEX_ERRORCHECK);
#endif
    if (mutex->attributes & V_MUTEX_DESTROYED)
        return EINVAL;
    unsigned int type = mutex->attributes & V_MUTEX_TYPE;
    /* Normal mutexes do not inspect ownership, including process-shared ones. */
    int tid = type == PTHREAD_MUTEX_NORMAL ? 0 : (int)V_RAW0(SYS_gettid);
    if (type != PTHREAD_MUTEX_NORMAL && __atomic_load_n(&mutex->owner, __ATOMIC_RELAXED) == tid) {
        if (type == PTHREAD_MUTEX_RECURSIVE) {
            if (mutex->depth == UINT_MAX)
                return EAGAIN;
            ++mutex->depth;
            return 0;
        }
        return nonblocking ? EBUSY : EDEADLK;
    }
    int expected = 0;
    if (!__atomic_compare_exchange_n(&mutex->lock, &expected, 1, 0, __ATOMIC_ACQUIRE,
                                     __ATOMIC_RELAXED)) {
        if (nonblocking)
            return EBUSY;
        if (deadline && (deadline->tv_nsec < 0 || deadline->tv_nsec >= 1000000000))
            return EINVAL;
        while (__atomic_exchange_n(&mutex->lock, 2, __ATOMIC_ACQUIRE)) {
            if (deadline && deadline->tv_sec < 0)
                return ETIMEDOUT;
            int operation = mutex->attributes & V_MUTEX_SHARED ? 0 : 128;
            if (deadline)
                operation |= 9 | (clock == CLOCK_REALTIME ? 256 : 0); /* WAIT_BITSET */
            long result = V_RAW6(SYS_futex, &mutex->lock, operation, 2, deadline, 0, UINT32_MAX);
            if (result == -ETIMEDOUT)
                return ETIMEDOUT;
            if (result < 0 && result != -EINTR && result != -EAGAIN)
                return (int)-result;
        }
    }
    __atomic_store_n(&mutex->owner, tid, __ATOMIC_RELAXED);
    mutex->depth = 1;
    return 0;
}
int pthread_mutex_lock(pthread_mutex_t *storage) {
    return v_mutex_acquire(storage, 0, CLOCK_REALTIME, NULL);
}
int pthread_mutex_trylock(pthread_mutex_t *storage) {
    return v_mutex_acquire(storage, 1, CLOCK_REALTIME, NULL);
}
int pthread_mutex_timedlock(pthread_mutex_t *storage, const struct timespec *deadline) {
    return v_mutex_acquire(storage, 0, CLOCK_REALTIME, deadline);
}
int pthread_mutex_clocklock(pthread_mutex_t *storage, clockid_t clock,
                            const struct timespec *deadline) {
    if (clock != CLOCK_REALTIME && clock != CLOCK_MONOTONIC)
        return EINVAL;
    return v_mutex_acquire(storage, 0, clock, deadline);
}
int pthread_mutex_unlock(pthread_mutex_t *storage) {
    struct v_mutex *mutex = (void *)storage;
#if defined(__GLIBC__)
    v_native_mutex_initializer((uintptr_t)&mutex->attributes,storage->__data.__kind,
                               PTHREAD_MUTEX_RECURSIVE,PTHREAD_MUTEX_ERRORCHECK);
#endif
    if (mutex->attributes & V_MUTEX_DESTROYED)
        return EINVAL;
    unsigned int type = mutex->attributes & V_MUTEX_TYPE;
    if (type != PTHREAD_MUTEX_NORMAL &&
        __atomic_load_n(&mutex->owner, __ATOMIC_RELAXED) != (int)V_RAW0(SYS_gettid))
        return EPERM;
    if (type == PTHREAD_MUTEX_RECURSIVE && mutex->depth > 1) {
        --mutex->depth;
        return 0;
    }
    __atomic_store_n(&mutex->owner, 0, __ATOMIC_RELAXED);
    mutex->depth = 0;
    if (__atomic_exchange_n(&mutex->lock, 0, __ATOMIC_RELEASE) == 2)
        (void)V_RAW6(SYS_futex, &mutex->lock, (mutex->attributes & V_MUTEX_SHARED ? 0 : 128) | 1, 1,
                     0, 0, 0);
    return 0;
}

/* A generation counter closes the unlock/sleep race. Waiters register before
 * releasing the mutex; signal changes the counter before the futex wake. */
struct v_condition {
    unsigned int sequence, waiters, attributes, destroyed;
};
#define V_COND_SHARED 1u
#define V_COND_MONOTONIC 2u
_Static_assert(sizeof(pthread_cond_t) >= sizeof(struct v_condition), "bare condition storage");
int pthread_condattr_init(pthread_condattr_t *a) {
    memset(a, 0, sizeof *a);
    return 0;
}
int pthread_condattr_destroy(pthread_condattr_t *a) {
    (void)a;
    return 0;
}
int pthread_condattr_getclock(const pthread_condattr_t *a, clockid_t *clock) {
    *clock = *(const unsigned int *)(const void *)a & V_COND_MONOTONIC ? CLOCK_MONOTONIC
                                                                       : CLOCK_REALTIME;
    return 0;
}
int pthread_condattr_setclock(pthread_condattr_t *a, clockid_t clock) {
    if (clock != CLOCK_MONOTONIC && clock != CLOCK_REALTIME)
        return EINVAL;
    unsigned int *p = (void *)a;
    *p = (*p & ~V_COND_MONOTONIC) | (clock == CLOCK_MONOTONIC ? V_COND_MONOTONIC : 0);
    return 0;
}
int pthread_condattr_getpshared(const pthread_condattr_t *a, int *shared) {
    *shared = *(const unsigned int *)(const void *)a & V_COND_SHARED ? PTHREAD_PROCESS_SHARED
                                                                     : PTHREAD_PROCESS_PRIVATE;
    return 0;
}
int pthread_condattr_setpshared(pthread_condattr_t *a, int shared) {
    if (shared != PTHREAD_PROCESS_PRIVATE && shared != PTHREAD_PROCESS_SHARED)
        return EINVAL;
    unsigned int *p = (void *)a;
    *p = (*p & ~V_COND_SHARED) | (shared == PTHREAD_PROCESS_SHARED ? V_COND_SHARED : 0);
    return 0;
}
int pthread_cond_init(pthread_cond_t *storage, const pthread_condattr_t *a) {
    unsigned int bits = a ? *(const unsigned int *)(const void *)a : 0;
    if (bits & ~(V_COND_SHARED | V_COND_MONOTONIC))
        return EINVAL;
    memset(storage, 0, sizeof *storage);
    ((struct v_condition *)(void *)storage)->attributes = bits;
    return 0;
}
int pthread_cond_destroy(pthread_cond_t *storage) {
    struct v_condition *c = (void *)storage;
    if (__atomic_load_n(&c->waiters, __ATOMIC_ACQUIRE))
        return EBUSY;
    c->destroyed = 1;
    return 0;
}
static int v_condition_wake(pthread_cond_t *storage, int count) {
    struct v_condition *c = (void *)storage;
    if (c->destroyed)
        return EINVAL;
    __atomic_add_fetch(&c->sequence, 1, __ATOMIC_RELEASE);
    long result = V_RAW6(SYS_futex, &c->sequence, 1 | (c->attributes & V_COND_SHARED ? 0 : 128),
                         count, 0, 0, 0);
    return result < 0 ? (int)-result : 0;
}
int pthread_cond_signal(pthread_cond_t *c) { return v_condition_wake(c, 1); }
int pthread_cond_broadcast(pthread_cond_t *c) { return v_condition_wake(c, INT_MAX); }
static int v_condition_wait(pthread_cond_t *storage, pthread_mutex_t *mutex, clockid_t clock,
                            const struct timespec *deadline) {
    extern int32_t v_cancel_condition_wait(uintptr_t, uintptr_t, int32_t, uintptr_t);
    return v_cancel_condition_wait((uintptr_t)storage, (uintptr_t)mutex, clock, (uintptr_t)deadline);
}
void v_cancel_condition_fields(uintptr_t storage, uintptr_t output) {
    struct v_condition *c = (void *)storage;
    uintptr_t *fields = (void *)output;
    fields[0] = (uintptr_t)&c->sequence; fields[1] = (uintptr_t)&c->waiters;
    fields[2] = c->attributes; fields[3] = c->destroyed;
}
int32_t v_cancel_mutex_lock(uintptr_t storage) { return pthread_mutex_lock((pthread_mutex_t *)storage); }
int32_t v_cancel_mutex_unlock(uintptr_t storage) { return pthread_mutex_unlock((pthread_mutex_t *)storage); }
int pthread_cond_wait(pthread_cond_t *c, pthread_mutex_t *m) {
    return v_condition_wait(c, m, CLOCK_MONOTONIC, NULL);
}
int pthread_cond_timedwait(pthread_cond_t *c, pthread_mutex_t *m, const struct timespec *t) {
    struct v_condition *v = (void *)c;
    return v_condition_wait(c, m,
                            v->attributes & V_COND_MONOTONIC ? CLOCK_MONOTONIC : CLOCK_REALTIME, t);
}
int pthread_cond_clockwait(pthread_cond_t *c, pthread_mutex_t *m, clockid_t clock,
                           const struct timespec *t) {
    return v_condition_wait(c, m, clock, t);
}
int(pthread_equal)(pthread_t a, pthread_t b) { return a == b; }
