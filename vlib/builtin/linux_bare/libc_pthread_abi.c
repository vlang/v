#include "libc_pthread_abi.h"
#include <sched.h>
#include <stddef.h>

/* Native typedefs, function pointers and opaque object sizes stay in C.
 * All thread state, allocation, synchronization and cleanup policy is V. */
_Static_assert(sizeof(pthread_t) == sizeof(uintptr_t), "native pthread handle");
_Static_assert(sizeof(pthread_key_t) == sizeof(uint32_t), "native pthread key");
_Static_assert(sizeof(pthread_once_t) == sizeof(uint32_t), "native pthread once");
_Static_assert(sizeof(pthread_attr_t) >= 3 * sizeof(uintptr_t), "native pthread attributes");
size_t v_pthread_attributes_size(void) { return sizeof(pthread_attr_t); }
size_t v_pthread_jump_size(void) { return sizeof(jmp_buf); }
uintptr_t v_pthread_call(uintptr_t function, uintptr_t argument) {
    return (uintptr_t)((void *(*)(void *))function)((void *)argument);
}
void v_pthread_destructor_call(uintptr_t function, uintptr_t argument) {
    ((void (*)(void *))function)((void *)argument);
}
void v_pthread_once_call(uintptr_t function) { ((void (*)(void))function)(); }

/* The compiler's native C TLS owns only these address-sized storage slots. */
static __thread uintptr_t v_pthread_tls[5];
uintptr_t v_pthread_tls_load(uint32_t slot) { return v_pthread_tls[slot]; }
void v_pthread_tls_store(uint32_t slot, uintptr_t value) { v_pthread_tls[slot] = value; }
uintptr_t v_pthread_tls_identity(void) { return (uintptr_t)v_pthread_tls; }

_Static_assert(offsetof(struct sched_param, sched_priority) == 0 && sizeof(struct sched_param) == 4,
               "Linux scheduling priority ABI");
int32_t v_pthread_sched_read(uintptr_t parameters) { return ((const struct sched_param *)parameters)->sched_priority; }
void v_pthread_sched_write(uintptr_t parameters, int32_t priority) { ((struct sched_param *)parameters)->sched_priority = priority; }
extern int32_t v_pthread_getname(uintptr_t, uintptr_t, uintptr_t);
extern int32_t v_pthread_setname(uintptr_t, uintptr_t);
extern int32_t v_pthread_getsched(uintptr_t, uintptr_t, uintptr_t);
extern int32_t v_pthread_setsched(uintptr_t, int32_t, uintptr_t);
extern int32_t v_pthread_setprio(uintptr_t, int32_t);
extern int32_t v_pthread_affinity(uintptr_t, uintptr_t, uintptr_t, int32_t);
int pthread_getname_np(pthread_t thread, char *name, size_t size) { return v_pthread_getname(thread, (uintptr_t)name, size); }
int pthread_setname_np(pthread_t thread, const char *name) { return v_pthread_setname(thread, (uintptr_t)name); }
int pthread_getschedparam(pthread_t thread, int *policy, struct sched_param *parameters) { return v_pthread_getsched(thread, (uintptr_t)policy, (uintptr_t)parameters); }
int pthread_setschedparam(pthread_t thread, int policy, const struct sched_param *parameters) { return v_pthread_setsched(thread, policy, (uintptr_t)parameters); }
int pthread_setschedprio(pthread_t thread, int priority) { return v_pthread_setprio(thread, priority); }
int pthread_getaffinity_np(pthread_t thread, size_t size, cpu_set_t *set) { return v_pthread_affinity(thread, size, (uintptr_t)set, 0); }
int pthread_setaffinity_np(pthread_t thread, size_t size, const cpu_set_t *set) { return v_pthread_affinity(thread, size, (uintptr_t)set, 1); }

extern int32_t v_pthread_create(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
extern int32_t v_pthread_join(uintptr_t, uintptr_t);
extern int32_t v_pthread_detach(uintptr_t);
extern uintptr_t v_pthread_self(void);
extern _Noreturn void v_pthread_exit(uintptr_t);
extern int32_t v_pthread_attr_init(uintptr_t);
extern int32_t v_pthread_attr_destroy(uintptr_t);
extern int32_t v_pthread_attr_setstack(uintptr_t, uintptr_t);
extern int32_t v_pthread_attr_getstack(uintptr_t, uintptr_t);
extern int32_t v_pthread_attr_setdetach(uintptr_t, int32_t);
extern int32_t v_pthread_attr_getdetach(uintptr_t, uintptr_t);
extern int32_t v_pthread_key_create(uintptr_t, uintptr_t);
extern int32_t v_pthread_key_delete(uint32_t);
extern int32_t v_pthread_setspecific(uint32_t, uintptr_t);
extern uintptr_t v_pthread_getspecific(uint32_t);
extern int32_t v_pthread_once(uintptr_t, uintptr_t);
extern int32_t v_pthread_tls_destructor(uintptr_t, uintptr_t, uintptr_t);

int pthread_create(pthread_t *handle, const pthread_attr_t *attributes,
                   void *(*start)(void *), void *argument) {
    return v_pthread_create((uintptr_t)handle, (uintptr_t)attributes, (uintptr_t)start, (uintptr_t)argument);
}
int pthread_join(pthread_t thread, void **result) { return v_pthread_join((uintptr_t)thread, (uintptr_t)result); }
int pthread_detach(pthread_t thread) { return v_pthread_detach((uintptr_t)thread); }
pthread_t pthread_self(void) { return (pthread_t)v_pthread_self(); }
_Noreturn void pthread_exit(void *result) { v_pthread_exit((uintptr_t)result); }
int pthread_attr_init(pthread_attr_t *attributes) { return v_pthread_attr_init((uintptr_t)attributes); }
int pthread_attr_destroy(pthread_attr_t *attributes) { return v_pthread_attr_destroy((uintptr_t)attributes); }
int pthread_attr_setstacksize(pthread_attr_t *attributes, size_t size) {
    return v_pthread_attr_setstack((uintptr_t)attributes, size);
}
int pthread_attr_getstacksize(const pthread_attr_t *attributes, size_t *size) {
    return v_pthread_attr_getstack((uintptr_t)attributes, (uintptr_t)size);
}
int pthread_attr_setdetachstate(pthread_attr_t *attributes, int state) {
    return v_pthread_attr_setdetach((uintptr_t)attributes, state);
}
int pthread_attr_getdetachstate(const pthread_attr_t *attributes, int *state) {
    return v_pthread_attr_getdetach((uintptr_t)attributes, (uintptr_t)state);
}
int pthread_key_create(pthread_key_t *key, void (*destructor)(void *)) {
    return v_pthread_key_create((uintptr_t)key, (uintptr_t)destructor);
}
int pthread_key_delete(pthread_key_t key) { return v_pthread_key_delete((uint32_t)key); }
int pthread_setspecific(pthread_key_t key, const void *value) {
    return v_pthread_setspecific((uint32_t)key, (uintptr_t)value);
}
void *pthread_getspecific(pthread_key_t key) { return (void *)v_pthread_getspecific((uint32_t)key); }
int pthread_once(pthread_once_t *control, void (*initialize)(void)) {
    return v_pthread_once((uintptr_t)control, (uintptr_t)initialize);
}
int __cxa_thread_atexit_impl(void (*function)(void *), void *argument, void *dso) {
    return v_pthread_tls_destructor((uintptr_t)function, (uintptr_t)argument, (uintptr_t)dso);
}

_Static_assert(sizeof(pthread_rwlock_t) >= 16, "native rwlock storage");
size_t v_rwlock_size(void) { return sizeof(pthread_rwlock_t); }
size_t v_rwlock_attributes_size(void) { return sizeof(pthread_rwlockattr_t); }
extern int32_t v_rwlock_attr_init(uintptr_t);
extern int32_t v_rwlock_attr_destroy(uintptr_t);
extern int32_t v_rwlock_attr_setshared(uintptr_t, int32_t);
extern int32_t v_rwlock_attr_getshared(uintptr_t, uintptr_t);
extern int32_t v_rwlock_attr_setkind(uintptr_t, int32_t);
extern int32_t v_rwlock_attr_getkind(uintptr_t, uintptr_t);
extern int32_t v_rwlock_init(uintptr_t, uintptr_t);
extern int32_t v_rwlock_destroy(uintptr_t);
extern int32_t v_rwlock_acquire(uintptr_t, int32_t, int32_t, uintptr_t);
extern int32_t v_rwlock_unlock(uintptr_t);
int pthread_rwlockattr_init(pthread_rwlockattr_t *p) { return v_rwlock_attr_init((uintptr_t)p); }
int pthread_rwlockattr_destroy(pthread_rwlockattr_t *p) { return v_rwlock_attr_destroy((uintptr_t)p); }
int pthread_rwlockattr_setpshared(pthread_rwlockattr_t *p, int shared) { return v_rwlock_attr_setshared((uintptr_t)p, shared); }
int pthread_rwlockattr_getpshared(const pthread_rwlockattr_t *p, int *shared) { return v_rwlock_attr_getshared((uintptr_t)p, (uintptr_t)shared); }
#if defined(__GLIBC__)
int pthread_rwlockattr_setkind_np(pthread_rwlockattr_t *p, int kind) { return v_rwlock_attr_setkind((uintptr_t)p, kind); }
int pthread_rwlockattr_getkind_np(const pthread_rwlockattr_t *p, int *kind) { return v_rwlock_attr_getkind((uintptr_t)p, (uintptr_t)kind); }
#endif
int pthread_rwlock_init(pthread_rwlock_t *p, const pthread_rwlockattr_t *a) { return v_rwlock_init((uintptr_t)p, (uintptr_t)a); }
int pthread_rwlock_destroy(pthread_rwlock_t *p) { return v_rwlock_destroy((uintptr_t)p); }
int pthread_rwlock_rdlock(pthread_rwlock_t *p) { return v_rwlock_acquire((uintptr_t)p, 0, 0, 0); }
int pthread_rwlock_wrlock(pthread_rwlock_t *p) { return v_rwlock_acquire((uintptr_t)p, 1, 0, 0); }
int pthread_rwlock_tryrdlock(pthread_rwlock_t *p) { return v_rwlock_acquire((uintptr_t)p, 0, 1, 0); }
int pthread_rwlock_trywrlock(pthread_rwlock_t *p) { return v_rwlock_acquire((uintptr_t)p, 1, 1, 0); }
int pthread_rwlock_timedrdlock(pthread_rwlock_t *p, const struct timespec *d) { return v_rwlock_acquire((uintptr_t)p, 0, 0, (uintptr_t)d); }
int pthread_rwlock_timedwrlock(pthread_rwlock_t *p, const struct timespec *d) { return v_rwlock_acquire((uintptr_t)p, 1, 0, (uintptr_t)d); }
int pthread_rwlock_unlock(pthread_rwlock_t *p) { return v_rwlock_unlock((uintptr_t)p); }

#include <semaphore.h>
#include <limits.h>

_Static_assert(sizeof(sem_t) >= 2 * sizeof(uint32_t) && _Alignof(sem_t) >= _Alignof(uint32_t),
               "Linux unnamed semaphore storage");
/* Freestanding limits.h can expose only ISO C limits. Check the POSIX
 * limit whenever the target headers publish it. */
#ifdef SEM_VALUE_MAX
_Static_assert(SEM_VALUE_MAX == INT32_MAX, "Linux semaphore value range");
#endif
extern int32_t v_sem_init(uintptr_t, int32_t, uint32_t);
extern int32_t v_sem_wait(uintptr_t, int32_t, uintptr_t, int32_t);
extern int32_t v_sem_post(uintptr_t);
extern int32_t v_sem_value(uintptr_t, uintptr_t);
int sem_init(sem_t *s, int shared, unsigned value) { return v_sem_init((uintptr_t)s, shared, value); }
/* Unnamed semaphores have no kernel object or allocated storage to release. */
int sem_destroy(sem_t *s) { (void)s; return 0; }
int sem_post(sem_t *s) { return v_sem_post((uintptr_t)s); }
int sem_wait(sem_t *s) { return v_sem_wait((uintptr_t)s, CLOCK_REALTIME, 0, 0); }
int sem_trywait(sem_t *s) { return v_sem_wait((uintptr_t)s, CLOCK_REALTIME, 0, 1); }
int sem_timedwait(sem_t *s, const struct timespec *t) {
    return v_sem_wait((uintptr_t)s, CLOCK_REALTIME, (uintptr_t)t, 0);
}
int sem_clockwait(sem_t *s, clockid_t clock, const struct timespec *t) {
    return v_sem_wait((uintptr_t)s, clock, (uintptr_t)t, 0);
}
int sem_getvalue(sem_t *s, int *value) { return v_sem_value((uintptr_t)s, (uintptr_t)value); }

extern int32_t v_pthread_getattr(uintptr_t, uintptr_t);
extern int32_t v_pthread_stack_bounds(uintptr_t, uintptr_t, uintptr_t);
int pthread_getattr_np(pthread_t t, pthread_attr_t *a) { return v_pthread_getattr(t, (uintptr_t)a); }
int pthread_attr_getstack(const pthread_attr_t *a, void **base, size_t *size) {
    return v_pthread_stack_bounds((uintptr_t)a, (uintptr_t)base, (uintptr_t)size);
}
/* The runtime allocates its stacks without separate guard mappings. */
int pthread_attr_getguardsize(const pthread_attr_t *a, size_t *size) { (void)a; *size = 0; return 0; }
extern int32_t v_pthread_atfork(uintptr_t, uintptr_t, uintptr_t);
int pthread_atfork(void (*prepare)(void), void (*parent)(void), void (*child)(void)) {
    return v_pthread_atfork((uintptr_t)prepare, (uintptr_t)parent, (uintptr_t)child);
}
