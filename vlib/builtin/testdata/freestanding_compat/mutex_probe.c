#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "probe.h"
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>

#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)
int v_freestanding_mutex_check(void) {
    pthread_mutexattr_t attributes;
    pthread_mutex_t mutex;
    int type, shared;
    CHECK(pthread_mutexattr_init(&attributes) == 0);
    CHECK(pthread_mutexattr_gettype(&attributes, &type) == 0 && type == PTHREAD_MUTEX_NORMAL);
    CHECK(pthread_mutexattr_getpshared(&attributes, &shared) == 0 && shared == PTHREAD_PROCESS_PRIVATE);
    CHECK(pthread_mutex_init(&mutex, &attributes) == 0 && pthread_mutex_lock(&mutex) == 0);
    struct timespec deadline;
    CHECK(clock_gettime(CLOCK_REALTIME, &deadline) == 0);
    deadline.tv_nsec += 20000000;
    if (deadline.tv_nsec >= 1000000000) { ++deadline.tv_sec; deadline.tv_nsec -= 1000000000; }
    errno = E2BIG;
    CHECK(pthread_mutex_timedlock(&mutex, &deadline) == ETIMEDOUT && errno == E2BIG);
    CHECK(pthread_mutex_unlock(&mutex) == 0 && pthread_mutex_destroy(&mutex) == 0);
    CHECK(pthread_mutexattr_settype(&attributes, PTHREAD_MUTEX_RECURSIVE) == 0);
    CHECK(pthread_mutex_init(&mutex, &attributes) == 0);
    CHECK(pthread_mutex_lock(&mutex) == 0 && pthread_mutex_lock(&mutex) == 0 && pthread_mutex_trylock(&mutex) == 0);
    CHECK(pthread_mutex_unlock(&mutex) == 0 && pthread_mutex_unlock(&mutex) == 0 && pthread_mutex_unlock(&mutex) == 0);
    CHECK(pthread_mutex_unlock(&mutex) == EPERM && pthread_mutex_destroy(&mutex) == 0);
    CHECK(pthread_mutexattr_settype(&attributes, PTHREAD_MUTEX_ERRORCHECK) == 0);
    CHECK(pthread_mutex_init(&mutex, &attributes) == 0 && pthread_mutex_lock(&mutex) == 0);
    CHECK(pthread_mutex_lock(&mutex) == EDEADLK && pthread_mutex_trylock(&mutex) == EBUSY);
    CHECK(pthread_mutex_unlock(&mutex) == 0 && pthread_mutex_unlock(&mutex) == EPERM);
    CHECK(pthread_mutex_destroy(&mutex) == 0);
    CHECK(pthread_mutexattr_settype(&attributes, 0x7fff) == EINVAL);
    CHECK(pthread_mutexattr_setpshared(&attributes, 0x7fff) == EINVAL);

    struct shared_counter { pthread_mutex_t mutex; unsigned int value; };
    struct shared_counter *counter = (void *)syscall(SYS_mmap, NULL, 4096, PROT_READ | PROT_WRITE,
                                                     MAP_SHARED | MAP_ANONYMOUS, -1, (off_t)0);
    CHECK(counter != MAP_FAILED);
    CHECK(pthread_mutexattr_settype(&attributes, PTHREAD_MUTEX_NORMAL) == 0);
    CHECK(pthread_mutexattr_setpshared(&attributes, PTHREAD_PROCESS_SHARED) == 0);
    CHECK(pthread_mutex_init(&counter->mutex, &attributes) == 0);
    long parent = syscall(SYS_getpid);
    long child = syscall(SYS_clone, SIGCHLD, 0, NULL, NULL, 0);
    CHECK(child >= 0);
    if (child == 0) {
        if (syscall(SYS_prctl, 1, SIGKILL, 0, 0, 0) || syscall(SYS_getppid) != parent) {
            syscall(SYS_exit_group, 1); __builtin_unreachable();
        }
    }
    for (unsigned int i = 0; i < 10000; ++i) {
        int error = pthread_mutex_lock(&counter->mutex);
        if (error && child == 0) { syscall(SYS_exit_group, 1); __builtin_unreachable(); }
        CHECK(!error);
        ++counter->value;
        CHECK(pthread_mutex_unlock(&counter->mutex) == 0);
    }
    if (child == 0) { syscall(SYS_exit_group, 0); __builtin_unreachable(); }
    int status = -1;
    long waited;
    do { waited = syscall(SYS_wait4, child, &status, 0, NULL); } while (waited < 0 && errno == EINTR);
    CHECK(waited == child && status == 0 && counter->value == 20000);
    CHECK(pthread_mutex_destroy(&counter->mutex) == 0);
    CHECK(syscall(SYS_munmap, counter, 4096) == 0);
    CHECK(pthread_mutexattr_destroy(&attributes) == 0);
    return 0;
}
