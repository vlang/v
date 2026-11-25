#ifndef V_RUNTIME_CANCELLATION_PROBE_H
#define V_RUNTIME_CANCELLATION_PROBE_H
#include <pthread.h>
#include <poll.h>
#include <stdatomic.h>

struct cancel_probe {
    int kind, fd, ready, release, resumed, done, failed, count;
    int order[4];
    pthread_mutex_t mutex;
    pthread_cond_t condition;
    pthread_t target;
};
static pthread_key_t cancel_probe_key;
static void cancel_inner(void *argument) {
    struct cancel_probe *s = argument;
    s->order[s->count++] = 1;
}
static void cancel_outer(void *argument) {
    struct cancel_probe *s = argument;
    if (s->kind == 6 && pthread_mutex_unlock(&s->mutex)) s->failed = 1;
    s->order[s->count++] = 2;
}
static void cancel_destructor(void *argument) {
    struct cancel_probe *s = argument;
    s->order[s->count++] = 3;
    __atomic_store_n(&s->done, 1, __ATOMIC_RELEASE);
}
static void *cancel_worker(void *argument) {
    struct cancel_probe *s = argument;
    if (pthread_setspecific(cancel_probe_key, s)) { s->failed = 1; return NULL; }
    if (s->kind == 3) {
        int old = -1;
        if (pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &old) || old != PTHREAD_CANCEL_ENABLE) s->failed = 1;
    }
    if (s->kind == 6 && pthread_mutex_lock(&s->mutex)) s->failed = 1;
    pthread_cleanup_push(cancel_outer, s);
    pthread_cleanup_push(cancel_inner, s);
    if (s->kind == 4 && pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL)) s->failed = 1;
    __atomic_store_n(&s->ready, 1, __ATOMIC_RELEASE);
    if (s->kind == 0) { char c; (void)read(s->fd, &c, 1); }
    else if (s->kind == 1) { char c; (void)recv(s->fd, &c, 1, 0); }
    else if (s->kind == 2) { struct pollfd p = {s->fd, POLLIN, 0}; (void)poll(&p, 1, -1); }
    else if (s->kind == 3) {
        while (!__atomic_load_n(&s->release, __ATOMIC_ACQUIRE)) { }
        int old = -1;
        if (pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old) || old != PTHREAD_CANCEL_DISABLE) s->failed = 1;
        __atomic_store_n(&s->resumed, 1, __ATOMIC_RELEASE);
        pthread_testcancel();
    } else if (s->kind == 4) {
        for (;;) __atomic_fetch_add(&s->resumed, 1, __ATOMIC_RELAXED);
    } else if (s->kind == 5) pthread_exit((void *)(uintptr_t)0x12345678);
    else if (s->kind == 6) {
        while (!s->release) (void)pthread_cond_wait(&s->condition, &s->mutex);
    }
    pthread_cleanup_pop(0);
    pthread_cleanup_pop(0);
    return NULL;
}
static void *cancel_join_worker(void *argument) {
    struct cancel_probe *s = argument;
    if (pthread_setspecific(cancel_probe_key, s)) { s->failed = 1; return NULL; }
    pthread_cleanup_push(cancel_outer, s);
    __atomic_store_n(&s->ready, 1, __ATOMIC_RELEASE);
    (void)pthread_join(s->target, NULL);
    pthread_cleanup_pop(0);
    return NULL;
}
static int cancel_wait(int *word) {
    struct timespec delay = {.tv_nsec = 1000000};
    for (int i = 0; i < 3000; ++i) {
        if (__atomic_load_n(word, __ATOMIC_ACQUIRE)) return 1;
        nanosleep(&delay, NULL);
    }
    return 0;
}
struct pending_wait {
    int kind, fd;
};
static void cancel_wait_handler(int number) { (void)number; }
static void *cancel_pending_wait(void *argument) {
    const struct pending_wait *wait = argument;
    struct timespec now = {0};
    sigset_t signals, empty;
    sigemptyset(&empty);
    sigemptyset(&signals);
    sigaddset(&signals, SIGUSR1);
    struct mmsghdr message = {0};
    int received;
    /* Leave a user signal pending so a missed cancellation cannot hang the test. */
    if (wait->kind >= 5 && (pthread_sigmask(SIG_BLOCK, &signals, NULL) ||
        pthread_kill(pthread_self(), SIGUSR1))) return NULL;
    if (pthread_cancel(pthread_self())) return NULL;
    switch (wait->kind) {
    case 0: (void)sigtimedwait(&signals, NULL, &now); break;
    case 1: (void)recvmsg(wait->fd, &message.msg_hdr, MSG_DONTWAIT); break;
    case 2: (void)sendmsg(wait->fd, &message.msg_hdr, MSG_DONTWAIT); break;
    case 3: (void)recvmmsg(wait->fd, &message, 1, MSG_DONTWAIT, &now); break;
    case 4: (void)sendmmsg(wait->fd, &message, 1, MSG_DONTWAIT); break;
    case 5: (void)sigwaitinfo(&signals, NULL); break;
    case 6: (void)sigwait(&signals, &received); break;
    case 7: (void)sigsuspend(&empty); break;
    }
    return NULL;
}
static int cancellation_native_probe(void) {
    int old = 77;
    errno = EDOM;
    CHECK(pthread_setcancelstate(77, &old) == EINVAL && old == 77 && errno == EDOM);
    CHECK(pthread_setcanceltype(77, &old) == EINVAL && old == 77 && errno == EDOM);
    int sockets[2];
    CHECK(!socketpair(AF_UNIX, SOCK_DGRAM, 0, sockets));
    struct sigaction action = {.sa_handler = cancel_wait_handler}, previous;
    sigemptyset(&action.sa_mask);
    CHECK(!sigaction(SIGUSR1, &action, &previous));
    for (int kind = 0; kind < 8; ++kind) {
        struct pending_wait pending = {kind, sockets[0]};
        pthread_t worker; void *result = NULL;
        CHECK(!pthread_create(&worker, NULL, cancel_pending_wait, &pending));
        CHECK(!pthread_join(worker, &result) && result == PTHREAD_CANCELED);
    }
    CHECK(!sigaction(SIGUSR1, &previous, NULL));
    CHECK(!close(sockets[0]) && !close(sockets[1]));
    CHECK(!pthread_key_create(&cancel_probe_key, cancel_destructor));
    for (int round = 0; round < 21; ++round) {
        struct cancel_probe s = {.kind = round % 7, .condition = PTHREAD_COND_INITIALIZER};
        pthread_mutexattr_t attr;
        CHECK(!pthread_mutexattr_init(&attr));
        CHECK(!pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK));
        CHECK(!pthread_mutex_init(&s.mutex, &attr));
        CHECK(!pthread_mutexattr_destroy(&attr));
        int pair[2];
        CHECK(s.kind == 1 ? !socketpair(AF_UNIX, SOCK_STREAM, 0, pair) : !pipe(pair));
        s.fd = pair[0]; pthread_t worker;
        CHECK(!pthread_create(&worker, NULL, cancel_worker, &s));
        /* Alternating requests before/after ready also exercise startup and
         * the pending-check/kernel-entry race, without relying on sleeps. */
        if (s.kind == 5) CHECK(cancel_wait(&s.done));
        else {
            if (round & 1 || s.kind == 3 || s.kind == 4 || s.kind == 6) CHECK(cancel_wait(&s.ready));
            errno = EDOM; CHECK(!pthread_cancel(worker) && errno == EDOM);
            if (s.kind == 3) __atomic_store_n(&s.release, 1, __ATOMIC_RELEASE);
            CHECK(cancel_wait(&s.done));
        }
        void *result = NULL; CHECK(!pthread_join(worker, &result));
        CHECK(result == (s.kind == 5 ? (void *)(uintptr_t)0x12345678 : PTHREAD_CANCELED));
        CHECK(!s.failed && s.count == 3 && s.order[0] == 1 && s.order[1] == 2 && s.order[2] == 3);
        if (s.kind == 3) CHECK(s.resumed == 1);
        CHECK(!close(pair[0]) && !close(pair[1]));
        CHECK(!pthread_cond_destroy(&s.condition) && !pthread_mutex_destroy(&s.mutex));
    }
    struct cancel_probe target = {0}, joiner = {0}; int pair[2];
    CHECK(!pipe(pair)); target.fd = pair[0];
    CHECK(!pthread_create(&joiner.target, NULL, cancel_worker, &target));
    CHECK(cancel_wait(&target.ready));
    pthread_t joining; CHECK(!pthread_create(&joining, NULL, cancel_join_worker, &joiner));
    CHECK(cancel_wait(&joiner.ready)); CHECK(!pthread_cancel(joining)); CHECK(cancel_wait(&joiner.done));
    void *result = NULL; CHECK(!pthread_join(joining, &result) && result == PTHREAD_CANCELED);
    CHECK(!joiner.failed && joiner.count == 2 && joiner.order[0] == 2 && joiner.order[1] == 3);
    CHECK(write(pair[1], "x", 1) == 1); CHECK(cancel_wait(&target.done));
    CHECK(!pthread_join(joiner.target, &result) && result == NULL);
    CHECK(!close(pair[0]) && !close(pair[1]));
    CHECK(!pthread_key_delete(cancel_probe_key));
    puts("pthread cancellation, blocking I/O, conditions, join recovery and cleanup order passed");
    return 0;
}
#endif
