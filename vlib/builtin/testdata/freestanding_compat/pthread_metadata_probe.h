#ifndef V_RUNTIME_PTHREAD_METADATA_PROBE_H
#define V_RUNTIME_PTHREAD_METADATA_PROBE_H
#include <sched.h>
#include <sys/syscall.h>

struct pthread_metadata_state {
    int ready, release, failed;
    pthread_t identity;
    pthread_t foreign;
};
static void *pthread_metadata_worker(void *argument) {
    struct pthread_metadata_state *s = argument;
    s->identity = pthread_self();
    char name[16];
    if (pthread_setname_np(s->identity, "worker-init") || pthread_getname_np(s->identity, name, sizeof(name)) || strcmp(name, "worker-init")) s->failed = 1;
    if (s->foreign && (pthread_setname_np(s->foreign, "fork-owner") || pthread_getname_np(s->foreign, name, sizeof(name)) || strcmp(name, "fork-owner"))) s->failed = 1;
    __atomic_store_n(&s->ready, 1, __ATOMIC_RELEASE);
    struct timespec delay = {.tv_nsec = 1000000};
    while (!__atomic_load_n(&s->release, __ATOMIC_ACQUIRE)) nanosleep(&delay, NULL);
    return NULL;
}
struct pthread_guard_probe {
    size_t page, requested;
    uintptr_t base;
    size_t guard;
};

static int pthread_guard_check(struct pthread_guard_probe *probe) {
    pthread_attr_t attr;
    size_t size;
    void *base;
    char local;
    CHECK(!pthread_getattr_np(pthread_self(), &attr));
    CHECK(!pthread_attr_getstack(&attr, &base, &size));
    CHECK(!pthread_attr_getguardsize(&attr, &probe->guard));
    CHECK(!pthread_attr_destroy(&attr));
    probe->base = (uintptr_t)base;
    CHECK((uintptr_t)&local >= probe->base && (uintptr_t)&local - probe->base < size);
    size_t rounded = (probe->requested + probe->page - 1) / probe->page * probe->page;
    CHECK(probe->guard >= probe->requested && probe->guard <= rounded);
#ifndef V_RUNTIME_HOSTED_ORACLE
    CHECK(probe->guard == rounded && size >= (1U << 20));
    if (rounded) {
        FILE *maps = fopen("/proc/self/maps", "r");
        CHECK(maps);
        int protected = 0;
        char line[512], permissions[5];
        unsigned long start, end;
        while (fgets(line, sizeof line, maps)) {
            if (sscanf(line, "%lx-%lx %4s", &start, &end, permissions) == 3 &&
                start <= probe->base - rounded && end >= probe->base &&
                !strncmp(permissions, "---", 3)) protected = 1;
        }
        CHECK(!fclose(maps) && protected);
    }
#endif
    return 0;
}

static void *pthread_guard_worker(void *argument) {
    return (void *)(uintptr_t)pthread_guard_check(argument);
}

static int pthread_guard_probe(void) {
    long page = sysconf(_SC_PAGESIZE);
    CHECK(page > 0);
    pthread_attr_t attr;
    size_t initial;
    CHECK(!pthread_attr_init(&attr));
    CHECK(!pthread_attr_getguardsize(&attr, &initial));
#ifndef V_RUNTIME_HOSTED_ORACLE
    CHECK(initial == (size_t)page);
#endif
    CHECK(!pthread_attr_setstacksize(&attr, 1U << 20));
    size_t requests[] = {initial, 2 * (size_t)page + 1, 0};
    for (unsigned i = 0; i < sizeof requests / sizeof requests[0]; ++i) {
        if (i) CHECK(!pthread_attr_setguardsize(&attr, requests[i]));
        size_t stored;
        CHECK(!pthread_attr_getguardsize(&attr, &stored) && stored == requests[i]);
        struct pthread_guard_probe probe = {.page = (size_t)page, .requested = stored};
        pthread_t thread;
        void *result;
        CHECK(!pthread_create(&thread, i ? &attr : NULL, pthread_guard_worker, &probe));
        CHECK(!pthread_join(thread, &result) && !result);
#ifndef V_RUNTIME_HOSTED_ORACLE
        if (probe.guard) {
            unsigned char residency;
            errno = 0;
            CHECK(syscall(SYS_mincore, (void *)(probe.base - probe.guard),
                          probe.page, &residency) == -1 && errno == ENOMEM);
        }
#endif
    }
    CHECK(!pthread_attr_destroy(&attr));
    return 0;
}
static int pthread_metadata_native_probe(void) {
    CHECK(!pthread_guard_probe());
    pthread_t self = pthread_self(); char saved[16], name[32];
    CHECK(!pthread_getname_np(self, saved, sizeof(saved)));
    errno = EDOM;
    CHECK(!pthread_setname_np(self, "native-main") && errno == EDOM);
    CHECK(!pthread_getname_np(self, name, sizeof(name)) && !strcmp(name, "native-main") && errno == EDOM);
    memset(name, 'X', sizeof(name));
    CHECK(pthread_getname_np(self, name, 15) == ERANGE && name[0] == 'X' && name[15] == 'X' && errno == EDOM);
    CHECK(pthread_setname_np(self, "0123456789abcdef") == ERANGE && errno == EDOM);
    CHECK(!pthread_getname_np(self, name, sizeof(name)) && !strcmp(name, "native-main"));
    int policy = -1; struct sched_param priority = {.sched_priority = -1};
    CHECK(!pthread_getschedparam(self, &policy, &priority) && policy == sched_getscheduler(0));
    struct sched_param kernel_priority;
    CHECK(!sched_getparam(0, &kernel_priority) && kernel_priority.sched_priority == priority.sched_priority);
    CHECK(!sched_setparam(0, &kernel_priority) && !sched_setscheduler(0, policy, &kernel_priority));
    CHECK(sched_get_priority_min(SCHED_OTHER) == 0 && sched_get_priority_max(SCHED_OTHER) == 0);
    CHECK(sched_get_priority_min(SCHED_FIFO) > 0 && sched_get_priority_max(SCHED_FIFO) >= sched_get_priority_min(SCHED_FIFO));
    struct timespec interval; CHECK(!sched_rr_get_interval(0, &interval));
    CHECK(interval.tv_sec >= 0 && interval.tv_nsec >= 0 && interval.tv_nsec < 1000000000);
    errno = 0; CHECK(sched_get_priority_min(-1) == -1 && errno == EINVAL);
    errno = EDOM; int current_nice = getpriority(PRIO_PROCESS, 0);
    CHECK(current_nice >= -20 && current_nice <= 19 && errno == EDOM);
    CHECK(!setpriority(PRIO_PROCESS, 0, current_nice) && nice(0) == current_nice && errno == EDOM);
    errno = 0; CHECK(getpriority(12345, 0) == -1 && errno == EINVAL);
    CHECK(!pthread_setschedparam(self, policy, &priority) && !pthread_setschedprio(self, priority.sched_priority));
    struct sched_param invalid = {.sched_priority = -1};
    CHECK(pthread_setschedparam(self, SCHED_OTHER, &invalid) == EINVAL && pthread_setschedprio(self, -1) == EINVAL);
    cpu_set_t current, actual; memset(&current, 0, sizeof(current));
    CHECK(!pthread_getaffinity_np(self, sizeof(current), &current) && CPU_COUNT(&current) > 0);
    CHECK(!pthread_setaffinity_np(self, sizeof(current), &current));
    memset(&actual, 0xff, sizeof(actual));
    CHECK(!pthread_getaffinity_np(self, sizeof(actual), &actual) && !memcmp(&current, &actual, sizeof(current)));
    CHECK(pthread_getaffinity_np(self, 0, &actual) == EINVAL);
    CPU_ZERO(&actual); CHECK(pthread_setaffinity_np(self, sizeof(actual), &actual) == EINVAL);
    struct pthread_metadata_state state = {0}; pthread_t worker;
    CHECK(!pthread_create(&worker, NULL, pthread_metadata_worker, &state));
    CHECK(cancel_wait(&state.ready) && !state.failed && pthread_equal(worker, state.identity));
    const char *names[] = {"foreign-name", "line\nname", "Ж-thread", "", "0123456789abcde"};
    for (unsigned i = 0; i < sizeof(names) / sizeof(*names); ++i) {
        errno = EDOM;
        CHECK(!pthread_setname_np(worker, names[i]) && errno == EDOM);
        memset(name, 'X', sizeof(name));
        CHECK(!pthread_getname_np(worker, name, sizeof(name)) && !strcmp(name, names[i]) && errno == EDOM);
    }
    CHECK(!pthread_getschedparam(worker, &policy, &priority));
    CHECK(!pthread_setschedparam(worker, policy, &priority) && !pthread_setschedprio(worker, priority.sched_priority));
    CHECK(!pthread_getaffinity_np(worker, sizeof(actual), &actual) && !memcmp(&current, &actual, sizeof(current)));
    CHECK(!pthread_setaffinity_np(worker, sizeof(current), &current));
    __atomic_store_n(&state.release, 1, __ATOMIC_RELEASE);
    CHECK(!pthread_join(worker, NULL));
    CHECK(!pthread_setname_np(self, saved));
    /* The fork child retains the calling pthread identity but has a new kernel
     * TID. A new worker must be able to query/change that surviving identity. */
    pid_t child = fork(); CHECK(child >= 0);
    if (!child) {
        struct pthread_metadata_state fork_state = {.foreign = self}; pthread_t other;
        if (pthread_create(&other, NULL, pthread_metadata_worker, &fork_state)) _exit(91);
        if (!cancel_wait(&fork_state.ready) || fork_state.failed) _exit(92);
        if (pthread_setname_np(self, "fork-main") || pthread_getschedparam(self, &policy, &priority)) _exit(93);
        __atomic_store_n(&fork_state.release, 1, __ATOMIC_RELEASE);
        if (pthread_join(other, NULL)) _exit(94);
        int wanted = current_nice < 19 ? current_nice + 1 : 19;
        if (nice(1) != wanted || getpriority(PRIO_PROCESS, 0) != wanted) _exit(95);
#ifndef V_RUNTIME_HOSTED_ORACLE
        /* GNU nice adds its int arguments before calling the kernel and
         * overflows for INT_MAX. Keep the native full-range clamp regression
         * separately from the common, well-defined hosted comparison above. */
        if (nice(2147483647) != 19 || getpriority(PRIO_PROCESS, 0) != 19) _exit(96);
#endif
        _exit(0);
    }
    int status = 0; pid_t waited = waitpid(child, &status, 0);
    if (waited != child || !WIFEXITED(status) || WEXITSTATUS(status)) fprintf(stderr, "metadata fork child status %d\n", status);
    CHECK(waited == child && WIFEXITED(status) && !WEXITSTATUS(status));
    puts("pthread names, scheduling, niceness, affinity and fork identity passed");
    return 0;
}
#endif
