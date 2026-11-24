#define _GNU_SOURCE
#include <dlfcn.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>

typedef int (*probe_fn)(uint64_t, int);

struct work {
    probe_fn probe;
    uint64_t value;
    pthread_barrier_t *barrier;
};

static void *worker(void *argument) {
    struct work *work = argument;
    int initialized = work->probe(work->value, 0);
    if (work->barrier) pthread_barrier_wait(work->barrier);
    if (initialized != 1) return (void *)(uintptr_t)1;
    if (work->probe(work->value, 1) != 1) return (void *)(uintptr_t)2;
    return NULL;
}

int main(int argc, char **argv) {
    if (argc != 2) return 1;
    void *library = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (!library) { fprintf(stderr, "%s\n", dlerror()); return 2; }
    probe_fn probe = (probe_fn)dlsym(library, "v_tls_library_probe");
    if (!probe || probe(17, 0) != 1) return 3;
    pthread_barrier_t barrier;
    if (pthread_barrier_init(&barrier, NULL, 3)) return 4;
    struct work work[2] = {{probe, 1, &barrier}, {probe, 2, &barrier}};
    pthread_t threads[2];
    for (int i = 0; i < 2; ++i) {
        if (pthread_create(&threads[i], NULL, worker, &work[i])) return 5;
    }
    pthread_barrier_wait(&barrier);
    if (probe(17, 1) != 1) return 6;
    for (int i = 0; i < 2; ++i) {
        void *result;
        if (pthread_join(threads[i], &result) || result) return 7;
    }
    struct work fresh = {probe, 3, NULL};
    pthread_t thread;
    void *result;
    if (pthread_create(&thread, NULL, worker, &fresh)) return 8;
    if (pthread_join(thread, &result) || result) return 9;
    if (probe(17, 1) != 1) return 10;
    pthread_barrier_destroy(&barrier);
    dlclose(library);
    puts("host threads TLS passed");
    return 0;
}
