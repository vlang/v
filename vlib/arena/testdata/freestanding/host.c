#define _GNU_SOURCE
#include <dlfcn.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>

typedef int (*probe_fn)(uint64_t, void (*)(void));
static pthread_barrier_t barrier;
static void checkpoint(void) { pthread_barrier_wait(&barrier); }
static void no_checkpoint(void) {}
struct work { probe_fn probe; uint64_t value; };
static void *worker(void *argument) {
    struct work *work = argument;
    return (void *)(uintptr_t)(work->probe(work->value, checkpoint) != 1);
}
int main(int argc, char **argv) {
    if (argc != 2) return 1;
    void *library = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (!library) { fprintf(stderr, "%s\n", dlerror()); return 2; }
    probe_fn probe = (probe_fn)dlsym(library, "v_arena_library_probe");
    if (!probe || pthread_barrier_init(&barrier, NULL, 3)) return 3;
    struct work work[2] = {{probe, 1}, {probe, 2}};
    pthread_t threads[2];
    for (int i = 0; i < 2; ++i) {
        if (pthread_create(&threads[i], NULL, worker, &work[i])) return 4;
    }
    if (probe(17, checkpoint) != 1) return 5;
    for (int i = 0; i < 2; ++i) {
        void *result;
        if (pthread_join(threads[i], &result) || result) return 6;
    }
    // A subsequent invocation begins with no stale top from prior calls.
    if (probe(23, no_checkpoint) != 1) return 7;
    pthread_barrier_destroy(&barrier);
    dlclose(library);
    puts("host threads arena passed");
    return 0;
}
