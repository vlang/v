#define _GNU_SOURCE
#include <dlfcn.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
typedef int64_t (*probe_fn)(int64_t);
struct work { probe_fn probe; int64_t base; };
static void *worker(void *argument) {
    struct work *w = argument;
    for (int64_t i = 0; i < 32; ++i) {
        if (w->probe(w->base+i) != (w->base+i)*3+1) return (void *)(uintptr_t)1;
    }
    return NULL;
}
int main(int argc, char **argv) {
    if (argc != 2) return 1;
    void *library = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (!library) { fprintf(stderr, "%s\n", dlerror()); return 2; }
    probe_fn probe = (probe_fn)dlsym(library, "v_closure_library_probe");
    if (!probe) return 3;
    struct work work[2] = {{probe, 0}, {probe, 100}};
    pthread_t threads[2];
    for (int i=0; i<2; ++i) if (pthread_create(&threads[i], NULL, worker, &work[i])) return 4;
    if (probe(17) != 52) return 5;
    for (int i=0; i<2; ++i) { void *result; if (pthread_join(threads[i], &result) || result) return 6; }
    dlclose(library);
    puts("freestanding library captured closures passed");
    return 0;
}
