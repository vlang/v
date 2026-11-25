#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
extern int dynamic_base_step(int);
extern uintptr_t dynamic_base_address(void);
static __thread int initialized = 41;
static __thread int zero;
static int constructed;
static int finished;
__attribute__((constructor)) static void initialize(void) {
    const char *value = getenv("VLIBC_DYNAMIC_TEST");
    if (!value || strcmp(value, "from-loader-environment")) _Exit(91);
    char *pointer = malloc(128);
    if (!pointer) _Exit(92);
    memset(pointer, 1, 128);
    free(pointer);
    constructed++;
}
__attribute__((destructor)) static void finalize(void) {
    printf("shared finalized\n");
}
int dynamic_step(int iteration, void *allocation) {
    if (constructed != 1 || initialized != 41 + iteration || zero != iteration || !dynamic_base_step(iteration)) return 0;
    initialized++; zero++;
    errno = iteration;
    if (allocation) free(allocation);
    return errno == iteration;
}
uintptr_t dynamic_address(void) { return dynamic_base_address(); }
void dynamic_complete(void) { __atomic_add_fetch(&finished, 1, __ATOMIC_RELEASE); }
int dynamic_completed(void) { return __atomic_load_n(&finished, __ATOMIC_ACQUIRE); }

size_t dynamic_mapped_bytes(void) {
    FILE *maps = fopen("/proc/self/maps", "r");
    if (!maps) return 0;
    char line[512];
    size_t total = 0;
    while (fgets(line, sizeof line, maps)) {
        unsigned long long start, end;
        if (sscanf(line, "%llx-%llx", &start, &end) == 2) total += (size_t)(end - start);
    }
    fclose(maps);
    return total;
}
