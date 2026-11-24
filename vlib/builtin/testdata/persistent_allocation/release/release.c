#include "release.h"
#include <stdint.h>

static uintptr_t tracked;
static int releases;
void __real_free(void *pointer);

void heap_track_release(void *pointer) {
    tracked = (uintptr_t)pointer;
    releases = 0;
}

int heap_release_count(void) { return releases; }

void __wrap_free(void *pointer) {
    if (tracked && (uintptr_t)pointer == tracked) {
        releases++;
        if (releases > 1) return;
    }
    __real_free(pointer);
}
