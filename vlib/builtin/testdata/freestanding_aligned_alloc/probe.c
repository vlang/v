#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

int aligned_alloc_probe(void) {
    void *(*allocate)(size_t, size_t) = aligned_alloc;
    const size_t alignments[] = {1, 2, 4, 8, 16, 64, 4096, 65536};
    for (size_t i = 0; i < sizeof alignments / sizeof alignments[0]; i++) {
        size_t alignment = alignments[i], size = alignment * 2;
        errno = 73;
        unsigned char *bytes = allocate(alignment, size);
        if (!bytes || (uintptr_t)bytes % alignment || errno != 73) return 1;
        for (size_t j = 0; j < size; j++) bytes[j] = (unsigned char)(j % 251);
        unsigned char *grown = realloc(bytes, size * 2);
        if (!grown) return 2;
        for (size_t j = 0; j < size; j++) if (grown[j] != (unsigned char)(j % 251)) return 3;
        free(grown);
    }
    if (allocate(0, 0) || errno != EINVAL) return 4;
    if (allocate(3, 12) || errno != EINVAL) return 5;
    if (allocate(64, 65) || errno != EINVAL) return 6;
    if (allocate(8, SIZE_MAX - 7) || errno != ENOMEM) return 7;
    /* C11 permits a zero-size allocation to return either NULL or a freeable
     * unique pointer. The bare allocator currently chooses the latter. */
    void *empty = allocate(64, 0);
    free(empty);
    return 0;
}
