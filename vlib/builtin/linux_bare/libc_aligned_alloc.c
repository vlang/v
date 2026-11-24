#include "libc_aligned_alloc.h"
#include <stdlib.h>
#include <errno.h>

void *aligned_alloc(size_t alignment, size_t size) {
    if (!alignment || (alignment & (alignment - 1)) || size % alignment) {
        errno = EINVAL;
        return NULL;
    }
    void *result = v_freestanding_aligned_alloc(alignment, size);
    if (!result) errno = ENOMEM;
    return result;
}
