#include <stdlib.h>
/* The namespaced hosted check uses ASan's allocator; the freestanding matrix
 * separately exercises V's actual heap and aligned-allocation ABI. */
void *v_freestanding_aligned_alloc(size_t alignment, size_t size) {
    void *pointer = NULL;
    if (alignment < sizeof(void *)) return malloc(size);
    return posix_memalign(&pointer, alignment, size) ? NULL : pointer;
}

/* V and the C adapters exchange owned buffers. Use ASan's allocator on both
 * sides while keeping the freestanding public allocation checks in the ABI. */
void *v_bare___malloc(size_t size) { return malloc(size); }
void *v_bare___calloc(size_t count, size_t size) { return calloc(count, size); }
void v_bare___free(void *pointer) { free(pointer); }
void *v_bare_realloc(void *pointer, size_t size) { return realloc(pointer, size); }
void *v_bare___malloc_align(size_t size, size_t alignment) {
    return v_freestanding_aligned_alloc(alignment, size);
}
