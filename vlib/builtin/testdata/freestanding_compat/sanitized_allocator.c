#include <stdlib.h>
/* The namespaced hosted check uses ASan's allocator; the freestanding matrix
 * separately exercises V's actual heap and aligned-allocation ABI. */
void *v_freestanding_aligned_alloc(size_t alignment, size_t size) {
    void *pointer = NULL;
    if (alignment < sizeof(void *)) return malloc(size);
    return posix_memalign(&pointer, alignment, size) ? NULL : pointer;
}
