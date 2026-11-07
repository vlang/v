#include <stdint.h>
#include <stdlib.h>

static int v_flat_test_dense_count;

static int v_flat_test_dense_attempts(void) {
    return v_flat_test_dense_count;
}

static void *v_flat_test_calloc(size_t count, size_t size) {
    if ((count == ((size_t)1 << 24) + 1 || count == ((size_t)1 << 20) + 1)
        && (size == sizeof(uint64_t) || size == sizeof(void *))) {
        int attempt = ++v_flat_test_dense_count;
        const char *failure = getenv("V_FLAT_TEST_DENSE_FAILURE");
        if (failure && (failure[0] == 'b'
            || (failure[0] == 'f' && attempt == 1)
            || (failure[0] == 's' && attempt == 2))) {
            return NULL;
        }
    }
    return calloc(count, size);
}

#define calloc v_flat_test_calloc
