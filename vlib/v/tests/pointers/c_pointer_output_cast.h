#include <stdbool.h>
#include <stdint.h>

static void pointer_output_cast_probe(void **out) {
    static int64_t value = 37;
    *out = &value;
}

static uint32_t pointer_output_pair_read = 41;
static uint32_t pointer_output_pair_write = 73;

static bool pointer_output_pair_cast_probe(void **read, void **write) {
    *read = &pointer_output_pair_read;
    *write = &pointer_output_pair_write;
    return true;
}

static bool pointer_output_pair_cast_valid(void *read, void *write) {
    return read == &pointer_output_pair_read && write == &pointer_output_pair_write;
}
