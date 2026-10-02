#include <stdint.h>

static void pointer_output_cast_probe(void **out) {
    static int64_t value = 37;
    *out = &value;
}
