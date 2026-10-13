#ifndef V_TEST_C_LIBC_FIELDS_H
#define V_TEST_C_LIBC_FIELDS_H

#include <stdint.h>

typedef struct LibcFieldRecord {
    uint64_t index;
    uint64_t select;
    uint64_t malloc;
    uint64_t exit;
    uint64_t byte;
    uint64_t int_str;
    uint64_t v_index;
} LibcFieldRecord;

typedef union LibcFieldUnion {
    uint64_t index;
    uint64_t select;
} LibcFieldUnion;

#endif
