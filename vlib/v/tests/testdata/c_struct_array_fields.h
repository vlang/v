#ifndef V_TEST_C_STRUCT_ARRAY_FIELDS_H
#define V_TEST_C_STRUCT_ARRAY_FIELDS_H

#include <stdint.h>

enum { c_array_field_size = 8 };

typedef struct CArrayFieldRecord {
    unsigned char Data4[8];
    unsigned char Named[c_array_field_size];
    unsigned char Computed[c_array_field_size + 1];
    unsigned char Matrix[2][4];
} CArrayFieldRecord;

typedef union CArrayFieldUnion {
    unsigned char Bytes[8];
    uint32_t Words[2];
} CArrayFieldUnion;

static inline int c_array_field_sum(const CArrayFieldRecord *value) {
    return value->Data4[7] + value->Named[7] + value->Computed[8] + value->Matrix[1][3];
}

#endif
