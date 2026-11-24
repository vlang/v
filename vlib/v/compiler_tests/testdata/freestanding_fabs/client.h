#ifndef V_FREESTANDING_FABS_CLIENT_H
#define V_FREESTANDING_FABS_CLIENT_H

#include <math.h>
#include <stdint.h>

/* Volatile indirection forces the C client to call the exported symbol rather
 * than having the compiler replace a direct fabs call with a builtin. */
static double (*volatile v_fabs_client_function)(double) = fabs;

static uint64_t v_fabs_client_bits(uint64_t input) {
    union { uint64_t bits; double value; } source = { .bits = input };
    union { uint64_t bits; double value; } result;
    result.value = v_fabs_client_function(source.value);
    return result.bits;
}

#endif
