#ifndef V_FREESTANDING_TLS_PROBE_H
#define V_FREESTANDING_TLS_PROBE_H

#include <stdint.h>

static _Thread_local volatile uint64_t tls_probe_initialized = UINT64_C(0x13579bdf2468ace0);
static _Thread_local volatile unsigned char tls_probe_zeroes[257] __attribute__((aligned(64)));

static int tls_probe_pristine(void) {
    if (tls_probe_initialized != UINT64_C(0x13579bdf2468ace0)) return 0;
    if ((uintptr_t)tls_probe_zeroes % 64 != 0) return 0;
    for (unsigned int i = 0; i < sizeof tls_probe_zeroes; ++i) {
        if (tls_probe_zeroes[i] != 0) return 0;
    }
    return 1;
}

static void tls_probe_store(uint64_t value) {
    tls_probe_initialized = value;
    for (unsigned int i = 0; i < sizeof tls_probe_zeroes; ++i) {
        tls_probe_zeroes[i] = (unsigned char)(value + i);
    }
}

static int tls_probe_matches(uint64_t value) {
    if (tls_probe_initialized != value) return 0;
    for (unsigned int i = 0; i < sizeof tls_probe_zeroes; ++i) {
        if (tls_probe_zeroes[i] != (unsigned char)(value + i)) return 0;
    }
    return 1;
}

#endif
