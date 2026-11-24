#include "probe.h"
#include "../../macos_bare/kernel_services.h"
#include <stddef.h>

/* The probe deliberately supplies its own compiler-emitted memory helpers. */
void *memset(void *target, int value, size_t size) {
    unsigned char *bytes = target;
    for (size_t i = 0; i < size; ++i) bytes[i] = (unsigned char)value;
    return target;
}
void *memcpy(void *target, const void *source, size_t size) {
    unsigned char *out = target;
    const unsigned char *in = source;
    for (size_t i = 0; i < size; ++i) out[i] = in[i];
    return target;
}

int probe_main(void) { return v_macos_kernel_services_probe(); }

uint64_t v_macos_probe_tid(void) {
    uint64_t result = 0;
    return v_macos_kernel_bsd_call6(372, 0, 0, 0, 0, 0, 0, &result) == 0 ? result : 0;
}
