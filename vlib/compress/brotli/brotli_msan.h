#ifndef V_COMPRESS_BROTLI_MSAN_H
#define V_COMPRESS_BROTLI_MSAN_H

#include <stddef.h>

#if defined(__has_feature)
#if !__has_feature(memory_sanitizer)
#define V_BROTLI_NEEDS_NOOP
#endif
#else
#define V_BROTLI_NEEDS_NOOP
#endif

#ifdef V_BROTLI_NEEDS_NOOP
static inline void v_brotli_unpoison_noop(const volatile void *data, size_t len) {
    (void)data;
    (void)len;
}
#endif

#endif
