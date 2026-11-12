#ifdef V_GCBOEHM_NATIVE
#if defined(__APPLE__) && !defined(GC_DARWIN_THREADS)
#define GC_DARWIN_THREADS 1
#endif
#if defined(__TINYC__) && defined(__linux__)
#include <gc/gc.h>
#else
#include <gc.h>
#endif
#endif
#define V_SYNC_THREAD_SCOPE
#include "thread_helper.h"

#ifndef _WIN32
#include <stdint.h>
#include <string.h>

/* Keep pthread_key_t storage and declarations owned by the native C headers.
 * Convert the value instead of writing a possibly 32-bit key into a u64 slot:
 * on big-endian systems that would leave the key in the wrong half of the slot.
 * memcpy also avoids aliasing V's u64 with a different native uint64_t spelling. */
static pthread_key_t v_sync_tls_native_key(const void *storage) {
    uint64_t value;
    memcpy(&value, storage, sizeof(value));
    return (pthread_key_t)value;
}

int v_sync_tls_create(void *storage) {
    pthread_key_t key;
    int result = pthread_key_create(&key, NULL);
    if (result == 0) {
        uint64_t value = (uint64_t)key;
        memcpy(storage, &value, sizeof(value));
    }
    return result;
}

int v_sync_tls_delete(const void *storage) {
    return pthread_key_delete(v_sync_tls_native_key(storage));
}

int v_sync_tls_set(const void *storage, const void *value) {
    return pthread_setspecific(v_sync_tls_native_key(storage), value);
}

void *v_sync_tls_get(const void *storage) {
    return pthread_getspecific(v_sync_tls_native_key(storage));
}
#endif
