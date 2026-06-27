/*
 * mbedtls_threading.h - Win32 mutex callbacks for MBEDTLS_THREADING_ALT.
 *
 * On Windows, mbedtls is built with MBEDTLS_THREADING_C + MBEDTLS_THREADING_ALT
 * (see thirdparty/mbedtls/include/mbedtls/mbedtls_config.h). The library then
 * expects the embedder to supply the four mutex primitives via
 * mbedtls_threading_set_alt(). These wrap a Win32 CRITICAL_SECTION; the matching
 * mbedtls_threading_mutex_t type is defined in
 * thirdparty/mbedtls/include/mbedtls/threading_alt.h.
 *
 * v_mbedtls_threading_setup() is called once from net.mbedtls' init() before any
 * TLS use. On every other platform it is a no-op (pthread threading, or none).
 */
#ifndef V_MBEDTLS_THREADING_H
#define V_MBEDTLS_THREADING_H

#if defined(_WIN32) && defined(MBEDTLS_THREADING_ALT)

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <mbedtls/threading.h>
#include <mbedtls/error.h>

static void v_mbedtls_mutex_init(mbedtls_threading_mutex_t *m) {
    InitializeCriticalSection(&m->cs);
    m->is_valid = 1;
}

static void v_mbedtls_mutex_free(mbedtls_threading_mutex_t *m) {
    if (m->is_valid) {
        DeleteCriticalSection(&m->cs);
        m->is_valid = 0;
    }
}

static int v_mbedtls_mutex_lock(mbedtls_threading_mutex_t *m) {
    if (!m->is_valid) {
        return MBEDTLS_ERR_THREADING_BAD_INPUT_DATA;
    }
    EnterCriticalSection(&m->cs);
    return 0;
}

static int v_mbedtls_mutex_unlock(mbedtls_threading_mutex_t *m) {
    if (!m->is_valid) {
        return MBEDTLS_ERR_THREADING_BAD_INPUT_DATA;
    }
    LeaveCriticalSection(&m->cs);
    return 0;
}

static void v_mbedtls_threading_setup(void) {
    mbedtls_threading_set_alt(v_mbedtls_mutex_init, v_mbedtls_mutex_free,
        v_mbedtls_mutex_lock, v_mbedtls_mutex_unlock);
}

#else /* not (Windows + THREADING_ALT) */

static void v_mbedtls_threading_setup(void) { }

#endif

#endif /* V_MBEDTLS_THREADING_H */
