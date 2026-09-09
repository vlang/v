/*
 * threading_alt.h - mbedtls_threading_mutex_t for MBEDTLS_THREADING_ALT.
 *
 * V provides this on Windows (which has no pthreads) so that
 * MBEDTLS_THREADING_C can be enabled. The mutex is a Win32 CRITICAL_SECTION;
 * the callbacks that operate on it live in
 * vlib/net/mbedtls/mbedtls_threading.h and are installed once at startup via
 * mbedtls_threading_set_alt() from the net.mbedtls module init().
 */
#ifndef MBEDTLS_THREADING_ALT_H
#define MBEDTLS_THREADING_ALT_H

#include <windows.h>

typedef struct mbedtls_threading_mutex_t {
    CRITICAL_SECTION cs;
    char is_valid;
} mbedtls_threading_mutex_t;

#endif /* MBEDTLS_THREADING_ALT_H */
