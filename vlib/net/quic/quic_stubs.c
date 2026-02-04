// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Stub implementations for OpenSSL QUIC functions
// These are only available in OpenSSL 3.x compiled with --enable-quic
// The stubs allow compilation even when QUIC support is not available

#include <stddef.h>
#include <stdint.h>

// Check if QUIC functions are already available
// If not, provide weak symbols that return failure

#if !defined(OPENSSL_IS_BORINGSSL) && !defined(SSL_QUIC_METHOD_ST_H)

// Stub for SSL_provide_quic_data
__attribute__((weak))
int SSL_provide_quic_data(void *ssl, int level, const uint8_t *data, size_t len) {
	// Return 0 to indicate failure - QUIC not supported
	return 0;
}

// Stub for SSL_process_quic_post_handshake  
__attribute__((weak))
int SSL_process_quic_post_handshake(void *ssl) {
	// Return 0 to indicate failure - QUIC not supported
	return 0;
}

#endif
