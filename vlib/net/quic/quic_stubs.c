// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// C helper functions for QUIC/ngtcp2 integration
// Provides callback implementations and crypto setup

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <ngtcp2/ngtcp2.h>
#include <ngtcp2/ngtcp2_crypto.h>
#include <ngtcp2/ngtcp2_crypto_ossl.h>
#include <openssl/ssl.h>
#include <openssl/rand.h>

// Storage for socket addresses used by the QUIC path.
// These persist for the lifetime of the connection.
static struct sockaddr_storage g_local_addr;
static struct sockaddr_storage g_remote_addr;
static socklen_t g_local_addrlen;
static socklen_t g_remote_addrlen;

// quic_resolve_and_set_path resolves the hostname:port and fills the
// ngtcp2_path structure with proper sockaddr data.
// Returns 0 on success, -1 on failure.
static int quic_resolve_and_set_path(ngtcp2_path *path,
                                     const char *hostname, int port) {
  struct addrinfo hints, *result;
  char port_str[16];

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_DGRAM;
  hints.ai_protocol = IPPROTO_UDP;

  snprintf(port_str, sizeof(port_str), "%d", port);

  if (getaddrinfo(hostname, port_str, &hints, &result) != 0) {
    return -1;
  }

  // Copy remote address
  memcpy(&g_remote_addr, result->ai_addr, result->ai_addrlen);
  g_remote_addrlen = (socklen_t)result->ai_addrlen;

  // Create a matching local address (any address, any port)
  memset(&g_local_addr, 0, sizeof(g_local_addr));
  if (result->ai_family == AF_INET6) {
    struct sockaddr_in6 *addr6 = (struct sockaddr_in6 *)&g_local_addr;
    addr6->sin6_family = AF_INET6;
    addr6->sin6_addr = in6addr_any;
    addr6->sin6_port = 0;
    g_local_addrlen = sizeof(struct sockaddr_in6);
  } else {
    struct sockaddr_in *addr4 = (struct sockaddr_in *)&g_local_addr;
    addr4->sin_family = AF_INET;
    addr4->sin_addr.s_addr = INADDR_ANY;
    addr4->sin_port = 0;
    g_local_addrlen = sizeof(struct sockaddr_in);
  }

  freeaddrinfo(result);

  // Set up the ngtcp2 path
  path->local.addr = (ngtcp2_sockaddr *)&g_local_addr;
  path->local.addrlen = g_local_addrlen;
  path->remote.addr = (ngtcp2_sockaddr *)&g_remote_addr;
  path->remote.addrlen = g_remote_addrlen;
  path->user_data = NULL;

  return 0;
}

// ngtcp2 rand callback: generates random bytes for non-cryptographic use
static void quic_rand_cb(uint8_t *dest, size_t destlen,
                         const ngtcp2_rand_ctx *rand_ctx) {
  (void)rand_ctx;
  RAND_bytes(dest, (int)destlen);
}

// ngtcp2 get_new_connection_id callback: generates new connection IDs
static int quic_get_new_connection_id_cb(ngtcp2_conn *conn, ngtcp2_cid *cid,
                                         uint8_t *token, size_t cidlen,
                                         void *user_data) {
  (void)conn;
  (void)user_data;
  if (RAND_bytes(cid->data, (int)cidlen) != 1) {
    return NGTCP2_ERR_CALLBACK_FAILURE;
  }
  cid->datalen = cidlen;
  if (RAND_bytes(token, NGTCP2_STATELESS_RESET_TOKENLEN) != 1) {
    return NGTCP2_ERR_CALLBACK_FAILURE;
  }
  return 0;
}

// quic_init_callbacks fills all required ngtcp2 callbacks for a client.
// This is done in C because V cannot directly assign C function pointers
// to struct fields typed as voidptr.
static void quic_init_callbacks(ngtcp2_callbacks *cb) {
  memset(cb, 0, sizeof(*cb));
  cb->client_initial = ngtcp2_crypto_client_initial_cb;
  cb->recv_crypto_data = ngtcp2_crypto_recv_crypto_data_cb;
  cb->encrypt = ngtcp2_crypto_encrypt_cb;
  cb->decrypt = ngtcp2_crypto_decrypt_cb;
  cb->hp_mask = ngtcp2_crypto_hp_mask_cb;
  cb->recv_retry = ngtcp2_crypto_recv_retry_cb;
  cb->update_key = ngtcp2_crypto_update_key_cb;
  cb->delete_crypto_aead_ctx = ngtcp2_crypto_delete_crypto_aead_ctx_cb;
  cb->delete_crypto_cipher_ctx = ngtcp2_crypto_delete_crypto_cipher_ctx_cb;
  cb->get_path_challenge_data = ngtcp2_crypto_get_path_challenge_data_cb;
  cb->version_negotiation = ngtcp2_crypto_version_negotiation_cb;
  cb->rand = quic_rand_cb;
  cb->get_new_connection_id = quic_get_new_connection_id_cb;
}

// conn_ref get_conn callback: retrieves ngtcp2_conn from conn_ref
static ngtcp2_conn *quic_get_conn_cb(ngtcp2_crypto_conn_ref *conn_ref) {
  return (ngtcp2_conn *)conn_ref->user_data;
}

// quic_setup_crypto sets up the complete crypto integration for a client
// connection. It creates the ossl_ctx, configures the SSL session, sets up
// a per-connection conn_ref, and attaches everything to the ngtcp2 connection.
// The caller must call quic_cleanup_crypto() to free the conn_ref when the
// connection is no longer needed.
// Returns 0 on success, negative value on failure.
static int quic_setup_crypto(ngtcp2_conn *conn, SSL *ssl,
                             const char *hostname) {
  ngtcp2_crypto_ossl_ctx *ossl_ctx = NULL;
  ngtcp2_crypto_conn_ref *conn_ref = NULL;

  if (conn == NULL) {
    return -10;
  }
  if (ssl == NULL) {
    return -11;
  }
  if (hostname == NULL || hostname[0] == '\0') {
    return -12;
  }

  // Initialize the crypto library
  if (ngtcp2_crypto_ossl_init() != 0) {
    return -1;
  }

  // Set SNI hostname for TLS
  SSL_set_tlsext_host_name(ssl, hostname);

  // Allocate per-connection conn_ref so multiple QUIC connections
  // can coexist without sharing global state.
  conn_ref = (ngtcp2_crypto_conn_ref *)malloc(sizeof(ngtcp2_crypto_conn_ref));
  if (conn_ref == NULL) {
    return -4;
  }
  conn_ref->get_conn = quic_get_conn_cb;
  conn_ref->user_data = (void *)conn;
  SSL_set_app_data(ssl, conn_ref);

  // Configure SSL for QUIC client (registers quic_tls_cbs)
  if (ngtcp2_crypto_ossl_configure_client_session(ssl) != 0) {
    free(conn_ref);
    return -2;
  }

  // Create ossl_ctx and attach SSL
  if (ngtcp2_crypto_ossl_ctx_new(&ossl_ctx, ssl) != 0) {
    free(conn_ref);
    return -3;
  }

  // Set TLS native handle on the connection
  ngtcp2_conn_set_tls_native_handle(conn, ossl_ctx);

  return 0;
}

// quic_cleanup_crypto frees the per-connection conn_ref allocated by
// quic_setup_crypto. Must be called when the QUIC connection is closed.
static void quic_cleanup_crypto(SSL *ssl) {
  if (ssl != NULL) {
    ngtcp2_crypto_conn_ref *conn_ref =
        (ngtcp2_crypto_conn_ref *)SSL_get_app_data(ssl);
    if (conn_ref != NULL) {
      free(conn_ref);
      SSL_set_app_data(ssl, NULL);
    }
  }
}

// Stub for SSL_provide_quic_data when QUIC-TLS is not available
#if !defined(OPENSSL_IS_BORINGSSL) && !defined(SSL_QUIC_METHOD_ST_H)

__attribute__((weak))
int SSL_provide_quic_data(void *ssl, int level, const uint8_t *data,
                          size_t len) {
  return 0;
}

__attribute__((weak))
int SSL_process_quic_post_handshake(void *ssl) {
  return 0;
}

#endif
