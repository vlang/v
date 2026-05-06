// NOTE: HTTP/3 support is experimental
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

// Per-connection address storage for the QUIC path.
// Each connection allocates its own QuicPathAddrs so that multiple
// connections can coexist without sharing global state.
typedef struct {
  struct sockaddr_storage local_addr;
  struct sockaddr_storage remote_addr;
  socklen_t               local_addrlen;
  socklen_t               remote_addrlen;
} QuicPathAddrs;

// quic_resolve_and_set_path resolves the hostname:port, writes the resulting
// sockaddr data into the caller-provided QuicPathAddrs, and fills the
// ngtcp2_path structure with pointers into that per-connection storage.
// The caller must keep addrs alive for the lifetime of the connection.
// Returns 0 on success, -1 on failure.
static int quic_resolve_and_set_path(ngtcp2_path *path,
                                     QuicPathAddrs *addrs,
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

  // Copy remote address into per-connection storage
  memcpy(&addrs->remote_addr, result->ai_addr, result->ai_addrlen);
  addrs->remote_addrlen = (socklen_t)result->ai_addrlen;

  // Create a matching local address (any address, any port)
  memset(&addrs->local_addr, 0, sizeof(addrs->local_addr));
  if (result->ai_family == AF_INET6) {
    struct sockaddr_in6 *addr6 = (struct sockaddr_in6 *)&addrs->local_addr;
    addr6->sin6_family = AF_INET6;
    addr6->sin6_addr = in6addr_any;
    addr6->sin6_port = 0;
    addrs->local_addrlen = sizeof(struct sockaddr_in6);
  } else {
    struct sockaddr_in *addr4 = (struct sockaddr_in *)&addrs->local_addr;
    addr4->sin_family = AF_INET;
    addr4->sin_addr.s_addr = INADDR_ANY;
    addr4->sin_port = 0;
    addrs->local_addrlen = sizeof(struct sockaddr_in);
  }

  freeaddrinfo(result);

  // Set up the ngtcp2 path with pointers into per-connection storage
  path->local.addr = (ngtcp2_sockaddr *)&addrs->local_addr;
  path->local.addrlen = addrs->local_addrlen;
  path->remote.addr = (ngtcp2_sockaddr *)&addrs->remote_addr;
  path->remote.addrlen = addrs->remote_addrlen;
  path->user_data = NULL;

  return 0;
}

// ngtcp2 rand callback: generates random bytes for non-cryptographic use
static void quic_rand_cb(uint8_t *dest, size_t destlen,
                         const ngtcp2_rand_ctx *rand_ctx) {
  (void)rand_ctx;
  if (RAND_bytes(dest, (int)destlen) != 1) {
    /* OpenSSL CSPRNG failed — use platform fallback */
    #if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__)
    arc4random_buf(dest, destlen);
    #else
    memset(dest, 0, destlen);
    #endif
  }
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

// QuicStreamEvents holds pending stream events for V-side processing.
// Must match the QuicStreamEvents struct in ngtcp2.c.v exactly.
#define QUIC_MAX_PENDING_EVENTS 64
#define QUIC_MAX_RECV_DATA_EVENTS 64
#define QUIC_RECV_DATA_BUF_SIZE 65536
typedef struct {
  int64_t fin_stream_ids[QUIC_MAX_PENDING_EVENTS];
  int     fin_count;
  int64_t closed_stream_ids[QUIC_MAX_PENDING_EVENTS];
  int     closed_count;
  int     overflow;
  // Per-chunk metadata for received stream data
  int64_t recv_stream_ids[QUIC_MAX_RECV_DATA_EVENTS];
  int     recv_offsets[QUIC_MAX_RECV_DATA_EVENTS];
  int     recv_lengths[QUIC_MAX_RECV_DATA_EVENTS];
  int     recv_count;
  // Shared flat buffer holding received data bytes
  uint8_t recv_data_buf[QUIC_RECV_DATA_BUF_SIZE];
  int     recv_data_buf_used;
} QuicStreamEvents;

// ngtcp2 recv_stream_data callback: called when stream data is received.
// Buffers received data into QuicStreamEvents for V-side processing.
// When FIN is signaled (flags & 0x01), records the FIN event separately.
static int quic_recv_stream_data_cb(ngtcp2_conn *conn, uint32_t flags,
    int64_t stream_id, uint64_t offset,
    const uint8_t *data, size_t datalen,
    void *user_data, void *stream_user_data) {
  (void)conn;
  (void)offset;
  (void)stream_user_data;

  if (user_data == NULL) {
    return 0;
  }

  QuicStreamEvents *events = (QuicStreamEvents *)user_data;

  // Buffer received data if there is any
  if (data != NULL && datalen > 0) {
    if (events->recv_count < QUIC_MAX_RECV_DATA_EVENTS &&
        events->recv_data_buf_used + (int)datalen <= QUIC_RECV_DATA_BUF_SIZE) {
      int idx = events->recv_count;
      events->recv_stream_ids[idx] = stream_id;
      events->recv_offsets[idx] = events->recv_data_buf_used;
      events->recv_lengths[idx] = (int)datalen;
      memcpy(&events->recv_data_buf[events->recv_data_buf_used], data, datalen);
      events->recv_data_buf_used += (int)datalen;
      events->recv_count++;
    } else {
      events->overflow = 1;
    }
  }

  // Record FIN event
  if (flags & NGTCP2_STREAM_DATA_FLAG_FIN) {
    if (events->fin_count < QUIC_MAX_PENDING_EVENTS) {
      events->fin_stream_ids[events->fin_count++] = stream_id;
    } else {
      events->overflow = 1;
    }
  }
  return 0;
}

// ngtcp2 stream_close callback: called when a stream is closed.
// Records the close event in QuicStreamEvents for V-side processing.
static int quic_stream_close_cb(ngtcp2_conn *conn, uint32_t flags,
    int64_t stream_id, uint64_t app_error_code,
    void *user_data, void *stream_user_data) {
  (void)conn;
  (void)flags;
  (void)app_error_code;
  (void)stream_user_data;
  if (user_data != NULL) {
    QuicStreamEvents *events = (QuicStreamEvents *)user_data;
    if (events->closed_count < QUIC_MAX_PENDING_EVENTS) {
      events->closed_stream_ids[events->closed_count++] = stream_id;
    } else {
      events->overflow = 1;
    }
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
  cb->recv_stream_data = quic_recv_stream_data_cb;
  cb->stream_close = quic_stream_close_cb;
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
