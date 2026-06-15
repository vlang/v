#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <winsock.h>
#include <wincrypt.h>
#include <wintrust.h>
#include <schannel.h>

#define SECURITY_WIN32
#include <security.h>
#include <sspi.h>

#define vsc_init_resp_buff_size 44000

#define IO_BUFFER_SIZE  0x10000

#define TLS_MAX_BUFSIZ      32768

// Define here to be sure
#define SP_PROT_TLS1_2_CLIENT 0x00000800

typedef void* (*vschannel_allocator)(void*, INT64);

typedef struct TlsContext TlsContext;

TlsContext new_tls_context();

// ALPN (RFC 7301) support. `wire` is the standard ALPN wire format: each
// protocol name preceded by a 1-byte length, e.g. "\x02h2\x08http/1.1".
// vschannel_alpn_supported reports whether this Windows version's SChannel
// can advertise ALPN at all (Windows 8.1+).
INT vschannel_alpn_supported();
void vschannel_set_alpn(TlsContext *tls_ctx, const char *wire, INT len);
INT vschannel_get_alpn(TlsContext *tls_ctx, char *out, INT out_cap);
INT vschannel_alpn_probe(TlsContext *tls_ctx, INT iport, LPWSTR host, char *out, INT out_cap);

// vschannel_request_on_open runs a one-shot HTTP/1.1 request over an already
// open connection (the HTTP/1.1 fallback when `h2` was not negotiated).
INT vschannel_request_on_open(TlsContext *tls_ctx, CHAR *req, DWORD req_len, CHAR **out, vschannel_allocator afn);

// Streaming transport for the HTTP/2 driver: open the connection and keep it
// open, then move raw application bytes across it. See vschannel.c.
INT vschannel_h2_connect(TlsContext *tls_ctx, INT iport, LPWSTR host);
INT vschannel_write(TlsContext *tls_ctx, const char *buf, INT len);
INT vschannel_read(TlsContext *tls_ctx, char *buf, INT cap);
void vschannel_h2_close(TlsContext *tls_ctx);

static void vschannel_init(TlsContext *tls_ctx, BOOL validate_server_certificate);

static void vschannel_cleanup(TlsContext *tls_ctx);

static INT vschannel_last_error(TlsContext *tls_ctx);

static INT request(TlsContext *tls_ctx, INT iport, LPWSTR host, CHAR *req, DWORD req_len, CHAR **out, vschannel_allocator afn);

static SECURITY_STATUS https_make_request(TlsContext *tls_ctx, CHAR *req, DWORD req_len, CHAR **out, int *length, vschannel_allocator afn);

static INT connect_to_server(TlsContext *tls_ctx, LPWSTR host, INT port_number);

static LONG disconnect_from_server(TlsContext *tls_ctx);

static SECURITY_STATUS perform_client_handshake(TlsContext *tls_ctx, LPWSTR host, SecBuffer *pExtraData);

static SECURITY_STATUS client_handshake_loop(TlsContext *tls_ctx, BOOL fDoInitialRead, SecBuffer *pExtraData);

static DWORD verify_server_certificate(PCCERT_CONTEXT  pServerCert, LPWSTR host, DWORD dwCertFlags);

static SECURITY_STATUS create_credentials(TlsContext *tls_ctx);

static void get_new_client_credentials(TlsContext *tls_ctx);
