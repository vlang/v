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

#if !defined(VSCHANNEL_REALLOC)
#define VSCHANNEL_REALLOC realloc
#endif

typedef struct TlsContext TlsContext;

TlsContext new_tls_context();

static void vschannel_init(TlsContext *tls_ctx);

static void vschannel_cleanup(TlsContext *tls_ctx);

static INT request(TlsContext *tls_ctx, INT iport, LPWSTR host, CHAR *req, CHAR **out);

static SECURITY_STATUS https_make_request(TlsContext *tls_ctx, CHAR *req, CHAR **out, int *length);

static INT connect_to_server(TlsContext *tls_ctx, LPWSTR host, INT port_number);

static LONG disconnect_from_server(TlsContext *tls_ctx);

static SECURITY_STATUS perform_client_handshake(TlsContext *tls_ctx, LPWSTR host, SecBuffer *pExtraData);

static SECURITY_STATUS client_handshake_loop(TlsContext *tls_ctx, BOOL fDoInitialRead, SecBuffer *pExtraData);

static DWORD verify_server_certificate(PCCERT_CONTEXT  pServerCert, LPWSTR host, DWORD dwCertFlags);

static SECURITY_STATUS create_credentials(TlsContext *tls_ctx);

static void get_new_client_credentials(TlsContext *tls_ctx);
