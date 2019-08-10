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

#define IO_BUFFER_SIZE  0x10000

#define TLS_MAX_BUFSIZ      32768

// Define here to be sure
#define SP_PROT_TLS1_2_CLIENT 0x00000800


static struct TlsContext new_tls_context();

static void vschannel_init();

static void vschannel_cleanup();

static INT request(CHAR *host, CHAR *req, CHAR *out);

static SECURITY_STATUS create_credentials(PCredHandle phCreds);

static INT connect_to_server(CHAR *host, INT port_number, SOCKET *pSocket);

static SECURITY_STATUS perform_client_handshake(
	SOCKET Socket, PCredHandle phCreds, CHAR *host,
	CtxtHandle *phContext, SecBuffer *pExtraData);

static SECURITY_STATUS client_handshake_loop(
	SOCKET Socket, PCredHandle phCreds, CtxtHandle *phContext,
	BOOL fDoInitialRead, SecBuffer *pExtraData);

static SECURITY_STATUS https_make_request(
	SOCKET Socket, PCredHandle phCreds,
	CtxtHandle *phContext, CHAR *req, CHAR *out, int *length);
	// CtxtHandle *phContext, CHAR *path);

static DWORD verify_server_certificate(
	PCCERT_CONTEXT  pServerCert, PSTR host, DWORD dwCertFlags);

static LONG disconnect_from_server(
	SOCKET Socket, PCredHandle phCreds, CtxtHandle *phContext);

static void get_new_client_credentials(CredHandle *phCreds, CtxtHandle *phContext);