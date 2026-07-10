#include <vschannel.h>
#include <sspi.h>

// ALPN (RFC 7301) compatibility shim. Older toolchain headers (notably the
// ones bundled with tcc) predate the SChannel ALPN additions, so the structs,
// enums and constants below are missing there. Define them ourselves when the
// SDK headers did not. SECPKG_ATTR_APPLICATION_PROTOCOL guards the schannel.h
// types; SECBUFFER_APPLICATION_PROTOCOLS guards the sspi.h buffer constant.
#ifndef ANYSIZE_ARRAY
#define ANYSIZE_ARRAY 1
#endif

#ifndef SECPKG_ATTR_APPLICATION_PROTOCOL
#define SECPKG_ATTR_APPLICATION_PROTOCOL 35

typedef enum _SEC_APPLICATION_PROTOCOL_NEGOTIATION_EXT {
	SecApplicationProtocolNegotiationExt_None,
	SecApplicationProtocolNegotiationExt_NPN,
	SecApplicationProtocolNegotiationExt_ALPN
} SEC_APPLICATION_PROTOCOL_NEGOTIATION_EXT, *PSEC_APPLICATION_PROTOCOL_NEGOTIATION_EXT;

typedef struct _SEC_APPLICATION_PROTOCOL_LIST {
	SEC_APPLICATION_PROTOCOL_NEGOTIATION_EXT ProtoNegoExt;
	unsigned short ProtocolListSize;
	unsigned char ProtocolList[ANYSIZE_ARRAY];
} SEC_APPLICATION_PROTOCOL_LIST, *PSEC_APPLICATION_PROTOCOL_LIST;

typedef struct _SEC_APPLICATION_PROTOCOLS {
	unsigned long ProtocolListsSize;
	SEC_APPLICATION_PROTOCOL_LIST ProtocolLists[ANYSIZE_ARRAY];
} SEC_APPLICATION_PROTOCOLS, *PSEC_APPLICATION_PROTOCOLS;

typedef enum _SEC_APPLICATION_PROTOCOL_NEGOTIATION_STATUS {
	SecApplicationProtocolNegotiationStatus_None,
	SecApplicationProtocolNegotiationStatus_Success,
	SecApplicationProtocolNegotiationStatus_SelectedClientOnly
} SEC_APPLICATION_PROTOCOL_NEGOTIATION_STATUS, *PSEC_APPLICATION_PROTOCOL_NEGOTIATION_STATUS;

#define MAX_PROTOCOL_ID_SIZE 0xff

typedef struct _SecPkgContext_ApplicationProtocol {
	SEC_APPLICATION_PROTOCOL_NEGOTIATION_STATUS ProtoNegoStatus;
	SEC_APPLICATION_PROTOCOL_NEGOTIATION_EXT ProtoNegoExt;
	unsigned char ProtocolIdSize;
	unsigned char ProtocolId[MAX_PROTOCOL_ID_SIZE];
} SecPkgContext_ApplicationProtocol, *PSecPkgContext_ApplicationProtocol;
#endif // SECPKG_ATTR_APPLICATION_PROTOCOL

#ifndef SECBUFFER_APPLICATION_PROTOCOLS
#define SECBUFFER_APPLICATION_PROTOCOLS 18
#endif

// Proxy
WCHAR *  psz_proxy_server  = L"proxy";
INT     i_proxy_port      = 80;

// Options
INT     port_number     = 443;
BOOL    use_proxy       = FALSE;
DWORD   protocol        = 0;
ALG_ID  aid_key_exch    = 0;

// TODO: joe-c
// socket / tls ctx
struct TlsContext {
	// SSPI
	PSecurityFunctionTable sspi;
	// Cred store
	HCERTSTORE             cert_store;
	SCHANNEL_CRED          schannel_cred;
	// Socket
	SOCKET                 socket;	
	CredHandle             h_client_creds;
	CtxtHandle             h_context;
	PCCERT_CONTEXT         p_pemote_cert_context;
	INT                    last_error_code;
	BOOL                   validate_server_certificate;
	BOOL                   creds_initialized;
	BOOL                   context_initialized;
	// ALPN protocol list to advertise, in the standard ALPN wire format (each
	// name 1-byte length-prefixed), e.g. "\x02h2\x08http/1.1". alpn_wire_len == 0
	// means "do not advertise ALPN".
	unsigned char          alpn_wire[256];
	unsigned long          alpn_wire_len;
	// Negotiated application protocol name (e.g. "h2"); negotiated_alpn_len == 0
	// when the server selected none.
	char                   negotiated_alpn[256];
	unsigned long          negotiated_alpn_len;
	// Streaming transport state, used by the keep-the-connection-open path
	// (vschannel_h2_connect / vschannel_write / vschannel_read) that backs the
	// HTTP/2 driver. The one-shot request() path does not touch these.
	SecPkgContext_StreamSizes stream_sizes;     // cached TLS record sizes
	BOOL                   stream_sizes_valid;
	// Ciphertext staging buffer: bytes recv()'d from the socket that have not yet
	// been decrypted into a full record (SEC_E_INCOMPLETE_MESSAGE) plus any
	// trailing SECBUFFER_EXTRA from the last DecryptMessage.
	unsigned char         *recv_buf;
	unsigned long          recv_buf_cap;
	unsigned long          recv_buf_len;
	// Decrypted plaintext carryover: a single DecryptMessage can yield more
	// application bytes than the caller's read buffer can hold, so the remainder
	// is stashed here and drained on the next vschannel_read().
	unsigned char         *plain_buf;
	unsigned long          plain_buf_cap;
	unsigned long          plain_buf_len;       // valid decrypted bytes
	unsigned long          plain_buf_off;       // bytes already returned to caller
	// Reusable encryption buffer for vschannel_write(): one full record
	// (header + max message + trailer). Cached so the HTTP/2 driver's many small
	// writes do not LocalAlloc/LocalFree on every call.
	unsigned char         *send_buf;
	unsigned long          send_buf_cap;
	BOOL                   stream_eof;          // close_notify / context expired seen
};

TlsContext new_tls_context() {
	return (struct TlsContext) {
		.cert_store            = NULL,
		.last_error_code       = 0,
		.socket                = INVALID_SOCKET,
		.validate_server_certificate = TRUE,
		.creds_initialized     = FALSE,
		.context_initialized   = FALSE,
		.p_pemote_cert_context = NULL,
		.alpn_wire_len         = 0,
		.negotiated_alpn_len   = 0,
		.stream_sizes_valid    = FALSE,
		.recv_buf              = NULL,
		.recv_buf_cap          = 0,
		.recv_buf_len          = 0,
		.plain_buf             = NULL,
		.plain_buf_cap         = 0,
		.plain_buf_len         = 0,
		.plain_buf_off         = 0,
		.send_buf              = NULL,
		.send_buf_cap          = 0,
		.stream_eof            = FALSE
	};
};

// vschannel_alpn_supported reports whether this Windows version's SChannel
// supports client-side ALPN, which was introduced in Windows 8.1 / Server
// 2012 R2 (version 6.3). On older versions, passing a
// SECBUFFER_APPLICATION_PROTOCOLS input buffer into the handshake can fail it
// outright, so callers should skip ALPN (and HTTP/2) entirely there. Uses
// RtlGetVersion because GetVersionEx lies on manifest-less binaries from
// Windows 8.1 onwards.
INT vschannel_alpn_supported() {
	static INT cached = -1;
	if (cached < 0) {
		typedef LONG (WINAPI *RtlGetVersionFn)(OSVERSIONINFOW *);
		OSVERSIONINFOW vi;
		RtlGetVersionFn get_version = (RtlGetVersionFn)GetProcAddress(
				GetModuleHandleW(L"ntdll.dll"), "RtlGetVersion");
		INT supported = 0;
		if (get_version != NULL) {
			ZeroMemory(&vi, sizeof(vi));
			vi.dwOSVersionInfoSize = sizeof(vi);
			if (get_version(&vi) == 0
					&& (vi.dwMajorVersion > 6
						|| (vi.dwMajorVersion == 6 && vi.dwMinorVersion >= 3))) {
				supported = 1;
			}
		}
		cached = supported;
	}
	return cached;
}

// vschannel_set_alpn configures the ALPN protocol list to advertise during the
// next handshake. `wire` is the standard ALPN wire format (each protocol name
// preceded by a 1-byte length), e.g. "\x02h2\x08http/1.1". Passing len == 0
// disables ALPN advertisement.
void vschannel_set_alpn(TlsContext *tls_ctx, const char *wire, INT len) {
	if (len < 0) {
		len = 0;
	}
	if (len > (INT)sizeof(tls_ctx->alpn_wire)) {
		len = (INT)sizeof(tls_ctx->alpn_wire);
	}
	if (len > 0) {
		memcpy(tls_ctx->alpn_wire, wire, (size_t)len);
	}
	tls_ctx->alpn_wire_len = (unsigned long)len;
}

// vschannel_get_alpn copies the protocol the server selected via ALPN (e.g.
// "h2") into `out` and returns its length, or 0 if none was negotiated.
INT vschannel_get_alpn(TlsContext *tls_ctx, char *out, INT out_cap) {
	unsigned long n = tls_ctx->negotiated_alpn_len;
	if (out_cap < 0) {
		out_cap = 0;
	}
	if (n > (unsigned long)out_cap) {
		n = (unsigned long)out_cap;
	}
	if (n > 0) {
		memcpy(out, tls_ctx->negotiated_alpn, (size_t)n);
	}
	return (INT)n;
}

// vschannel_capture_alpn queries the negotiated ALPN protocol from a completed
// handshake and stores it on the context for vschannel_get_alpn().
static void vschannel_capture_alpn(TlsContext *tls_ctx) {
	SecPkgContext_ApplicationProtocol appproto;
	SECURITY_STATUS st;

	tls_ctx->negotiated_alpn_len = 0;
	st = tls_ctx->sspi->QueryContextAttributes(&tls_ctx->h_context,
			SECPKG_ATTR_APPLICATION_PROTOCOL, &appproto);
	if (st == SEC_E_OK
			&& appproto.ProtoNegoStatus == SecApplicationProtocolNegotiationStatus_Success
			&& appproto.ProtocolIdSize > 0
			&& appproto.ProtocolIdSize <= sizeof(tls_ctx->negotiated_alpn)) {
		memcpy(tls_ctx->negotiated_alpn, appproto.ProtocolId, appproto.ProtocolIdSize);
		tls_ctx->negotiated_alpn_len = appproto.ProtocolIdSize;
	}
}

static void vschannel_clear_last_error(TlsContext *tls_ctx) {
	tls_ctx->last_error_code = 0;
}

static void vschannel_set_last_error(TlsContext *tls_ctx, INT err_code) {
	tls_ctx->last_error_code = err_code;
}

static INT vschannel_last_error(TlsContext *tls_ctx) {
	return tls_ctx->last_error_code;
}

void vschannel_cleanup(TlsContext *tls_ctx) {
	// Free the server certificate context.
	if(tls_ctx->p_pemote_cert_context) {
		CertFreeCertificateContext(tls_ctx->p_pemote_cert_context);
		tls_ctx->p_pemote_cert_context = NULL;
	}

	// Free SSPI context handle.
	if(tls_ctx->context_initialized) {
		tls_ctx->sspi->DeleteSecurityContext(&tls_ctx->h_context);
		tls_ctx->context_initialized = FALSE;
	}

	// Free SSPI credentials handle.
	if(tls_ctx->creds_initialized) {
		tls_ctx->sspi->FreeCredentialsHandle(&tls_ctx->h_client_creds);
		tls_ctx->creds_initialized = FALSE;
	}
	
	// Close socket.
	if(tls_ctx->socket != INVALID_SOCKET) {
		closesocket(tls_ctx->socket);
		tls_ctx->socket = INVALID_SOCKET;
	}
	
	// Close "MY" certificate store.
	if(tls_ctx->cert_store) {
		CertCloseStore(tls_ctx->cert_store, 0);
		tls_ctx->cert_store = NULL;
	}

	// Free streaming-transport buffers.
	if(tls_ctx->recv_buf) {
		LocalFree(tls_ctx->recv_buf);
		tls_ctx->recv_buf = NULL;
	}
	tls_ctx->recv_buf_cap = 0;
	tls_ctx->recv_buf_len = 0;
	if(tls_ctx->plain_buf) {
		LocalFree(tls_ctx->plain_buf);
		tls_ctx->plain_buf = NULL;
	}
	tls_ctx->plain_buf_cap = 0;
	tls_ctx->plain_buf_len = 0;
	tls_ctx->plain_buf_off = 0;
	if(tls_ctx->send_buf) {
		LocalFree(tls_ctx->send_buf);
		tls_ctx->send_buf = NULL;
	}
	tls_ctx->send_buf_cap = 0;
	tls_ctx->stream_sizes_valid = FALSE;
	tls_ctx->stream_eof = FALSE;
}

void vschannel_init(TlsContext *tls_ctx, BOOL validate_server_certificate) {
	tls_ctx->sspi = InitSecurityInterface();
	tls_ctx->validate_server_certificate = validate_server_certificate;

	if(tls_ctx->sspi == NULL) {
		wprintf(L"Error 0x%x reading security interface.\n",
			   GetLastError());
		vschannel_cleanup(tls_ctx);
	}

	// Create credentials.
	if(create_credentials(tls_ctx)) {
		wprintf(L"Error creating credentials\n");
		vschannel_cleanup(tls_ctx);
	}
	tls_ctx->creds_initialized = TRUE;
}

// vschannel_open_and_handshake performs the connection setup shared by
// request(), vschannel_alpn_probe() and vschannel_h2_connect(): connect to
// host:iport, run the TLS handshake (advertising any configured ALPN), record
// the negotiated protocol, and — when verify_cert is set — validate the server
// certificate. On success the connection is left open (context_initialized) and,
// when pExtraData is non-NULL, it receives any application bytes the handshake
// bundled with its final flight (the caller then owns pExtraData->pvBuffer and
// must LocalFree it). On failure it records the error, frees any bundled extra,
// tears the connection down, and returns a non-zero SECURITY_STATUS. A connect
// failure already set last_error via connect_to_server.
static SECURITY_STATUS vschannel_open_and_handshake(TlsContext *tls_ctx, INT iport, LPWSTR host, BOOL verify_cert, SecBuffer *pExtraData) {
	SecBuffer       local_extra;
	SecBuffer      *extra = pExtraData ? pExtraData : &local_extra;
	SECURITY_STATUS Status;

	extra->pvBuffer = NULL;
	extra->cbBuffer = 0;

	protocol = SP_PROT_TLS1_2_CLIENT;
	port_number = iport;
	vschannel_clear_last_error(tls_ctx);

	if(connect_to_server(tls_ctx, host, port_number)) {
		vschannel_cleanup(tls_ctx);
		return SEC_E_INTERNAL_ERROR;
	}

	Status = perform_client_handshake(tls_ctx, host, extra);
	if(Status != SEC_E_OK) {
		vschannel_set_last_error(tls_ctx, Status);
		goto fail;
	}
	tls_ctx->context_initialized = TRUE;

	// Record the ALPN protocol the server selected (if any).
	vschannel_capture_alpn(tls_ctx);

	if(verify_cert) {
		// Get and validate the server's certificate.
		Status = tls_ctx->sspi->QueryContextAttributes(&tls_ctx->h_context,
				SECPKG_ATTR_REMOTE_CERT_CONTEXT, (PVOID)&tls_ctx->p_pemote_cert_context);
		if(Status != SEC_E_OK) {
			vschannel_set_last_error(tls_ctx, Status);
			goto fail;
		}
		Status = verify_server_certificate(tls_ctx->p_pemote_cert_context, host, 0);
		if(Status != SEC_E_OK) {
			// Could not authenticate the server (possible MITM): abort.
			vschannel_set_last_error(tls_ctx, Status);
			goto fail;
		}
		CertFreeCertificateContext(tls_ctx->p_pemote_cert_context);
		tls_ctx->p_pemote_cert_context = NULL;
	}

	// If the caller does not want the bundled application data, drop it.
	if(pExtraData == NULL && local_extra.pvBuffer != NULL) {
		LocalFree(local_extra.pvBuffer);
	}
	return SEC_E_OK;

fail:
	if(extra->pvBuffer != NULL) {
		LocalFree(extra->pvBuffer);
		extra->pvBuffer = NULL;
	}
	vschannel_cleanup(tls_ctx);
	return Status;
}

INT request(TlsContext *tls_ctx, INT iport, LPWSTR host, CHAR *req, DWORD req_len, CHAR **out, vschannel_allocator afn)
{
	SECURITY_STATUS Status;
	INT resp_length = 0;

	// Connect + handshake (+ cert validation when enabled). request() does not
	// consume handshake-bundled application data (HTTP/1.1 servers do not send
	// before the request), so pass NULL to have it dropped.
	if(vschannel_open_and_handshake(tls_ctx, iport, host,
			tls_ctx->validate_server_certificate, NULL) != SEC_E_OK) {
		return resp_length;
	}

	// Request from server
	Status = https_make_request(tls_ctx, req, req_len, out, &resp_length, afn);
	if(Status) {
		vschannel_set_last_error(tls_ctx, Status);
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}
	
	// Send a close_notify alert to the server and
	// close down the connection.
	Status = disconnect_from_server(tls_ctx);
	if(Status) {
		vschannel_set_last_error(tls_ctx, Status);
		wprintf(L"Error disconnecting from server\n");
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}
	tls_ctx->context_initialized = FALSE;
	tls_ctx->socket = INVALID_SOCKET;

	return resp_length;
}

// vschannel_request_on_open runs a one-shot HTTP/1.1 request over a connection
// that vschannel_h2_connect() already opened and handshaked, then closes it.
// It is the HTTP/1.1 fallback used when a server, asked for ALPN `h2`, does not
// select it: rather than reconnect, we reuse the open connection. Returns the
// response length (see request()).
INT vschannel_request_on_open(TlsContext *tls_ctx, CHAR *req, DWORD req_len, CHAR **out, vschannel_allocator afn) {
	SECURITY_STATUS Status;
	INT resp_length = 0;

	Status = https_make_request(tls_ctx, req, req_len, out, &resp_length, afn);
	if(Status) {
		vschannel_set_last_error(tls_ctx, Status);
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}

	Status = disconnect_from_server(tls_ctx);
	if(Status) {
		vschannel_set_last_error(tls_ctx, Status);
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}
	tls_ctx->context_initialized = FALSE;
	tls_ctx->socket = INVALID_SOCKET;

	return resp_length;
}

// vschannel_alpn_probe connects to host:iport, performs the TLS handshake while
// advertising whatever ALPN list was configured via vschannel_set_alpn(),
// captures the protocol the server selected into `out` (up to out_cap bytes),
// and disconnects without sending an application request. Returns the
// negotiated protocol length (0 = handshake succeeded but no protocol selected),
// or -1 on connect/handshake failure (see vschannel_last_error). Intended for
// tests and capability checks, since request() only speaks HTTP/1.1.
INT vschannel_alpn_probe(TlsContext *tls_ctx, INT iport, LPWSTR host, char *out, INT out_cap) {
	// Probe only: handshake (no cert validation, no application data) then close.
	if(vschannel_open_and_handshake(tls_ctx, iport, host, FALSE, NULL) != SEC_E_OK) {
		return -1;
	}

	disconnect_from_server(tls_ctx);
	tls_ctx->context_initialized = FALSE;
	tls_ctx->socket = INVALID_SOCKET;

	return vschannel_get_alpn(tls_ctx, out, out_cap);
}

// ---------------------------------------------------------------------------
// Streaming transport (keep the TLS connection open and exchange raw bytes).
//
// request() above is a one-shot: connect, handshake, send the whole HTTP/1.1
// request, read the whole response, disconnect. An HTTP/2 driver instead needs
// a long-lived, byte-oriented transport. The functions below expose exactly
// that: vschannel_h2_connect() opens the connection (handshake + cert check)
// and leaves it open, vschannel_write()/vschannel_read() move application bytes
// across it, and vschannel_h2_close() shuts it down. They reuse the same SSPI
// primitives as request()/https_make_request(), but keep the encrypt/decrypt
// state on the TlsContext so reads can span calls.
// ---------------------------------------------------------------------------

// vschannel_ensure_stream_state caches the negotiated TLS record sizes and
// allocates the ciphertext/plaintext working buffers, once per connection.
static SECURITY_STATUS vschannel_ensure_stream_state(TlsContext *tls_ctx) {
	if(tls_ctx->stream_sizes_valid) {
		return SEC_E_OK;
	}
	SECURITY_STATUS scRet = tls_ctx->sspi->QueryContextAttributes(&tls_ctx->h_context,
			SECPKG_ATTR_STREAM_SIZES, &tls_ctx->stream_sizes);
	if(scRet != SEC_E_OK) {
		return scRet;
	}
	// One full wire record: header + max plaintext + trailer. recv() never needs
	// more than this buffered to complete a single record; trailing bytes of the
	// next record are carried as SECBUFFER_EXTRA.
	tls_ctx->recv_buf_cap = tls_ctx->stream_sizes.cbHeader
			+ tls_ctx->stream_sizes.cbMaximumMessage + tls_ctx->stream_sizes.cbTrailer;
	tls_ctx->recv_buf = (unsigned char *)LocalAlloc(LPTR, tls_ctx->recv_buf_cap);
	// One record decrypts to at most cbMaximumMessage plaintext bytes.
	tls_ctx->plain_buf_cap = tls_ctx->stream_sizes.cbMaximumMessage;
	tls_ctx->plain_buf = (unsigned char *)LocalAlloc(LPTR, tls_ctx->plain_buf_cap);
	// Reusable send buffer: one full outgoing record.
	tls_ctx->send_buf_cap = tls_ctx->recv_buf_cap;
	tls_ctx->send_buf = (unsigned char *)LocalAlloc(LPTR, tls_ctx->send_buf_cap);
	if(tls_ctx->recv_buf == NULL || tls_ctx->plain_buf == NULL || tls_ctx->send_buf == NULL) {
		return SEC_E_INTERNAL_ERROR;
	}
	tls_ctx->recv_buf_len = 0;
	tls_ctx->plain_buf_len = 0;
	tls_ctx->plain_buf_off = 0;
	tls_ctx->stream_sizes_valid = TRUE;
	return SEC_E_OK;
}

// vschannel_h2_connect connects to host:iport, performs the TLS handshake while
// advertising whatever ALPN list was set via vschannel_set_alpn(), validates the
// server certificate (when enabled), and leaves the connection open for
// vschannel_write()/vschannel_read(). Returns 0 on success, non-zero on failure
// (see vschannel_last_error). The negotiated protocol is available afterwards
// via vschannel_get_alpn().
INT vschannel_h2_connect(TlsContext *tls_ctx, INT iport, LPWSTR host) {
	SecBuffer       ExtraData;
	SECURITY_STATUS Status;

	// Connect + handshake + cert validation (when enabled). Unlike the one-shot
	// paths, keep the handshake-bundled application data: for HTTP/2 it is the
	// server's first record (SETTINGS), needed below.
	if(vschannel_open_and_handshake(tls_ctx, iport, host,
			tls_ctx->validate_server_certificate, &ExtraData) != SEC_E_OK) {
		return -1;
	}

	Status = vschannel_ensure_stream_state(tls_ctx);
	if(Status != SEC_E_OK) {
		vschannel_set_last_error(tls_ctx, Status);
		if(ExtraData.pvBuffer != NULL) {
			LocalFree(ExtraData.pvBuffer);
		}
		vschannel_cleanup(tls_ctx);
		return -1;
	}

	// The final handshake flight often arrives in the same TCP segment as the
	// server's first application record (for HTTP/2, the SETTINGS frame), which
	// the handshake hands back as SECBUFFER_EXTRA. Those bytes are already off
	// the socket, so they must be carried into the read buffer; otherwise the
	// first vschannel_read() would skip them and H2Conn would desync. Grow the
	// staging buffer if the bundled data exceeds one record.
	if(ExtraData.pvBuffer != NULL) {
		if(ExtraData.cbBuffer > 0) {
			if(ExtraData.cbBuffer > tls_ctx->recv_buf_cap) {
				unsigned char *grown = (unsigned char *)LocalAlloc(LPTR, ExtraData.cbBuffer);
				if(grown != NULL) {
					LocalFree(tls_ctx->recv_buf);
					tls_ctx->recv_buf = grown;
					tls_ctx->recv_buf_cap = ExtraData.cbBuffer;
				}
			}
			if(ExtraData.cbBuffer <= tls_ctx->recv_buf_cap) {
				MoveMemory(tls_ctx->recv_buf, ExtraData.pvBuffer, ExtraData.cbBuffer);
				tls_ctx->recv_buf_len = ExtraData.cbBuffer;
			}
		}
		LocalFree(ExtraData.pvBuffer);
	}

	return 0;
}

// vschannel_write encrypts and sends `len` application bytes over the open
// connection, chunked to the negotiated maximum record size. Returns the number
// of bytes consumed (== len) on success, or -1 on error.
INT vschannel_write(TlsContext *tls_ctx, const char *buf, INT len) {
	SecBufferDesc Message;
	SecBuffer     Buffers[4];
	SECURITY_STATUS scRet;
	PBYTE  io;
	DWORD  off;
	INT    cbData;

	if(len <= 0) {
		return 0;
	}
	if(vschannel_ensure_stream_state(tls_ctx) != SEC_E_OK) {
		return -1;
	}

	// Reuse the per-context send buffer (one full record) rather than allocating
	// on every write; the HTTP/2 driver issues many small writes per request.
	io = (PBYTE)tls_ctx->send_buf;

	off = 0;
	while(off < (DWORD)len) {
		DWORD chunk = (DWORD)len - off;
		if(chunk > tls_ctx->stream_sizes.cbMaximumMessage) {
			chunk = tls_ctx->stream_sizes.cbMaximumMessage;
		}
		memcpy(io + tls_ctx->stream_sizes.cbHeader, buf + off, chunk);

		Buffers[0].pvBuffer   = io;
		Buffers[0].cbBuffer   = tls_ctx->stream_sizes.cbHeader;
		Buffers[0].BufferType = SECBUFFER_STREAM_HEADER;
		Buffers[1].pvBuffer   = io + tls_ctx->stream_sizes.cbHeader;
		Buffers[1].cbBuffer   = chunk;
		Buffers[1].BufferType = SECBUFFER_DATA;
		Buffers[2].pvBuffer   = io + tls_ctx->stream_sizes.cbHeader + chunk;
		Buffers[2].cbBuffer   = tls_ctx->stream_sizes.cbTrailer;
		Buffers[2].BufferType = SECBUFFER_STREAM_TRAILER;
		Buffers[3].BufferType = SECBUFFER_EMPTY;

		Message.ulVersion = SECBUFFER_VERSION;
		Message.cBuffers  = 4;
		Message.pBuffers  = Buffers;

		scRet = tls_ctx->sspi->EncryptMessage(&tls_ctx->h_context, 0, &Message, 0);
		if(FAILED(scRet)) {
			vschannel_set_last_error(tls_ctx, scRet);
			return -1;
		}

		DWORD to_send = Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer;
		DWORD sent = 0;
		while(sent < to_send) {
			cbData = send(tls_ctx->socket, (char*)io + sent, (int)(to_send - sent), 0);
			if(cbData == SOCKET_ERROR || cbData == 0) {
				vschannel_set_last_error(tls_ctx, WSAGetLastError());
				return -1;
			}
			sent += (DWORD)cbData;
		}
		off += chunk;
	}

	return len;
}

// vschannel_read returns up to `cap` decrypted application bytes from the open
// connection. It returns the number of bytes written into `buf` (> 0), 0 at
// end of stream (close_notify / context expired / peer closed the socket), or
// -1 on error. Leftover decrypted plaintext that did not fit in `buf`, and
// ciphertext that did not yet form a complete record, are carried on the
// TlsContext across calls.
INT vschannel_read(TlsContext *tls_ctx, char *buf, INT cap) {
	SecBufferDesc Message;
	SecBuffer     Buffers[4];
	SecBuffer     ExtraBuffer;
	SecBuffer    *pDataBuffer;
	SecBuffer    *pExtraBuffer;
	SECURITY_STATUS scRet;
	INT cbData;
	int i;

	if(cap <= 0) {
		return 0;
	}
	if(vschannel_ensure_stream_state(tls_ctx) != SEC_E_OK) {
		return -1;
	}

	// 1. Serve leftover decrypted plaintext from a previous record first.
	if(tls_ctx->plain_buf_off < tls_ctx->plain_buf_len) {
		DWORD avail = tls_ctx->plain_buf_len - tls_ctx->plain_buf_off;
		DWORD n = avail < (DWORD)cap ? avail : (DWORD)cap;
		memcpy(buf, tls_ctx->plain_buf + tls_ctx->plain_buf_off, n);
		tls_ctx->plain_buf_off += n;
		return (INT)n;
	}

	if(tls_ctx->stream_eof) {
		return 0;
	}

	for(;;) {
		// Try to decrypt whatever ciphertext we have buffered.
		if(tls_ctx->recv_buf_len > 0) {
			Buffers[0].pvBuffer   = tls_ctx->recv_buf;
			Buffers[0].cbBuffer   = tls_ctx->recv_buf_len;
			Buffers[0].BufferType = SECBUFFER_DATA;
			Buffers[1].BufferType = SECBUFFER_EMPTY;
			Buffers[2].BufferType = SECBUFFER_EMPTY;
			Buffers[3].BufferType = SECBUFFER_EMPTY;

			Message.ulVersion = SECBUFFER_VERSION;
			Message.cBuffers  = 4;
			Message.pBuffers  = Buffers;

			scRet = tls_ctx->sspi->DecryptMessage(&tls_ctx->h_context, &Message, 0, NULL);

			if(scRet == SEC_E_OK || scRet == SEC_I_RENEGOTIATE || scRet == SEC_I_CONTEXT_EXPIRED) {
				pDataBuffer  = NULL;
				pExtraBuffer = NULL;
				for(i = 1; i < 4; i++) {
					if(pDataBuffer == NULL && Buffers[i].BufferType == SECBUFFER_DATA) {
						pDataBuffer = &Buffers[i];
					}
					if(pExtraBuffer == NULL && Buffers[i].BufferType == SECBUFFER_EXTRA) {
						pExtraBuffer = &Buffers[i];
					}
				}

				// Copy out decrypted application data: as much as fits in the
				// caller's buffer, the rest into the plaintext carryover. Both
				// reads happen before the SECBUFFER_EXTRA move below, since the
				// data buffer points inside recv_buf.
				INT produced = 0;
				if(pDataBuffer && pDataBuffer->cbBuffer > 0) {
					DWORD data_len = pDataBuffer->cbBuffer;
					DWORD n = data_len < (DWORD)cap ? data_len : (DWORD)cap;
					memcpy(buf, pDataBuffer->pvBuffer, n);
					produced = (INT)n;
					DWORD rest = data_len - n;
					if(rest > 0) {
						memcpy(tls_ctx->plain_buf, (unsigned char*)pDataBuffer->pvBuffer + n, rest);
						tls_ctx->plain_buf_len = rest;
						tls_ctx->plain_buf_off = 0;
					}
				}

				// Carry trailing ciphertext (start of the next record) to the
				// front of recv_buf for the next decrypt.
				if(pExtraBuffer) {
					MoveMemory(tls_ctx->recv_buf, pExtraBuffer->pvBuffer, pExtraBuffer->cbBuffer);
					tls_ctx->recv_buf_len = pExtraBuffer->cbBuffer;
				} else {
					tls_ctx->recv_buf_len = 0;
				}

				if(scRet == SEC_I_RENEGOTIATE) {
					// The server requested a new handshake. Run it, then carry any
					// extra data it left behind and keep reading.
					SECURITY_STATUS rh = client_handshake_loop(tls_ctx, FALSE, &ExtraBuffer);
					if(rh != SEC_E_OK) {
						vschannel_set_last_error(tls_ctx, rh);
						return -1;
					}
					if(ExtraBuffer.pvBuffer) {
						if(ExtraBuffer.cbBuffer <= tls_ctx->recv_buf_cap) {
							MoveMemory(tls_ctx->recv_buf, ExtraBuffer.pvBuffer, ExtraBuffer.cbBuffer);
							tls_ctx->recv_buf_len = ExtraBuffer.cbBuffer;
						}
						LocalFree(ExtraBuffer.pvBuffer);
					}
				} else if(scRet == SEC_I_CONTEXT_EXPIRED) {
					// Graceful close_notify from the server.
					tls_ctx->stream_eof = TRUE;
					if(produced > 0) {
						return produced;
					}
					return 0;
				}

				if(produced > 0) {
					return produced;
				}
				// A record with no application data (e.g. a session ticket or a
				// renegotiation). Keep going.
				continue;
			} else if(scRet == SEC_E_INCOMPLETE_MESSAGE) {
				// Need more bytes to complete the current record: fall through to
				// recv() more ciphertext.
			} else {
				vschannel_set_last_error(tls_ctx, scRet);
				return -1;
			}
		}

		// Receive more ciphertext.
		if(tls_ctx->recv_buf_len >= tls_ctx->recv_buf_cap) {
			// Should not happen: a single record fits in recv_buf_cap.
			vschannel_set_last_error(tls_ctx, SEC_E_INTERNAL_ERROR);
			return -1;
		}
		cbData = recv(tls_ctx->socket, (char*)tls_ctx->recv_buf + tls_ctx->recv_buf_len,
				(int)(tls_ctx->recv_buf_cap - tls_ctx->recv_buf_len), 0);
		if(cbData == SOCKET_ERROR) {
			vschannel_set_last_error(tls_ctx, WSAGetLastError());
			return -1;
		}
		if(cbData == 0) {
			// Peer closed the socket. Any buffered bytes are an incomplete record.
			tls_ctx->stream_eof = TRUE;
			return 0;
		}
		tls_ctx->recv_buf_len += (DWORD)cbData;
	}
}

// vschannel_h2_close sends a close_notify alert and tears down the connection.
void vschannel_h2_close(TlsContext *tls_ctx) {
	if(tls_ctx->context_initialized) {
		disconnect_from_server(tls_ctx);
		tls_ctx->context_initialized = FALSE;
		tls_ctx->socket = INVALID_SOCKET;
	}
	vschannel_cleanup(tls_ctx);
}


static SECURITY_STATUS create_credentials(TlsContext *tls_ctx) {
	TimeStamp       tsExpiry;
	SECURITY_STATUS Status;

	DWORD           cSupportedAlgs = 0;
	ALG_ID          rgbSupportedAlgs[16];

	PCCERT_CONTEXT  pCertContext = NULL;

	// Open the "MY" certificate store, which is where Internet Explorer
	// stores its client certificates.
	if(tls_ctx->cert_store == NULL) {
		tls_ctx->cert_store = CertOpenSystemStore(0, L"MY");

		if(!tls_ctx->cert_store) {
			wprintf(L"Error 0x%x returned by CertOpenSystemStore\n", 
			GetLastError());
			return SEC_E_NO_CREDENTIALS;
		}
	}

	// Build Schannel credential structure. Currently, this sample only
	// specifies the protocol to be used (and optionally the certificate, 
	// of course). Real applications may wish to specify other parameters 
	// as well.

	ZeroMemory(&tls_ctx->schannel_cred, sizeof(tls_ctx->schannel_cred));

	tls_ctx->schannel_cred.dwVersion  = SCHANNEL_CRED_VERSION;
	if(pCertContext)
	{
		tls_ctx->schannel_cred.cCreds     = 1;
		tls_ctx->schannel_cred.paCred     = &pCertContext;
	}

	tls_ctx->schannel_cred.grbitEnabledProtocols = protocol;

	if(aid_key_exch)
	{
		rgbSupportedAlgs[cSupportedAlgs++] = aid_key_exch;
	}

	if(cSupportedAlgs)
	{
		tls_ctx->schannel_cred.cSupportedAlgs    = cSupportedAlgs;
		tls_ctx->schannel_cred.palgSupportedAlgs = rgbSupportedAlgs;
	}

	tls_ctx->schannel_cred.dwFlags |= SCH_CRED_NO_DEFAULT_CREDS;
	tls_ctx->schannel_cred.dwFlags |= SCH_CRED_MANUAL_CRED_VALIDATION;

	// Keep certificate validation under the caller's control. The validated
	// path runs explicit hostname/chain validation after the handshake.

	// Create an SSPI credential.

	Status = tls_ctx->sspi->AcquireCredentialsHandle(
						NULL,                   // Name of principal    
						UNISP_NAME_W,           // Name of package
						SECPKG_CRED_OUTBOUND,   // Flags indicating use
						NULL,                   // Pointer to logon ID
						&tls_ctx->schannel_cred,          // Package specific data
						NULL,                   // Pointer to GetKey() func
						NULL,                   // Value to pass to GetKey()
						&tls_ctx->h_client_creds,                // (out) Cred Handle
						&tsExpiry);             // (out) Lifetime (optional)
	if(Status != SEC_E_OK) {
		wprintf(L"Error 0x%x returned by AcquireCredentialsHandle\n", Status);
		goto cleanup;
	}

cleanup:

	// Free the certificate context. Schannel has already made its own copy.

	if(pCertContext) {
		CertFreeCertificateContext(pCertContext);
	}


	return Status;
}


static INT connect_to_server(TlsContext *tls_ctx, LPWSTR host, INT port_number) {
	SOCKET Socket;
	
	SOCKADDR_STORAGE local_address = { 0 };
	SOCKADDR_STORAGE remote_address = { 0 };

	DWORD local_address_length =  sizeof(local_address);
	DWORD remote_address_length = sizeof(remote_address);

	struct timeval tv;
	tv.tv_sec = 60;
	tv.tv_usec = 0;

	Socket = socket(PF_INET, SOCK_STREAM, 0);
	if(Socket == INVALID_SOCKET) {
		INT err_code = WSAGetLastError();
		vschannel_set_last_error(tls_ctx, err_code);
		return err_code;
	}

	LPWSTR connect_name = use_proxy ? psz_proxy_server : host;

	WCHAR service_name[10];
	int res = wsprintf(service_name, L"%d", port_number);

	if(WSAConnectByNameW(Socket,connect_name, service_name, &local_address_length, 
		&local_address, &remote_address_length, &remote_address, &tv, NULL) == FALSE) {
		INT err_code = WSAGetLastError();
		vschannel_set_last_error(tls_ctx, err_code);
		closesocket(Socket);
		return err_code;
	}

	if(use_proxy) {
		BYTE  pbMessage[200]; 
		DWORD cbMessage;

		// Build message for proxy server
		strcpy(pbMessage, "CONNECT ");
		strcat(pbMessage, host);
		strcat(pbMessage, ":");
		_itoa(port_number, pbMessage + strlen(pbMessage), 10);
		strcat(pbMessage, " HTTP/1.0\r\nUser-Agent: webclient\r\n\r\n");
		cbMessage = (DWORD)strlen(pbMessage);

		// Send message to proxy server
		if(send(Socket, pbMessage, cbMessage, 0) == SOCKET_ERROR) {
			INT err_code = WSAGetLastError();
			vschannel_set_last_error(tls_ctx, err_code);
			return err_code;
		}

		// Receive message from proxy server
		cbMessage = recv(Socket, pbMessage, 200, 0);
		if(cbMessage == SOCKET_ERROR) {
			INT err_code = WSAGetLastError();
			vschannel_set_last_error(tls_ctx, err_code);
			return err_code;
		}

		// this sample is limited but in normal use it 
		// should continue to receive until CR LF CR LF is received
	}

	tls_ctx->socket = Socket;

	return SEC_E_OK;
}


static LONG disconnect_from_server(TlsContext *tls_ctx) {
	DWORD           dwType;
	PBYTE           pbMessage;
	DWORD           cbMessage;
	DWORD           cbData;

	SecBufferDesc   OutBuffer;
	SecBuffer       OutBuffers[1];
	DWORD           dwSSPIFlags;
	DWORD           dwSSPIOutFlags;
	TimeStamp       tsExpiry;
	DWORD           Status;

	// Notify schannel that we are about to close the connection.

	dwType = SCHANNEL_SHUTDOWN;

	OutBuffers[0].pvBuffer   = &dwType;
	OutBuffers[0].BufferType = SECBUFFER_TOKEN;
	OutBuffers[0].cbBuffer   = sizeof(dwType);

	OutBuffer.cBuffers  = 1;
	OutBuffer.pBuffers  = OutBuffers;
	OutBuffer.ulVersion = SECBUFFER_VERSION;

	Status = tls_ctx->sspi->ApplyControlToken(&tls_ctx->h_context, &OutBuffer);

	if(FAILED(Status)) {
		wprintf(L"Error 0x%x returned by ApplyControlToken\n", Status);
		goto cleanup;
	}

	// Build an SSL close notify message.

	dwSSPIFlags = ISC_REQ_SEQUENCE_DETECT   |
				  ISC_REQ_REPLAY_DETECT     |
				  ISC_REQ_CONFIDENTIALITY   |
				  ISC_RET_EXTENDED_ERROR    |
				  ISC_REQ_ALLOCATE_MEMORY   |
				  ISC_REQ_STREAM;

	OutBuffers[0].pvBuffer   = NULL;
	OutBuffers[0].BufferType = SECBUFFER_TOKEN;
	OutBuffers[0].cbBuffer   = 0;

	OutBuffer.cBuffers  = 1;
	OutBuffer.pBuffers  = OutBuffers;
	OutBuffer.ulVersion = SECBUFFER_VERSION;

	Status = tls_ctx->sspi->InitializeSecurityContext(
		&tls_ctx->h_client_creds, &tls_ctx->h_context, NULL, dwSSPIFlags, 0, SECURITY_NATIVE_DREP,
		NULL, 0, &tls_ctx->h_context, &OutBuffer, &dwSSPIOutFlags, &tsExpiry);

	if(FAILED(Status))  {
		wprintf(L"Error 0x%x returned by InitializeSecurityContext\n", Status);
		goto cleanup;
	}

	pbMessage = OutBuffers[0].pvBuffer;
	cbMessage = OutBuffers[0].cbBuffer;

	// Send the close notify message to the server.

	if(pbMessage != NULL && cbMessage != 0) {
		cbData = send(tls_ctx->socket, pbMessage, cbMessage, 0);
		if(cbData == SOCKET_ERROR || cbData == 0) {
			Status = WSAGetLastError();
			wprintf(L"Error %d sending close notify\n", Status);
			goto cleanup;
		}

		// Free output buffer.
		tls_ctx->sspi->FreeContextBuffer(pbMessage);
	}
	

cleanup:

	// Free the security context.
	tls_ctx->sspi->DeleteSecurityContext(&tls_ctx->h_context);

	// Close the socket.
	closesocket(tls_ctx->socket);

	return Status;
}


static SECURITY_STATUS perform_client_handshake(TlsContext *tls_ctx, WCHAR *host, SecBuffer *pExtraData) {
	SecBufferDesc   OutBuffer;
	SecBuffer       OutBuffers[1];
	DWORD           dwSSPIFlags;
	DWORD           dwSSPIOutFlags;
	TimeStamp       tsExpiry;
	SECURITY_STATUS scRet;
	DWORD           cbData;

	dwSSPIFlags = ISC_REQ_SEQUENCE_DETECT   |
				  ISC_REQ_REPLAY_DETECT     |
				  ISC_REQ_CONFIDENTIALITY   |
				  ISC_RET_EXTENDED_ERROR    |
				  ISC_REQ_ALLOCATE_MEMORY   |
				  ISC_REQ_STREAM;

	//
	//  Optionally advertise ALPN protocols in the ClientHello. SChannel takes
	//  this as a SECBUFFER_APPLICATION_PROTOCOLS input buffer holding a
	//  SEC_APPLICATION_PROTOCOLS record. The backing store is a 4-byte-aligned
	//  unsigned long array so the struct cast is well aligned on every compiler.
	//
	SecBuffer      InBuffers[1];
	SecBufferDesc  InBuffer;
	SecBufferDesc *pInput = NULL;
	unsigned long  alpn_store[80]; // 320 bytes; alpn_wire is at most 256
	if (tls_ctx->alpn_wire_len > 0) {
		SEC_APPLICATION_PROTOCOLS     *protos = (SEC_APPLICATION_PROTOCOLS *)alpn_store;
		SEC_APPLICATION_PROTOCOL_LIST *list   = &protos->ProtocolLists[0];
		unsigned long wlen = tls_ctx->alpn_wire_len;

		list->ProtoNegoExt     = SecApplicationProtocolNegotiationExt_ALPN;
		list->ProtocolListSize = (unsigned short)wlen;
		memcpy(list->ProtocolList, tls_ctx->alpn_wire, (size_t)wlen);
		protos->ProtocolListsSize =
			(unsigned long)(FIELD_OFFSET(SEC_APPLICATION_PROTOCOL_LIST, ProtocolList) + wlen);

		InBuffers[0].pvBuffer   = protos;
		InBuffers[0].cbBuffer   =
			(unsigned long)(FIELD_OFFSET(SEC_APPLICATION_PROTOCOLS, ProtocolLists) + protos->ProtocolListsSize);
		InBuffers[0].BufferType = SECBUFFER_APPLICATION_PROTOCOLS;

		InBuffer.cBuffers  = 1;
		InBuffer.pBuffers  = InBuffers;
		InBuffer.ulVersion = SECBUFFER_VERSION;
		pInput = &InBuffer;
	}

	//
	//  Initiate a ClientHello message and generate a token.
	//

	OutBuffers[0].pvBuffer   = NULL;
	OutBuffers[0].BufferType = SECBUFFER_TOKEN;
	OutBuffers[0].cbBuffer   = 0;

	OutBuffer.cBuffers = 1;
	OutBuffer.pBuffers = OutBuffers;
	OutBuffer.ulVersion = SECBUFFER_VERSION;

	scRet = tls_ctx->sspi->InitializeSecurityContext(
					&tls_ctx->h_client_creds,
					NULL,
					host,
					dwSSPIFlags,
					0,
					SECURITY_NATIVE_DREP,
					pInput,
					0,
					&tls_ctx->h_context,
					&OutBuffer,
					&dwSSPIOutFlags,
					&tsExpiry);

	if(scRet != SEC_I_CONTINUE_NEEDED)
	{
		wprintf(L"Error %d returned by InitializeSecurityContext (1)\n", scRet);
		return scRet;
	}

	// Send response to server if there is one.
	if(OutBuffers[0].cbBuffer != 0 && OutBuffers[0].pvBuffer != NULL)
	{
		cbData = send(tls_ctx->socket, OutBuffers[0].pvBuffer, OutBuffers[0].cbBuffer, 0);
		if(cbData == SOCKET_ERROR || cbData == 0) {
			wprintf(L"Error %d sending data to server (1)\n", WSAGetLastError());
			tls_ctx->sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
			tls_ctx->sspi->DeleteSecurityContext(&tls_ctx->h_context);
			return SEC_E_INTERNAL_ERROR;
		}

		// Free output buffer.
		tls_ctx->sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
		OutBuffers[0].pvBuffer = NULL;
	}

	return client_handshake_loop(tls_ctx, TRUE, pExtraData);
}


static SECURITY_STATUS client_handshake_loop(TlsContext *tls_ctx, BOOL fDoInitialRead, SecBuffer *pExtraData) {
	SecBufferDesc   InBuffer;
	SecBuffer       InBuffers[2];
	SecBufferDesc   OutBuffer;
	SecBuffer       OutBuffers[1];
	DWORD           dwSSPIFlags;
	DWORD           dwSSPIOutFlags;
	TimeStamp       tsExpiry;
	SECURITY_STATUS scRet;
	DWORD           cbData;

	PUCHAR          IoBuffer;
	DWORD           cbIoBuffer;
	BOOL            fDoRead;


	dwSSPIFlags = ISC_REQ_SEQUENCE_DETECT   |
				  ISC_REQ_REPLAY_DETECT     |
				  ISC_REQ_CONFIDENTIALITY   |
				  ISC_RET_EXTENDED_ERROR    |
				  ISC_REQ_ALLOCATE_MEMORY   |
				  ISC_REQ_STREAM;

	//
	// Allocate data buffer.
	//

	IoBuffer = LocalAlloc(LPTR, IO_BUFFER_SIZE);
	if(IoBuffer == NULL)
	{
		wprintf(L"Out of memory (1)\n");
		return SEC_E_INTERNAL_ERROR;
	}
	cbIoBuffer = 0;

	fDoRead = fDoInitialRead;


	// 
	// Loop until the handshake is finished or an error occurs.
	//

	scRet = SEC_I_CONTINUE_NEEDED;

	while(scRet == SEC_I_CONTINUE_NEEDED ||
		  scRet == SEC_E_INCOMPLETE_MESSAGE ||
		  scRet == SEC_I_INCOMPLETE_CREDENTIALS) {
		
		// Read data from server.
		if(0 == cbIoBuffer || scRet == SEC_E_INCOMPLETE_MESSAGE) {
			if(fDoRead) {
				cbData = recv(tls_ctx->socket, 
							  IoBuffer + cbIoBuffer, 
							  IO_BUFFER_SIZE - cbIoBuffer, 
							  0);
				if(cbData == SOCKET_ERROR) {
					wprintf(L"Error %d reading data from server\n", WSAGetLastError());
					scRet = SEC_E_INTERNAL_ERROR;
					break;
				}
				else if(cbData == 0) {
					wprintf(L"Server unexpectedly disconnected\n");
					scRet = SEC_E_INTERNAL_ERROR;
					break;
				}

				cbIoBuffer += cbData;
			}
			else {
				fDoRead = TRUE;
			}
		}

		// Set up the input buffers. Buffer 0 is used to pass in data
		// received from the server. Schannel will consume some or all
		// of this. Leftover data (if any) will be placed in buffer 1 and
		// given a buffer type of SECBUFFER_EXTRA.

		InBuffers[0].pvBuffer   = IoBuffer;
		InBuffers[0].cbBuffer   = cbIoBuffer;
		InBuffers[0].BufferType = SECBUFFER_TOKEN;

		InBuffers[1].pvBuffer   = NULL;
		InBuffers[1].cbBuffer   = 0;
		InBuffers[1].BufferType = SECBUFFER_EMPTY;

		InBuffer.cBuffers       = 2;
		InBuffer.pBuffers       = InBuffers;
		InBuffer.ulVersion      = SECBUFFER_VERSION;

		// Set up the output buffers. These are initialized to NULL
		// so as to make it less likely we'll attempt to free random
		// garbage later.

		OutBuffers[0].pvBuffer  = NULL;
		OutBuffers[0].BufferType= SECBUFFER_TOKEN;
		OutBuffers[0].cbBuffer  = 0;

		OutBuffer.cBuffers      = 1;
		OutBuffer.pBuffers      = OutBuffers;
		OutBuffer.ulVersion     = SECBUFFER_VERSION;

		// Call InitializeSecurityContext.

		scRet = tls_ctx->sspi->InitializeSecurityContext(
			&tls_ctx->h_client_creds, &tls_ctx->h_context,	NULL, dwSSPIFlags, 0, SECURITY_NATIVE_DREP,
			&InBuffer, 0, NULL, &OutBuffer, &dwSSPIOutFlags, &tsExpiry);

		// If InitializeSecurityContext was successful (or if the error was 
		// one of the special extended ones), send the contends of the output
		// buffer to the server.

		if(scRet == SEC_E_OK ||
		   scRet == SEC_I_CONTINUE_NEEDED ||
		   FAILED(scRet) && (dwSSPIOutFlags & ISC_RET_EXTENDED_ERROR)) {
			if(OutBuffers[0].cbBuffer != 0 && OutBuffers[0].pvBuffer != NULL) {
				cbData = send(tls_ctx->socket,
							  OutBuffers[0].pvBuffer,
							  OutBuffers[0].cbBuffer,
							  0);
				if(cbData == SOCKET_ERROR || cbData == 0) {
					wprintf(L"Error %d sending data to server (2)\n", 
						WSAGetLastError());
					tls_ctx->sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
					tls_ctx->sspi->DeleteSecurityContext(&tls_ctx->h_context);
					return SEC_E_INTERNAL_ERROR;
				}

				// Free output buffer.
				tls_ctx->sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
				OutBuffers[0].pvBuffer = NULL;
			}
		}

		// If InitializeSecurityContext returned SEC_E_INCOMPLETE_MESSAGE,
		// then we need to read more data from the server and try again.
		if(scRet == SEC_E_INCOMPLETE_MESSAGE) {
			continue;
		}

		// If InitializeSecurityContext returned SEC_E_OK, then the 
		// handshake completed successfully.

		if(scRet == SEC_E_OK) {
			// If the "extra" buffer contains data, this is encrypted application
			// protocol layer stuff. It needs to be saved. The application layer
			// will later decrypt it with DecryptMessage.

			if(InBuffers[1].BufferType == SECBUFFER_EXTRA)
			{
				pExtraData->pvBuffer = LocalAlloc(LPTR, InBuffers[1].cbBuffer);
				if(pExtraData->pvBuffer == NULL) {
					wprintf(L"Out of memory (2)\n");
					return SEC_E_INTERNAL_ERROR;
				}

				MoveMemory(pExtraData->pvBuffer,
					IoBuffer + (cbIoBuffer - InBuffers[1].cbBuffer),
					InBuffers[1].cbBuffer);

				pExtraData->cbBuffer   = InBuffers[1].cbBuffer;
				pExtraData->BufferType = SECBUFFER_TOKEN;

				// wprintf(L"%d bytes of app data was bundled with handshake data\n", pExtraData->cbBuffer);
			}
			else {
				pExtraData->pvBuffer   = NULL;
				pExtraData->cbBuffer   = 0;
				pExtraData->BufferType = SECBUFFER_EMPTY;
			}

			// Bail out to quit
			break;
		}

		// Check for fatal error.
		if(FAILED(scRet)) {
			wprintf(L"Error 0x%x returned by InitializeSecurityContext (2)\n", scRet);
			break;
		}

		// If InitializeSecurityContext returned SEC_I_INCOMPLETE_CREDENTIALS,
		// then the server just requested client authentication. 
		if(scRet == SEC_I_INCOMPLETE_CREDENTIALS) {
			// Busted. The server has requested client authentication and
			// the credential we supplied didn't contain a client certificate.

			// This function will read the list of trusted certificate
			// authorities ("issuers") that was received from the server
			// and attempt to find a suitable client certificate that
			// was issued by one of these. If this function is successful, 
			// then we will connect using the new certificate. Otherwise,
			// we will attempt to connect anonymously (using our current
			// credentials).
			
			get_new_client_credentials(tls_ctx);

			// Go around again.
			fDoRead = FALSE;
			scRet = SEC_I_CONTINUE_NEEDED;
			continue;
		}

		// Copy any leftover data from the "extra" buffer, and go around
		// again.

		if ( InBuffers[1].BufferType == SECBUFFER_EXTRA ) {
			MoveMemory(IoBuffer,
					   IoBuffer + (cbIoBuffer - InBuffers[1].cbBuffer),
					   InBuffers[1].cbBuffer);

			cbIoBuffer = InBuffers[1].cbBuffer;
		}
		else {
			cbIoBuffer = 0;
		}
	}

	// Delete the security context in the case of a fatal error.
	if(FAILED(scRet)) {
		tls_ctx->sspi->DeleteSecurityContext(&tls_ctx->h_context);
	}

	LocalFree(IoBuffer);

	return scRet;
}


static SECURITY_STATUS https_make_request(TlsContext *tls_ctx, CHAR *req, DWORD req_len, CHAR **out, int *length, vschannel_allocator afn) {
	SecPkgContext_StreamSizes Sizes;
	SECURITY_STATUS scRet;
	SecBufferDesc   Message;
	SecBuffer       Buffers[4];
	SecBuffer 	   *pDataBuffer;
	SecBuffer 	   *pExtraBuffer;
	SecBuffer       ExtraBuffer;

	PBYTE pbIoBuffer;
	DWORD cbIoBuffer;
	DWORD cbIoBufferLength;
	PBYTE pbMessage;
	DWORD cbMessage;

	INT   cbData;
	INT   i;
	DWORD req_offset;
	DWORD chunk_len;
	DWORD to_send;
	DWORD sent;


	// Read stream encryption properties.
	scRet = tls_ctx->sspi->QueryContextAttributes(&tls_ctx->h_context, SECPKG_ATTR_STREAM_SIZES, &Sizes);
	if(scRet != SEC_E_OK) {
		wprintf(L"Error 0x%x reading SECPKG_ATTR_STREAM_SIZES\n", scRet);
		return scRet;
	}

	// Allocate a working buffer. The plaintext sent to EncryptMessage
	// should never be more than 'Sizes.cbMaximumMessage', so a buffer 
	// size of this plus the header and trailer sizes should be safe enough.
	cbIoBufferLength = Sizes.cbHeader +  Sizes.cbMaximumMessage + Sizes.cbTrailer;

	pbIoBuffer = LocalAlloc(LPTR, cbIoBufferLength);
	if(pbIoBuffer == NULL) {
		wprintf(L"Out of memory (2)\n");
		return SEC_E_INTERNAL_ERROR;
	}
	
	// Build and send HTTP request in chunks no larger than cbMaximumMessage.
	// EncryptMessage expects plaintext <= cbMaximumMessage.
	pbMessage = pbIoBuffer + Sizes.cbHeader;
	req_offset = 0;
	while(req_offset < req_len) {
		chunk_len = req_len - req_offset;
		if(chunk_len > Sizes.cbMaximumMessage) {
			chunk_len = Sizes.cbMaximumMessage;
		}

		memcpy(pbMessage, req + req_offset, chunk_len);
		cbMessage = chunk_len;

		Buffers[0].pvBuffer     = pbIoBuffer;
		Buffers[0].cbBuffer     = Sizes.cbHeader;
		Buffers[0].BufferType   = SECBUFFER_STREAM_HEADER;

		Buffers[1].pvBuffer     = pbMessage;
		Buffers[1].cbBuffer     = cbMessage;
		Buffers[1].BufferType   = SECBUFFER_DATA;

		Buffers[2].pvBuffer     = pbMessage + cbMessage;
		Buffers[2].cbBuffer     = Sizes.cbTrailer;
		Buffers[2].BufferType   = SECBUFFER_STREAM_TRAILER;

		Buffers[3].BufferType   = SECBUFFER_EMPTY;

		Message.ulVersion       = SECBUFFER_VERSION;
		Message.cBuffers        = 4;
		Message.pBuffers        = Buffers;

		scRet = tls_ctx->sspi->EncryptMessage(&tls_ctx->h_context, 0, &Message, 0);
		if(FAILED(scRet)) {
			wprintf(L"Error 0x%x returned by EncryptMessage\n", scRet);
			LocalFree(pbIoBuffer);
			return scRet;
		}

		// Send all encrypted bytes for this chunk.
		to_send = Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer;
		sent = 0;
		while(sent < to_send) {
			cbData = send(tls_ctx->socket, (char*)pbIoBuffer + sent, (int)(to_send - sent), 0);
			if(cbData == SOCKET_ERROR || cbData == 0) {
				wprintf(L"Error %d sending data to server (3)\n", WSAGetLastError());
				tls_ctx->sspi->DeleteSecurityContext(&tls_ctx->h_context);
				LocalFree(pbIoBuffer);
				return SEC_E_INTERNAL_ERROR;
			}
			sent += (DWORD)cbData;
		}

		req_offset += chunk_len;
	}

	// Read data from server until done.
	INT buff_size = vsc_init_resp_buff_size;
	cbIoBuffer = 0;
	while(TRUE){
		// Read some data.
		if(0 == cbIoBuffer || scRet == SEC_E_INCOMPLETE_MESSAGE) {
			cbData = recv(tls_ctx->socket, pbIoBuffer + cbIoBuffer, cbIoBufferLength - cbIoBuffer, 0);
			if(cbData == SOCKET_ERROR) {
				wprintf(L"Error %d reading data from server\n", WSAGetLastError());
				scRet = SEC_E_INTERNAL_ERROR;
				break;
			}
			else if(cbData == 0) {
				// Server disconnected.
				if(cbIoBuffer) {
					wprintf(L"Server unexpectedly disconnected\n");
					scRet = SEC_E_INTERNAL_ERROR;
					LocalFree(pbIoBuffer);
					return scRet;
				}
				else {
					break;
				}
			}
			else {
				cbIoBuffer += cbData;
			}
		}

		// Attempt to decrypt the received data.
		Buffers[0].pvBuffer     = pbIoBuffer;
		Buffers[0].cbBuffer     = cbIoBuffer;
		Buffers[0].BufferType   = SECBUFFER_DATA;

		Buffers[1].BufferType   = SECBUFFER_EMPTY;
		Buffers[2].BufferType   = SECBUFFER_EMPTY;
		Buffers[3].BufferType   = SECBUFFER_EMPTY;

		Message.ulVersion       = SECBUFFER_VERSION;
		Message.cBuffers        = 4;
		Message.pBuffers        = Buffers;

		scRet = tls_ctx->sspi->DecryptMessage(&tls_ctx->h_context, &Message, 0, NULL);

		if(scRet == SEC_E_INCOMPLETE_MESSAGE) {
			// The input buffer contains only a fragment of an
			// encrypted record. Loop around and read some more
			// data.
			continue;
		}

		// Server signalled end of session
		if(scRet == SEC_I_CONTEXT_EXPIRED) {
			break;
		}

		if( scRet != SEC_E_OK && 
			scRet != SEC_I_RENEGOTIATE && 
			scRet != SEC_I_CONTEXT_EXPIRED)
		{
			wprintf(L"Error 0x%x returned by DecryptMessage\n", scRet);
			LocalFree(pbIoBuffer);
			return scRet;
		}

		// Locate data and (optional) extra buffers.
		pDataBuffer  = NULL;
		pExtraBuffer = NULL;
		for(i = 1; i < 4; i++) {
			if(pDataBuffer == NULL && Buffers[i].BufferType == SECBUFFER_DATA)
			{
				pDataBuffer = &Buffers[i];
				// wprintf(L"Buffers[%d].BufferType = SECBUFFER_DATA\n",i);
			}
			if(pExtraBuffer == NULL && Buffers[i].BufferType == SECBUFFER_EXTRA)
			{
				pExtraBuffer = &Buffers[i];
			}
		}

		// increase buffer size if we need
		int required_length = *length+(int)pDataBuffer->cbBuffer;
		if( required_length > buff_size ) {
			CHAR *a = afn(*out, required_length);
			if( a == NULL ) {
				scRet = SEC_E_INTERNAL_ERROR;
				LocalFree(pbIoBuffer);
				return scRet;
			}
			*out = a;
			buff_size = required_length;
		}
		// Copy the decrypted data to our output buffer
		memcpy(*out+*length, pDataBuffer->pvBuffer, (int)pDataBuffer->cbBuffer);
		*length += (int)pDataBuffer->cbBuffer;
		
		// Move any "extra" data to the input buffer.
		if(pExtraBuffer) {
			MoveMemory(pbIoBuffer, pExtraBuffer->pvBuffer, pExtraBuffer->cbBuffer);
			cbIoBuffer = pExtraBuffer->cbBuffer;
		}
		else {
			cbIoBuffer = 0;
		}

		if(scRet == SEC_I_RENEGOTIATE)
		{
			// The server wants to perform another handshake sequence.
			scRet = client_handshake_loop(tls_ctx, FALSE, &ExtraBuffer);
			if(scRet != SEC_E_OK) {
				LocalFree(pbIoBuffer);
				return scRet;
			}

			// Move any "extra" data to the input buffer.
			if(ExtraBuffer.pvBuffer)
			{
				MoveMemory(pbIoBuffer, ExtraBuffer.pvBuffer, ExtraBuffer.cbBuffer);
				cbIoBuffer = ExtraBuffer.cbBuffer;
				LocalFree(ExtraBuffer.pvBuffer);
			}
		}
	}

	LocalFree(pbIoBuffer);
	return SEC_E_OK;
}


static DWORD verify_server_certificate( PCCERT_CONTEXT  pServerCert, LPWSTR host, DWORD dwCertFlags) {
	HTTPSPolicyCallbackData  polHttps;
	CERT_CHAIN_POLICY_PARA   PolicyPara;
	CERT_CHAIN_POLICY_STATUS PolicyStatus;
	CERT_CHAIN_PARA          ChainPara;
	PCCERT_CHAIN_CONTEXT     pChainContext = NULL;

	CHAR *rgszUsages[] = {  szOID_PKIX_KP_SERVER_AUTH,
							szOID_SERVER_GATED_CRYPTO,
							szOID_SGC_NETSCAPE };
	DWORD cUsages = sizeof(rgszUsages) / sizeof(CHAR*);

	PWSTR   pwszServerName = NULL;
	DWORD   cchServerName;
	DWORD   Status;

	if(pServerCert == NULL)
	{
		Status = SEC_E_WRONG_PRINCIPAL;
		goto cleanup;
	}

	if(host == NULL || wcslen(host) == 0) {
		Status = SEC_E_WRONG_PRINCIPAL;
		goto cleanup;
	}

	// Build certificate chain.

	ZeroMemory(&ChainPara, sizeof(ChainPara));
	ChainPara.cbSize = sizeof(ChainPara);
	ChainPara.RequestedUsage.dwType = USAGE_MATCH_TYPE_OR;
	ChainPara.RequestedUsage.Usage.cUsageIdentifier     = cUsages;
	ChainPara.RequestedUsage.Usage.rgpszUsageIdentifier = rgszUsages;

	// Best-effort TLS revocation check: detect a positively revoked leaf
	// certificate, but let policy evaluation ignore unknown/offline status.
	if(!CertGetCertificateChain(NULL, pServerCert, NULL, pServerCert->hCertStore, &ChainPara,
		CERT_CHAIN_CACHE_END_CERT |
		CERT_CHAIN_REVOCATION_CHECK_END_CERT |
		CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT,
		NULL, &pChainContext)) {
		Status = GetLastError();
		wprintf(L"Error 0x%x returned by CertGetCertificateChain!\n", Status);
		goto cleanup;
	}

	// Validate certificate chain.
	ZeroMemory(&polHttps, sizeof(HTTPSPolicyCallbackData));
	polHttps.cbStruct           = sizeof(HTTPSPolicyCallbackData);
	polHttps.dwAuthType         = AUTHTYPE_SERVER;
	polHttps.fdwChecks          = dwCertFlags;
	polHttps.pwszServerName     = host;

	memset(&PolicyPara, 0, sizeof(PolicyPara));
	PolicyPara.cbSize            = sizeof(PolicyPara);
	PolicyPara.dwFlags           = CERT_CHAIN_POLICY_IGNORE_ALL_REV_UNKNOWN_FLAGS;
	PolicyPara.pvExtraPolicyPara = &polHttps;

	memset(&PolicyStatus, 0, sizeof(PolicyStatus));
	PolicyStatus.cbSize = sizeof(PolicyStatus);

	if(!CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL, pChainContext, &PolicyPara, &PolicyStatus)){
		Status = GetLastError();
		wprintf(L"Error 0x%x returned by CertVerifyCertificateChainPolicy!\n", Status);
		goto cleanup;
	}

	if(PolicyStatus.dwError) {
		Status = PolicyStatus.dwError;
		goto cleanup;
	}


	Status = SEC_E_OK;

cleanup:

	if(pChainContext)
	{
		CertFreeCertificateChain(pChainContext);
	}

	if(pwszServerName)
	{
		LocalFree(pwszServerName);
	}

	return Status;
}


static void get_new_client_credentials(TlsContext *tls_ctx) {
	CredHandle hCreds;
	SecPkgContext_IssuerListInfoEx IssuerListInfo;
	PCCERT_CHAIN_CONTEXT pChainContext;
	CERT_CHAIN_FIND_BY_ISSUER_PARA FindByIssuerPara;
	PCCERT_CONTEXT  pCertContext;
	TimeStamp       tsExpiry;
	SECURITY_STATUS Status;

	// Read list of trusted issuers from schannel.
	Status = tls_ctx->sspi->QueryContextAttributes(&tls_ctx->h_context, SECPKG_ATTR_ISSUER_LIST_EX, (PVOID)&IssuerListInfo);
	if(Status != SEC_E_OK) {
		wprintf(L"Error 0x%x querying issuer list info\n", Status);
		return;
	}

	// Enumerate the client certificates.

	ZeroMemory(&FindByIssuerPara, sizeof(FindByIssuerPara));

	FindByIssuerPara.cbSize = sizeof(FindByIssuerPara);
	FindByIssuerPara.pszUsageIdentifier = szOID_PKIX_KP_CLIENT_AUTH;
	FindByIssuerPara.dwKeySpec = 0;
	FindByIssuerPara.cIssuer   = IssuerListInfo.cIssuers;
	FindByIssuerPara.rgIssuer  = IssuerListInfo.aIssuers;

	pChainContext = NULL;

	while(TRUE) {
		// Find a certificate chain.
		pChainContext = CertFindChainInStore(tls_ctx->cert_store,
											 X509_ASN_ENCODING,
											 0,
											 CERT_CHAIN_FIND_BY_ISSUER,
											 &FindByIssuerPara,
											 pChainContext);
		if(pChainContext == NULL) {
			wprintf(L"Error 0x%x finding cert chain\n", GetLastError());
			break;
		}

		// Get pointer to leaf certificate context.
		pCertContext = pChainContext->rgpChain[0]->rgpElement[0]->pCertContext;

		// Create schannel credential.
		tls_ctx->schannel_cred.dwVersion = SCHANNEL_CRED_VERSION;
		tls_ctx->schannel_cred.cCreds = 1;
		tls_ctx->schannel_cred.paCred = &pCertContext;

		Status = tls_ctx->sspi->AcquireCredentialsHandle(
							NULL,                   // Name of principal
							UNISP_NAME_W,           // Name of package
							SECPKG_CRED_OUTBOUND,   // Flags indicating use
							NULL,                   // Pointer to logon ID
							&tls_ctx->schannel_cred,          // Package specific data
							NULL,                   // Pointer to GetKey() func
							NULL,                   // Value to pass to GetKey()
							&hCreds,                // (out) Cred Handle
							&tsExpiry);             // (out) Lifetime (optional)
		if(Status != SEC_E_OK) {
			wprintf(L"Error 0x%x returned by AcquireCredentialsHandle\n", Status);
			continue;
		}

		// Destroy the old credentials.
		tls_ctx->sspi->FreeCredentialsHandle(&tls_ctx->h_client_creds);

		tls_ctx->h_client_creds = hCreds;

		//
		// As you can see, this sample code maintains a single credential
		// handle, replacing it as necessary. This is a little unusual.
		//
		// Many applications maintain a global credential handle that's
		// anonymous (that is, it doesn't contain a client certificate),
		// which is used to connect to all servers. If a particular server
		// should require client authentication, then a new credential 
		// is created for use when connecting to that server. The global
		// anonymous credential is retained for future connections to
		// other servers.
		//
		// Maintaining a single anonymous credential that's used whenever
		// possible is most efficient, since creating new credentials all
		// the time is rather expensive.
		//

		break;
	}
}
