#include <vschannel.h>
#include <sspi.h>

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
	BOOL                   creds_initialized;
	BOOL                   context_initialized;
};

TlsContext new_tls_context() {
	return (struct TlsContext) {
		.cert_store            = NULL,
		.socket                = INVALID_SOCKET,
		.creds_initialized     = FALSE,
		.context_initialized   = FALSE,
		.p_pemote_cert_context = NULL
	};
};

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
}

void vschannel_init(TlsContext *tls_ctx) {
	tls_ctx->sspi = InitSecurityInterface();

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

INT request(TlsContext *tls_ctx, INT iport, LPWSTR host, CHAR *req, CHAR **out)
{
	SecBuffer  ExtraData;
	SECURITY_STATUS Status;

	INT i;
	INT iOption;
	PCHAR pszOption;

	INT resp_length = 0;

	protocol = SP_PROT_TLS1_2_CLIENT;

	port_number = iport;

	// Connect to server.
	if(connect_to_server(tls_ctx, host, port_number)) {
		wprintf(L"Error connecting to server\n");
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}

	// Perform handshake
	if(perform_client_handshake(tls_ctx, host, &ExtraData)) {
		wprintf(L"Error performing handshake\n");
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}
	tls_ctx->context_initialized = TRUE;

	// Authenticate server's credentials.

	// Get server's certificate.
	Status = tls_ctx->sspi->QueryContextAttributes(&tls_ctx->h_context,
											 SECPKG_ATTR_REMOTE_CERT_CONTEXT,
											 (PVOID)&tls_ctx->p_pemote_cert_context);
	if(Status != SEC_E_OK) {
		wprintf(L"Error 0x%x querying remote certificate\n", Status);
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}

	// Attempt to validate server certificate.
	Status = verify_server_certificate(tls_ctx->p_pemote_cert_context, host,0);
	if(Status) {
		// The server certificate did not validate correctly. At this
		// point, we cannot tell if we are connecting to the correct 
		// server, or if we are connecting to a "man in the middle" 
		// attack server.

		// It is therefore best if we abort the connection.

		wprintf(L"Error 0x%x authenticating server credentials!\n", Status);
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}

	// Free the server certificate context.
	CertFreeCertificateContext(tls_ctx->p_pemote_cert_context);
	tls_ctx->p_pemote_cert_context = NULL;

	// Request from server
	if(https_make_request(tls_ctx, req, out, &resp_length)) {
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}
	
	// Send a close_notify alert to the server and
	// close down the connection.
	if(disconnect_from_server(tls_ctx)) {
		wprintf(L"Error disconnecting from server\n");
		vschannel_cleanup(tls_ctx);
		return resp_length;
	}
	tls_ctx->context_initialized = FALSE;
	tls_ctx->socket = INVALID_SOCKET;

	return resp_length;
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

	// The SCH_CRED_MANUAL_CRED_VALIDATION flag is specified because
	// this sample verifies the server certificate manually. 
	// Applications that expect to run on WinNT, Win9x, or WinME 
	// should specify this flag and also manually verify the server
	// certificate. Applications running on newer versions of Windows can
	// leave off this flag, in which case the InitializeSecurityContext
	// function will validate the server certificate automatically.
	// tls_ctx->schannel_cred.dwFlags |= SCH_CRED_MANUAL_CRED_VALIDATION;

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
		wprintf(L"Error %d creating socket\n", WSAGetLastError());
		return WSAGetLastError();
	}

	LPWSTR connect_name = use_proxy ? psz_proxy_server : host;

	WCHAR service_name[10];
	int res = wsprintf(service_name, L"%d", port_number);

	if(WSAConnectByNameW(Socket,connect_name, service_name, &local_address_length, 
		&local_address, &remote_address_length, &remote_address, &tv, NULL) == SOCKET_ERROR) {
		wprintf(L"Error %d connecting to \"%s\" (%s)\n", 
			WSAGetLastError(),
			connect_name, 
			service_name);
		closesocket(Socket);
		return WSAGetLastError();
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
			wprintf(L"Error %d sending message to proxy!\n", WSAGetLastError());
			return WSAGetLastError();
		}

		// Receive message from proxy server
		cbMessage = recv(Socket, pbMessage, 200, 0);
		if(cbMessage == SOCKET_ERROR) {
			wprintf(L"Error %d receiving message from proxy\n", WSAGetLastError());
			return WSAGetLastError();
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
					NULL,
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


static SECURITY_STATUS https_make_request(TlsContext *tls_ctx, CHAR *req, CHAR **out, int *length) {
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

	DWORD cbData;
	INT   i;


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
	
	// Build an HTTP request to send to the server.

	// Build the HTTP request offset into the data buffer by "header size"
	// bytes. This enables Schannel to perform the encryption in place,
	// which is a significant performance win.
	pbMessage = pbIoBuffer + Sizes.cbHeader;

	// Build HTTP request. Note that I'm assuming that this is less than
	// the maximum message size. If it weren't, it would have to be broken up.
	sprintf(pbMessage, "%s", req);

	cbMessage = (DWORD)strlen(pbMessage);


	// Encrypt the HTTP request.
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
		return scRet;
	}

	// Send the encrypted data to the server.
	cbData = send(tls_ctx->socket, pbIoBuffer, Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer, 0);
	if(cbData == SOCKET_ERROR || cbData == 0) {
		wprintf(L"Error %d sending data to server (3)\n",  WSAGetLastError());
		tls_ctx->sspi->DeleteSecurityContext(&tls_ctx->h_context);
		return SEC_E_INTERNAL_ERROR;
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
			CHAR *a = VSCHANNEL_REALLOC(*out, required_length);
			if( a == NULL ) {
				scRet = SEC_E_INTERNAL_ERROR;
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
				return scRet;
			}

			// Move any "extra" data to the input buffer.
			if(ExtraBuffer.pvBuffer)
			{
				MoveMemory(pbIoBuffer, ExtraBuffer.pvBuffer, ExtraBuffer.cbBuffer);
				cbIoBuffer = ExtraBuffer.cbBuffer;
			}
		}
	}

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

	if(!CertGetCertificateChain(NULL, pServerCert, NULL, pServerCert->hCertStore, &ChainPara, 0, NULL, &pChainContext)) {
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
