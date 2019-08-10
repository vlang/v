#include <vschannel.h>


// TODO: joe-c
// create context struct instead of using global

// Proxy
CHAR *  psz_proxy_server  = "proxy";
INT     i_proxy_port      = 80;

// Options
INT     port_number     = 443;
BOOL    use_proxy       = FALSE;
DWORD   protocol        = 0;
ALG_ID  aid_key_exch    = 0;

// Cred store
HCERTSTORE      cert_store = NULL;
SCHANNEL_CRED   schannel_cred;

// Loaded sec lib
HMODULE g_hsecurity = NULL;

// SSPI
PSecurityFunctionTable sspi;

struct TlsContext tls_ctx;

struct TlsContext {
	SOCKET  Socket;
	WSADATA WsaData;
	CredHandle hClientCreds;
	CtxtHandle hContext;
	BOOL fCredsInitialized;
	BOOL fContextInitialized;
	PCCERT_CONTEXT pRemoteCertContext;
};

struct TlsContext new_tls_context() {
	return (struct TlsContext) {
		.Socket = INVALID_SOCKET,
		.fCredsInitialized = FALSE,
		.fContextInitialized = FALSE,
		.pRemoteCertContext = NULL
	};
};

BOOL load_security_library(void) {
	INIT_SECURITY_INTERFACE pInitSecurityInterface;

	//  Load Security DLL
	g_hsecurity = LoadLibraryA("schannel.dll");
	if(g_hsecurity == NULL) {
		printf("Error 0x%x loading %s.\n", GetLastError(), "schannel.dll");
		return FALSE;
	}

	pInitSecurityInterface = (INIT_SECURITY_INTERFACE)GetProcAddress(g_hsecurity, "InitSecurityInterfaceA");
	
	if(pInitSecurityInterface == NULL) {
		printf("Error 0x%x reading InitSecurityInterface entry point.\n", GetLastError());
		return FALSE;
	}

	sspi = pInitSecurityInterface();

	if(sspi == NULL) {
		printf("Error 0x%x reading security interface.\n",
			   GetLastError());
		return FALSE;
	}

	return TRUE;
}

void unload_security_library(void) {
	FreeLibrary(g_hsecurity);
	g_hsecurity = NULL;
}

void vschannel_cleanup() {
	// Free the server certificate context.
	if(tls_ctx.pRemoteCertContext) {
		CertFreeCertificateContext(tls_ctx.pRemoteCertContext);
		tls_ctx.pRemoteCertContext = NULL;
	}

	// Free SSPI context handle.
	if(tls_ctx.fContextInitialized) {
		sspi->DeleteSecurityContext(&tls_ctx.hContext);
		tls_ctx.fContextInitialized = FALSE;
	}

	// Free SSPI credentials handle.
	if(tls_ctx.fCredsInitialized) {
		sspi->FreeCredentialsHandle(&tls_ctx.hClientCreds);
		tls_ctx.fCredsInitialized = FALSE;
	}
	
	// Close socket.
	if(tls_ctx.Socket != INVALID_SOCKET) {
		closesocket(tls_ctx.Socket);
	}

	// Shutdown WinSock subsystem.
	WSACleanup();
	
	// Close "MY" certificate store.
	if(cert_store) {
		CertCloseStore(cert_store, 0);
	}

	unload_security_library();
}

void vschannel_init() {
	tls_ctx = new_tls_context();

	if(!load_security_library()) {
		printf("Error initializing the security library\n");
		vschannel_cleanup();
	}

	// Initialize the WinSock subsystem.
	if(WSAStartup(0x0101, &tls_ctx.WsaData) == SOCKET_ERROR) {
		printf("Error %d returned by WSAStartup\n", GetLastError());
		vschannel_cleanup();
	}

	// Create credentials.
	if(create_credentials(&tls_ctx.hClientCreds)) {
		printf("Error creating credentials\n");
		vschannel_cleanup();
	}
	tls_ctx.fCredsInitialized = TRUE;
}

INT request(CHAR *host, CHAR *req, CHAR *out)
{
	SecBuffer  ExtraData;
	SECURITY_STATUS Status;

	INT i;
	INT iOption;
	PCHAR pszOption;

	INT resp_length = 0;

	protocol = SP_PROT_TLS1_2_CLIENT;

	// Connect to server.
	if(connect_to_server(host, port_number, &tls_ctx.Socket)) {
		printf("Error connecting to server\n");
		vschannel_cleanup();
		return resp_length;
	}

	// Perform handshake
	if(perform_client_handshake(tls_ctx.Socket, &tls_ctx.hClientCreds, host, &tls_ctx.hContext, &ExtraData)) {
		printf("Error performing handshake\n");
		vschannel_cleanup();
		return resp_length;
	}
	tls_ctx.fContextInitialized = TRUE;

	// Authenticate server's credentials.

	// Get server's certificate.
	Status = sspi->QueryContextAttributes(&tls_ctx.hContext,
											 SECPKG_ATTR_REMOTE_CERT_CONTEXT,
											 (PVOID)&tls_ctx.pRemoteCertContext);
	if(Status != SEC_E_OK) {
		printf("Error 0x%x querying remote certificate\n", Status);
		vschannel_cleanup();
		return resp_length;
	}

	// Attempt to validate server certificate.
	Status = verify_server_certificate(tls_ctx.pRemoteCertContext, host,0);
	if(Status) {
		// The server certificate did not validate correctly. At this
		// point, we cannot tell if we are connecting to the correct 
		// server, or if we are connecting to a "man in the middle" 
		// attack server.

		// It is therefore best if we abort the connection.

		printf("Error 0x%x authenticating server credentials!\n", Status);
		vschannel_cleanup();
		return resp_length;
	}

	// Free the server certificate context.
	CertFreeCertificateContext(tls_ctx.pRemoteCertContext);
	tls_ctx.pRemoteCertContext = NULL;

	// Request from server
	if(https_make_request(tls_ctx.Socket, &tls_ctx.hClientCreds, &tls_ctx.hContext,  req, out, &resp_length)) {
		vschannel_cleanup();
		return resp_length;
	}
	
	
	// Send a close_notify alert to the server and
	// close down the connection.
	if(disconnect_from_server(tls_ctx.Socket, &tls_ctx.hClientCreds, &tls_ctx.hContext)) {
		printf("Error disconnecting from server\n");
		vschannel_cleanup();
		return resp_length;
	}
	tls_ctx.fContextInitialized = FALSE;
	tls_ctx.Socket = INVALID_SOCKET;

	return resp_length;
}


static SECURITY_STATUS create_credentials(PCredHandle phCreds) {
	TimeStamp       tsExpiry;
	SECURITY_STATUS Status;

	DWORD           cSupportedAlgs = 0;
	ALG_ID          rgbSupportedAlgs[16];

	PCCERT_CONTEXT  pCertContext = NULL;

	// Open the "MY" certificate store, which is where Internet Explorer
	// stores its client certificates.
	if(cert_store == NULL) {
		cert_store = CertOpenSystemStore(0, "MY");

		if(!cert_store) {
			printf("Error 0x%x returned by CertOpenSystemStore\n", 
			GetLastError());
			return SEC_E_NO_CREDENTIALS;
		}
	}

	// Build Schannel credential structure. Currently, this sample only
	// specifies the protocol to be used (and optionally the certificate, 
	// of course). Real applications may wish to specify other parameters 
	// as well.

	ZeroMemory(&schannel_cred, sizeof(schannel_cred));

	schannel_cred.dwVersion  = SCHANNEL_CRED_VERSION;
	if(pCertContext)
	{
		schannel_cred.cCreds     = 1;
		schannel_cred.paCred     = &pCertContext;
	}

	schannel_cred.grbitEnabledProtocols = protocol;

	if(aid_key_exch)
	{
		rgbSupportedAlgs[cSupportedAlgs++] = aid_key_exch;
	}

	if(cSupportedAlgs)
	{
		schannel_cred.cSupportedAlgs    = cSupportedAlgs;
		schannel_cred.palgSupportedAlgs = rgbSupportedAlgs;
	}

	schannel_cred.dwFlags |= SCH_CRED_NO_DEFAULT_CREDS;

	// The SCH_CRED_MANUAL_CRED_VALIDATION flag is specified because
	// this sample verifies the server certificate manually. 
	// Applications that expect to run on WinNT, Win9x, or WinME 
	// should specify this flag and also manually verify the server
	// certificate. Applications running on newer versions of Windows can
	// leave off this flag, in which case the InitializeSecurityContext
	// function will validate the server certificate automatically.
	// schannel_cred.dwFlags |= SCH_CRED_MANUAL_CRED_VALIDATION;

	// Create an SSPI credential.

	Status = sspi->AcquireCredentialsHandle(
						NULL,                   // Name of principal    
						UNISP_NAME_A,           // Name of package
						SECPKG_CRED_OUTBOUND,   // Flags indicating use
						NULL,                   // Pointer to logon ID
						&schannel_cred,          // Package specific data
						NULL,                   // Pointer to GetKey() func
						NULL,                   // Value to pass to GetKey()
						phCreds,                // (out) Cred Handle
						&tsExpiry);             // (out) Lifetime (optional)
	if(Status != SEC_E_OK) {
		printf("Error 0x%x returned by AcquireCredentialsHandle\n", Status);
		goto cleanup;
	}

cleanup:

	// Free the certificate context. Schannel has already made its own copy.

	if(pCertContext) {
		CertFreeCertificateContext(pCertContext);
	}


	return Status;
}


static INT connect_to_server( CHAR *host, INT port_number, SOCKET *pSocket) {
	SOCKET Socket;
	struct sockaddr_in sin;
	struct hostent *hp;

	Socket = socket(PF_INET, SOCK_STREAM, 0);
	if(Socket == INVALID_SOCKET) {
		printf("Error %d creating socket\n", WSAGetLastError());
		return WSAGetLastError();
	}

	if(use_proxy) {
		sin.sin_family = AF_INET;
		sin.sin_port = ntohs((u_short)i_proxy_port);

		if((hp = gethostbyname(psz_proxy_server)) == NULL)
		{
			printf("Error %d returned by gethostbyname\n", WSAGetLastError());
			return WSAGetLastError();
		}
		else
		{
			memcpy(&sin.sin_addr, hp->h_addr, 4);
		}
	}
	else {
		sin.sin_family = AF_INET;
		sin.sin_port = htons((u_short)port_number);

		if((hp = gethostbyname(host)) == NULL)
		{
			printf("Error %d returned by gethostbyname\n", WSAGetLastError());
			return WSAGetLastError();
		}
		else {
			memcpy(&sin.sin_addr, hp->h_addr, 4);
		}
	}

	if(connect(Socket, (struct sockaddr *)&sin, sizeof(sin)) == SOCKET_ERROR) {
		printf("Error %d connecting to \"%s\" (%s)\n", 
			WSAGetLastError(),
			host, 
			inet_ntoa(sin.sin_addr));
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
			printf("Error %d sending message to proxy!\n", WSAGetLastError());
			return WSAGetLastError();
		}

		// Receive message from proxy server
		cbMessage = recv(Socket, pbMessage, 200, 0);
		if(cbMessage == SOCKET_ERROR) {
			printf("Error %d receiving message from proxy\n", WSAGetLastError());
			return WSAGetLastError();
		}

		// this sample is limited but in normal use it 
		// should continue to receive until CR LF CR LF is received
	}

	*pSocket = Socket;

	return SEC_E_OK;
}


static LONG disconnect_from_server(SOCKET Socket, PCredHandle phCreds, CtxtHandle *phContext) {
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

	Status = sspi->ApplyControlToken(phContext, &OutBuffer);

	if(FAILED(Status)) {
		printf("Error 0x%x returned by ApplyControlToken\n", Status);
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

	Status = sspi->InitializeSecurityContext(
		phCreds, phContext, NULL, dwSSPIFlags, 0, SECURITY_NATIVE_DREP,
		NULL, 0, phContext, &OutBuffer, &dwSSPIOutFlags, &tsExpiry);

	if(FAILED(Status))  {
		printf("Error 0x%x returned by InitializeSecurityContext\n", Status);
		goto cleanup;
	}

	pbMessage = OutBuffers[0].pvBuffer;
	cbMessage = OutBuffers[0].cbBuffer;

	// Send the close notify message to the server.

	if(pbMessage != NULL && cbMessage != 0) {
		cbData = send(Socket, pbMessage, cbMessage, 0);
		if(cbData == SOCKET_ERROR || cbData == 0) {
			Status = WSAGetLastError();
			printf("Error %d sending close notify\n", Status);
			goto cleanup;
		}

		// Free output buffer.
		sspi->FreeContextBuffer(pbMessage);
	}
	

cleanup:

	// Free the security context.
	sspi->DeleteSecurityContext(phContext);

	// Close the socket.
	closesocket(Socket);

	return Status;
}


static
SECURITY_STATUS
perform_client_handshake(
	SOCKET          Socket,         // in
	PCredHandle     phCreds,        // in
	CHAR *          host,  // in
	CtxtHandle *    phContext,      // out
	SecBuffer *     pExtraData)     // out
{
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

	scRet = sspi->InitializeSecurityContext(
					phCreds,
					NULL,
					host,
					dwSSPIFlags,
					0,
					SECURITY_NATIVE_DREP,
					NULL,
					0,
					phContext,
					&OutBuffer,
					&dwSSPIOutFlags,
					&tsExpiry);

	if(scRet != SEC_I_CONTINUE_NEEDED)
	{
		printf("Error %d returned by InitializeSecurityContext (1)\n", scRet);
		return scRet;
	}

	// Send response to server if there is one.
	if(OutBuffers[0].cbBuffer != 0 && OutBuffers[0].pvBuffer != NULL)
	{
		cbData = send(Socket, OutBuffers[0].pvBuffer, OutBuffers[0].cbBuffer, 0);
		if(cbData == SOCKET_ERROR || cbData == 0) {
			printf("Error %d sending data to server (1)\n", WSAGetLastError());
			sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
			sspi->DeleteSecurityContext(phContext);
			return SEC_E_INTERNAL_ERROR;
		}

		// Free output buffer.
		sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
		OutBuffers[0].pvBuffer = NULL;
	}

	return client_handshake_loop(Socket, phCreds, phContext, TRUE, pExtraData);
}


static SECURITY_STATUS client_handshake_loop(
	SOCKET          Socket,         // in
	PCredHandle     phCreds,        // in
	CtxtHandle *    phContext,      // in, out
	BOOL            fDoInitialRead, // in
	SecBuffer *     pExtraData)     // out
{
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

	IoBuffer = LocalAlloc(LMEM_FIXED, IO_BUFFER_SIZE);
	if(IoBuffer == NULL)
	{
		printf("Out of memory (1)\n");
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
				cbData = recv(Socket, 
							  IoBuffer + cbIoBuffer, 
							  IO_BUFFER_SIZE - cbIoBuffer, 
							  0);
				if(cbData == SOCKET_ERROR) {
					printf("Error %d reading data from server\n", WSAGetLastError());
					scRet = SEC_E_INTERNAL_ERROR;
					break;
				}
				else if(cbData == 0) {
					printf("Server unexpectedly disconnected\n");
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

		scRet = sspi->InitializeSecurityContext(
			phCreds, phContext,	NULL, dwSSPIFlags, 0, SECURITY_NATIVE_DREP,
			&InBuffer, 0, NULL, &OutBuffer, &dwSSPIOutFlags, &tsExpiry);

		// If InitializeSecurityContext was successful (or if the error was 
		// one of the special extended ones), send the contends of the output
		// buffer to the server.

		if(scRet == SEC_E_OK ||
		   scRet == SEC_I_CONTINUE_NEEDED ||
		   FAILED(scRet) && (dwSSPIOutFlags & ISC_RET_EXTENDED_ERROR)) {
			if(OutBuffers[0].cbBuffer != 0 && OutBuffers[0].pvBuffer != NULL) {
				cbData = send(Socket,
							  OutBuffers[0].pvBuffer,
							  OutBuffers[0].cbBuffer,
							  0);
				if(cbData == SOCKET_ERROR || cbData == 0) {
					printf("Error %d sending data to server (2)\n", 
						WSAGetLastError());
					sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
					sspi->DeleteSecurityContext(phContext);
					return SEC_E_INTERNAL_ERROR;
				}

				// Free output buffer.
				sspi->FreeContextBuffer(OutBuffers[0].pvBuffer);
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
				pExtraData->pvBuffer = LocalAlloc(LMEM_FIXED, 
												  InBuffers[1].cbBuffer);
				if(pExtraData->pvBuffer == NULL) {
					printf("Out of memory (2)\n");
					return SEC_E_INTERNAL_ERROR;
				}

				MoveMemory(pExtraData->pvBuffer,
					IoBuffer + (cbIoBuffer - InBuffers[1].cbBuffer),
					InBuffers[1].cbBuffer);

				pExtraData->cbBuffer   = InBuffers[1].cbBuffer;
				pExtraData->BufferType = SECBUFFER_TOKEN;

				// printf("%d bytes of app data was bundled with handshake data\n", pExtraData->cbBuffer);
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
			printf("Error 0x%x returned by InitializeSecurityContext (2)\n", scRet);
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
			
			get_new_client_credentials(phCreds, phContext);

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
		sspi->DeleteSecurityContext(phContext);
	}

	LocalFree(IoBuffer);

	return scRet;
}


static SECURITY_STATUS https_make_request(SOCKET Socket, PCredHandle phCreds, CtxtHandle *phContext, CHAR *req, CHAR *out, int *length) {
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
	scRet = sspi->QueryContextAttributes(phContext, SECPKG_ATTR_STREAM_SIZES, &Sizes);
	if(scRet != SEC_E_OK) {
		printf("Error 0x%x reading SECPKG_ATTR_STREAM_SIZES\n", scRet);
		return scRet;
	}

	// Allocate a working buffer. The plaintext sent to EncryptMessage
	// should never be more than 'Sizes.cbMaximumMessage', so a buffer 
	// size of this plus the header and trailer sizes should be safe enough.
	cbIoBufferLength = Sizes.cbHeader +  Sizes.cbMaximumMessage + Sizes.cbTrailer;

	pbIoBuffer = LocalAlloc(LMEM_FIXED, cbIoBufferLength);
	if(pbIoBuffer == NULL) {
		printf("Out of memory (2)\n");
		return SEC_E_INTERNAL_ERROR;
	}
	
	// Build an HTTP request to send to the server.

	// Build the HTTP request offset into the data buffer by "header size"
	// bytes. This enables Schannel to perform the encryption in place,
	// which is a significant performance win.
	pbMessage = pbIoBuffer + Sizes.cbHeader;

	// Build HTTP request. Note that I'm assuming that this is less than
	// the maximum message size. If it weren't, it would have to be broken up.
	sprintf(pbMessage,  req);

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

	scRet = sspi->EncryptMessage(phContext, 0, &Message, 0);

	if(FAILED(scRet)) {
		printf("Error 0x%x returned by EncryptMessage\n", scRet);
		return scRet;
	}

	// Send the encrypted data to the server.
	cbData = send(Socket, pbIoBuffer, Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer, 0);
	if(cbData == SOCKET_ERROR || cbData == 0) {
		printf("Error %d sending data to server (3)\n",  WSAGetLastError());
		sspi->DeleteSecurityContext(phContext);
		return SEC_E_INTERNAL_ERROR;
	}

	// Read data from server until done.
	cbIoBuffer = 0;
	while(TRUE){
		// Read some data.
		if(0 == cbIoBuffer || scRet == SEC_E_INCOMPLETE_MESSAGE) {
			cbData = recv(Socket, pbIoBuffer + cbIoBuffer, cbIoBufferLength - cbIoBuffer, 0);
			if(cbData == SOCKET_ERROR) {
				printf("Error %d reading data from server\n", WSAGetLastError());
				scRet = SEC_E_INTERNAL_ERROR;
				break;
			}
			else if(cbData == 0) {
				// Server disconnected.
				if(cbIoBuffer) {
					printf("Server unexpectedly disconnected\n");
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

		scRet = sspi->DecryptMessage(phContext, &Message, 0, NULL);

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
			printf("Error 0x%x returned by DecryptMessage\n", scRet);
			return scRet;
		}

		// Locate data and (optional) extra buffers.
		pDataBuffer  = NULL;
		pExtraBuffer = NULL;
		for(i = 1; i < 4; i++) {
			if(pDataBuffer == NULL && Buffers[i].BufferType == SECBUFFER_DATA)
			{
				pDataBuffer = &Buffers[i];
				// printf("Buffers[%d].BufferType = SECBUFFER_DATA\n",i);
			}
			if(pExtraBuffer == NULL && Buffers[i].BufferType == SECBUFFER_EXTRA)
			{
				pExtraBuffer = &Buffers[i];
			}
		}

		// Copy the decrypted data to our output buffer
		*length += (int)pDataBuffer->cbBuffer;
		memcpy(out, pDataBuffer->pvBuffer, (int)pDataBuffer->cbBuffer);
		out += (int)pDataBuffer->cbBuffer;
		
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
			scRet = client_handshake_loop(Socket, phCreds, phContext, FALSE, &ExtraBuffer);
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


static DWORD verify_server_certificate( PCCERT_CONTEXT  pServerCert, PSTR host, DWORD dwCertFlags) {
	HTTPSPolicyCallbackData  polHttps;
	CERT_CHAIN_POLICY_PARA   PolicyPara;
	CERT_CHAIN_POLICY_STATUS PolicyStatus;
	CERT_CHAIN_PARA          ChainPara;
	PCCERT_CHAIN_CONTEXT     pChainContext = NULL;

	CHAR *rgszUsages[] = {  szOID_PKIX_KP_SERVER_AUTH,
							szOID_SERVER_GATED_CRYPTO,
							szOID_SGC_NETSCAPE };
	DWORD cUsages = sizeof(rgszUsages) / sizeof(LPSTR);

	PWSTR   pwszServerName = NULL;
	DWORD   cchServerName;
	DWORD   Status;

	if(pServerCert == NULL)
	{
		Status = SEC_E_WRONG_PRINCIPAL;
		goto cleanup;
	}

	// Convert server name to unicode.
	if(host == NULL || strlen(host) == 0) {
		Status = SEC_E_WRONG_PRINCIPAL;
		goto cleanup;
	}

	cchServerName = MultiByteToWideChar(CP_ACP, 0, host, -1, NULL, 0);
	pwszServerName = LocalAlloc(LMEM_FIXED, cchServerName * sizeof(WCHAR));
	if(pwszServerName == NULL) {
		Status = SEC_E_INSUFFICIENT_MEMORY;
		goto cleanup;
	}
	cchServerName = MultiByteToWideChar(CP_ACP, 0, host, -1, pwszServerName, cchServerName);
	if(cchServerName == 0) {
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
		printf("Error 0x%x returned by CertGetCertificateChain!\n", Status);
		goto cleanup;
	}

	// Validate certificate chain.
	ZeroMemory(&polHttps, sizeof(HTTPSPolicyCallbackData));
	polHttps.cbStruct           = sizeof(HTTPSPolicyCallbackData);
	polHttps.dwAuthType         = AUTHTYPE_SERVER;
	polHttps.fdwChecks          = dwCertFlags;
	polHttps.pwszServerName     = pwszServerName;

	memset(&PolicyPara, 0, sizeof(PolicyPara));
	PolicyPara.cbSize            = sizeof(PolicyPara);
	PolicyPara.pvExtraPolicyPara = &polHttps;

	memset(&PolicyStatus, 0, sizeof(PolicyStatus));
	PolicyStatus.cbSize = sizeof(PolicyStatus);

	if(!CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL, pChainContext, &PolicyPara, &PolicyStatus)){
		Status = GetLastError();
		printf("Error 0x%x returned by CertVerifyCertificateChainPolicy!\n", Status);
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


static void get_new_client_credentials(CredHandle *phCreds, CtxtHandle *phContext) {
	CredHandle hCreds;
	SecPkgContext_IssuerListInfoEx IssuerListInfo;
	PCCERT_CHAIN_CONTEXT pChainContext;
	CERT_CHAIN_FIND_BY_ISSUER_PARA FindByIssuerPara;
	PCCERT_CONTEXT  pCertContext;
	TimeStamp       tsExpiry;
	SECURITY_STATUS Status;

	// Read list of trusted issuers from schannel.
	Status = sspi->QueryContextAttributes(phContext, SECPKG_ATTR_ISSUER_LIST_EX, (PVOID)&IssuerListInfo);
	if(Status != SEC_E_OK) {
		printf("Error 0x%x querying issuer list info\n", Status);
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
		pChainContext = CertFindChainInStore(cert_store,
											 X509_ASN_ENCODING,
											 0,
											 CERT_CHAIN_FIND_BY_ISSUER,
											 &FindByIssuerPara,
											 pChainContext);
		if(pChainContext == NULL) {
			printf("Error 0x%x finding cert chain\n", GetLastError());
			break;
		}

		// Get pointer to leaf certificate context.
		pCertContext = pChainContext->rgpChain[0]->rgpElement[0]->pCertContext;

		// Create schannel credential.
		schannel_cred.dwVersion = SCHANNEL_CRED_VERSION;
		schannel_cred.cCreds = 1;
		schannel_cred.paCred = &pCertContext;

		Status = sspi->AcquireCredentialsHandle(
							NULL,                   // Name of principal
							UNISP_NAME_A,           // Name of package
							SECPKG_CRED_OUTBOUND,   // Flags indicating use
							NULL,                   // Pointer to logon ID
							&schannel_cred,          // Package specific data
							NULL,                   // Pointer to GetKey() func
							NULL,                   // Value to pass to GetKey()
							&hCreds,                // (out) Cred Handle
							&tsExpiry);             // (out) Lifetime (optional)
		if(Status != SEC_E_OK) {
			printf("Error 0x%x returned by AcquireCredentialsHandle\n", Status);
			continue;
		}

		// Destroy the old credentials.
		sspi->FreeCredentialsHandle(phCreds);

		*phCreds = hCreds;

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
	

