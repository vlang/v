module http

#define vschannel_test_credentials_initialized(ctx) ((ctx)->creds_initialized)

fn C.vschannel_test_credentials_initialized(&C.TlsContext) bool

fn test_vschannel_h1_header_error_cleans_up_credentials() {
	mut ctx := C.new_tls_context()
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&ctx, C.BOOL(0))
	assert C.vschannel_test_credentials_initialized(&ctx)
	req := Request{}
	req.vschannel_h1_on_open(&ctx, .trace, 'localhost', 443, '/', 'payload', Header{}) or {
		assert err.msg() == 'net.http: TRACE requests must not carry a body'
		assert !C.vschannel_test_credentials_initialized(&ctx)
		return
	}
	C.vschannel_cleanup(&ctx)
	assert false, 'TRACE body must fail before sending an HTTP request'
}

struct SchannelProgressAbort {
	Error
}

fn (_ SchannelProgressAbort) code() int { return 12345 }

fn schannel_abort_progress(_ &Request, _ []u8, _ u64) ! {
	return SchannelProgressAbort{}
}

fn test_vschannel_h1_progress_abort_preserves_error_for_outer_retry_loop() {
	req := Request{ on_progress: schannel_abort_progress }
	response := 'HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok'
	req.vschannel_finish_response(response.str, response.len, 0) or {
		assert err is ProgressCallbackError
		if err is ProgressCallbackError {
			assert err.cause is SchannelProgressAbort
			assert err.cause.code() == 12345
		}
		return
	}
	assert false, 'progress callback error must stop the request'
}
