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
