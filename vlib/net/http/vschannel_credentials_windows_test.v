module http

const vschannel_sec_e_no_credentials = -2146893042 // 0x8009030E (SEC_E_NO_CREDENTIALS)

fn test_vschannel_credentials_initialization() {
	mut ctx := C.new_tls_context()
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&ctx, C.BOOL(0))
	assert C.vschannel_last_error(&ctx) == 0
	C.vschannel_cleanup(&ctx)
}

fn test_vschannel_uninitialized_credentials_abort() {
	mut ctx := C.new_tls_context()
	res := C.vschannel_h2_connect(&ctx, 443, '127.0.0.1'.to_wide())
	assert res != 0
	assert C.vschannel_last_error(&ctx) == vschannel_sec_e_no_credentials
	C.vschannel_cleanup(&ctx)
}
