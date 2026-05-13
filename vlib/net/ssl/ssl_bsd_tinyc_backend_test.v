// vtest build: tinyc && (freebsd || openbsd)
module ssl

fn test_bsd_tinyc_defaults_to_openssl_backend() {
	mut conn := SSLConn{}
	assert conn.sslctx == unsafe { nil }
}
