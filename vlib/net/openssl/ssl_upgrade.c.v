module openssl

import net

// new_ssl_server_conn wraps an already-established TCP connection in a
// server-side TLS connection using `config`'s certificate. It performs the
// TLS handshake before returning. This enables protocols that upgrade a
// plain connection to TLS (e.g. SMTP STARTTLS). The caller retains ownership of
// tcp_conn and must keep it open until the returned TLS connection is shut down.
pub fn new_ssl_server_conn(mut tcp_conn net.TcpConn, config SSLConnectConfig) !&SSLConn {
	if config.cert == '' || config.cert_key == '' {
		return error('net.openssl new_ssl_server_conn, no certificate or key provided')
	}

	sslctx := C.SSL_CTX_new(C.v_net_openssl_TLS_server_method())
	if sslctx == 0 {
		return error('net.openssl new_ssl_server_conn, could not get ssl context')
	}
	C.SSL_CTX_set_options(sslctx, C.SSL_OP_NO_COMPRESSION | C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3)

	if config.in_memory_verification {
		mut res := C.v_net_openssl_SSL_CTX_use_certificate_chain_memory(sslctx,
			config.cert.str, usize(config.cert.len))
		if res != 1 {
			C.SSL_CTX_free(sslctx)
			return error('net.openssl new_ssl_server_conn, could not load certificate from memory')
		}
		res = C.v_net_openssl_SSL_CTX_use_PrivateKey_memory(sslctx, config.cert_key.str,
			usize(config.cert_key.len))
		if res != 1 {
			C.SSL_CTX_free(sslctx)
			return error('net.openssl new_ssl_server_conn, could not load private key from memory')
		}
	} else {
		mut res := C.SSL_CTX_use_certificate_chain_file(voidptr(sslctx), &char(config.cert.str))
		if res != 1 {
			C.SSL_CTX_free(sslctx)
			return error('net.openssl new_ssl_server_conn, SSL_CTX_use_certificate_chain_file failed')
		}
		res = C.SSL_CTX_use_PrivateKey_file(voidptr(sslctx), &char(config.cert_key.str),
			C.SSL_FILETYPE_PEM)
		if res != 1 {
			C.SSL_CTX_free(sslctx)
			return error('net.openssl new_ssl_server_conn, SSL_CTX_use_PrivateKey_file failed')
		}
	}

	mut res := C.SSL_CTX_check_private_key(voidptr(sslctx))
	if res != 1 {
		C.SSL_CTX_free(sslctx)
		return error('net.openssl new_ssl_server_conn, SSL_CTX_check_private_key failed')
	}

	ssl := unsafe { &C.SSL(C.SSL_new(sslctx)) }
	if ssl == 0 {
		C.SSL_CTX_free(sslctx)
		return error('net.openssl new_ssl_server_conn, could not create SSL instance')
	}

	if C.SSL_set_fd(voidptr(ssl), tcp_conn.sock.handle) != 1 {
		C.SSL_free(voidptr(ssl))
		C.SSL_CTX_free(sslctx)
		return error('net.openssl new_ssl_server_conn, could not assign ssl to socket')
	}

	net.set_blocking(tcp_conn.sock.handle, false) or {
		C.SSL_free(voidptr(ssl))
		C.SSL_CTX_free(sslctx)
		return error('net.openssl new_ssl_server_conn, could not make socket non-blocking: ${err}')
	}

	mut conn := &SSLConn{
		config:      config
		sslctx:      sslctx
		ssl:         ssl
		handle:      tcp_conn.sock.handle
		duration:    tcp_conn.read_timeout()
		owns_socket: false
	}
	// The TLS connection borrows the socket; shutdown leaves closing it to the caller.
	conn.accept_handshake() or {
		C.SSL_free(voidptr(conn.ssl))
		C.SSL_CTX_free(sslctx)
		return err
	}
	return conn
}
