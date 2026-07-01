module openssl

import net

// SSLListener is the SSL listener implementation for OpenSSL.
pub struct SSLListener {
	saddr   string
	config  SSLConnectConfig
	options SSLListenerOptions
mut:
	tcp_listener &net.TcpListener = unsafe { nil }
	sslctx       &C.SSL_CTX       = unsafe { nil }
}

// SSLListenerOptions configures the TCP listener used by an SSLListener.
@[params]
pub struct SSLListenerOptions {
pub:
	family net.AddrFamily = .unspec
}

// new_ssl_listener creates a new SSLListener binding to `saddr` with `config`.
pub fn new_ssl_listener(saddr string, config SSLConnectConfig, options SSLListenerOptions) !&SSLListener {
	mut listener := &SSLListener{
		saddr:   saddr
		config:  config
		options: options
	}
	listener.init()!
	return listener
}

fn ssl_listener_family(saddr string, family net.AddrFamily) net.AddrFamily {
	if family != .unspec {
		return family
	}
	address, _ := net.split_address(saddr) or { return .ip }
	if address == '::' || address.contains(':') {
		return .ip6
	}
	return .ip
}

fn (mut l SSLListener) init() ! {
	if l.config.cert == '' || l.config.cert_key == '' {
		return error('net.openssl SSLListener.init, no certificate or key provided')
	}
	if l.config.validate && l.config.verify == '' {
		return error('net.openssl SSLListener.init, no root CA provided')
	}

	l.tcp_listener =
		net.listen_tcp(ssl_listener_family(l.saddr, l.options.family), l.saddr, net.ListenOptions{})!

	l.sslctx = unsafe { C.SSL_CTX_new(C.v_net_openssl_TLS_server_method()) }
	if l.sslctx == 0 {
		l.tcp_listener.close() or {}
		return error('net.openssl SSLListener.init, could not get ssl context')
	}
	C.SSL_CTX_set_options(l.sslctx, C.SSL_OP_NO_COMPRESSION | C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3)

	if l.config.in_memory_verification {
		mut res := C.v_net_openssl_SSL_CTX_use_certificate_chain_memory(l.sslctx,
			l.config.cert.str, usize(l.config.cert.len))
		if res != 1 {
			l.shutdown() or {}
			return error('net.openssl SSLListener.init, could not load certificate from memory')
		}
		res = C.v_net_openssl_SSL_CTX_use_PrivateKey_memory(l.sslctx, l.config.cert_key.str,
			usize(l.config.cert_key.len))
		if res != 1 {
			l.shutdown() or {}
			return error('net.openssl SSLListener.init, could not load private key from memory')
		}
		if l.config.validate {
			res = C.v_net_openssl_SSL_CTX_load_verify_memory(l.sslctx, l.config.verify.str,
				usize(l.config.verify.len))
			if res != 1 {
				l.shutdown() or {}
				return error('net.openssl SSLListener.init, could not load root CA from memory')
			}
		}
	} else {
		mut res := C.SSL_CTX_use_certificate_chain_file(voidptr(l.sslctx), &char(l.config.cert.str))
		if res != 1 {
			C.ERR_print_errors_fp(C.stderr)
			l.shutdown() or {}
			return error('net.openssl SSLListener.init, SSL_CTX_use_certificate_chain_file failed')
		}
		res = C.SSL_CTX_use_PrivateKey_file(voidptr(l.sslctx), &char(l.config.cert_key.str),
			C.SSL_FILETYPE_PEM)
		if res != 1 {
			l.shutdown() or {}
			return error('net.openssl SSLListener.init, SSL_CTX_use_PrivateKey_file failed')
		}
	}

	mut res := C.SSL_CTX_check_private_key(voidptr(l.sslctx))
	if res != 1 {
		l.shutdown() or {}
		return error('net.openssl SSLListener.init, SSL_CTX_check_private_key failed')
	}

	if l.config.validate {
		if !l.config.in_memory_verification {
			res = C.SSL_CTX_load_verify_locations(voidptr(l.sslctx), &char(l.config.verify.str),
				unsafe { nil })
			if res != 1 {
				l.shutdown() or {}
				return error('net.openssl SSLListener.init, SSL_CTX_load_verify_locations failed')
			}
		}
		C.SSL_CTX_set_verify(voidptr(l.sslctx),
			C.SSL_VERIFY_PEER | C.SSL_VERIFY_FAIL_IF_NO_PEER_CERT, unsafe { nil })
	}

	if l.config.alpn_protocols.len > 0 {
		mut wire := []u8{cap: 64}
		for proto in l.config.alpn_protocols {
			if proto.len == 0 || proto.len > 255 {
				l.shutdown() or {}
				return error('net.openssl SSLListener.init, invalid ALPN protocol "${proto}"')
			}
			wire << u8(proto.len)
			wire << proto.bytes()
		}
		if C.v_net_openssl_SSL_CTX_set_alpn_select_protos(l.sslctx, wire.data, u32(wire.len)) != 0 {
			l.shutdown() or {}
			return error('net.openssl SSLListener.init, failed to configure ALPN protocols (requires OpenSSL >= 1.0.2)')
		}
	}
}

// accept accepts a new TCP connection and performs the SSL handshake.
pub fn (mut l SSLListener) accept() !&SSLConn {
	mut conn := l.accept_without_handshake()!
	conn.accept_handshake() or {
		conn.shutdown() or {}
		return err
	}
	return conn
}

// accept_without_handshake accepts a new TCP connection but does not perform the handshake yet.
pub fn (mut l SSLListener) accept_without_handshake() !&SSLConn {
	mut tcp_conn := l.tcp_listener.accept()!

	ssl := unsafe { &C.SSL(C.SSL_new(l.sslctx)) }
	if ssl == 0 {
		tcp_conn.close() or {}
		return error('net.openssl SSLListener.accept, could not create SSL instance')
	}

	if C.SSL_set_fd(voidptr(ssl), tcp_conn.sock.handle) != 1 {
		C.SSL_free(voidptr(ssl))
		tcp_conn.close() or {}
		return error('net.openssl SSLListener.accept, could not assign ssl to socket')
	}
	// OpenSSL only returns SSL_ERROR_WANT_READ/WRITE while the socket is non-blocking.
	// The SSLConn retry loops use those results to enforce the configured deadline.
	net.set_blocking(tcp_conn.sock.handle, false) or {
		C.SSL_free(voidptr(ssl))
		tcp_conn.close() or {}
		return error('net.openssl SSLListener.accept, could not make socket non-blocking: ${err}')
	}

	mut conn := &SSLConn{
		config:      l.config
		sslctx:      unsafe { nil } // 设为 nil 避免连接关闭时意外释放共享的 sslctx
		ssl:         ssl
		handle:      tcp_conn.sock.handle
		duration:    tcp_conn.read_timeout()
		owns_socket: true
	}

	return conn
}

// accept_handshake performs the SSL handshake on the connection.
pub fn (mut conn SSLConn) accept_handshake() ! {
	// 执行 SSL 握手过程
	deadline := ssl_timeout_deadline(conn.duration)
	for {
		C.ERR_clear_error()
		res := C.SSL_accept(voidptr(conn.ssl))
		if res == 1 {
			break
		}

		err_res := ssl_error(res, conn.ssl)!
		if err_res == .ssl_error_want_read {
			timeout := ssl_remaining_timeout(deadline)
			if timeout <= 0 {
				return net.err_timed_out
			}
			conn.wait_for_read(timeout)!
			continue
		}
		if err_res == .ssl_error_want_write {
			timeout := ssl_remaining_timeout(deadline)
			if timeout <= 0 {
				return net.err_timed_out
			}
			conn.wait_for_write(timeout)!
			continue
		}

		// 握手失败
		return error('net.openssl SSLListener.accept, SSL handshake failed: ${err_res}')
	}
}

// shutdown shuts down the SSL listener and releases the context.
pub fn (mut l SSLListener) shutdown() ! {
	if l.sslctx != 0 {
		C.SSL_CTX_free(l.sslctx)
		l.sslctx = unsafe { nil }
	}
	if l.tcp_listener != unsafe { nil } {
		l.tcp_listener.close()!
	}
}
