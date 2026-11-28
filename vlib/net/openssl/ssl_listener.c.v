module openssl

import net
import sync

// SSLListener is the SSL listener implementation for OpenSSL.
pub struct SSLListener {
	saddr   string
	config  SSLConnectConfig
	options SSLListenerOptions
mut:
	context_mu   &sync.Mutex      = sync.new_mutex()
	reload_mu    &sync.Mutex      = sync.new_mutex()
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

fn listen_ssl_tcp(saddr string, family net.AddrFamily, options net.ListenOptions) !&net.TcpListener {
	if family != .unspec {
		return net.listen_tcp(family, saddr, options)
	}
	address, _ := net.split_address(saddr)!
	if address == '' || address == '::' {
		return net.listen_tcp(.ip6, saddr, options)
	}
	addrs := net.resolve_addrs(saddr, .unspec, .tcp)!
	mut attempted := []net.AddrFamily{}
	mut errors := []IError{}
	for addr in addrs {
		candidate := addr.family()
		if candidate in attempted {
			continue
		}
		attempted << candidate
		listener := net.listen_tcp(candidate, saddr, options) or {
			errors << err
			continue
		}
		return listener
	}
	if errors.len > 0 {
		return errors[errors.len - 1]
	}
	return error('net.openssl SSLListener.init, no addresses resolved for ${saddr}')
}

fn (mut l SSLListener) init() ! {
	ctx := new_server_ssl_context(l.config)!
	l.tcp_listener = listen_ssl_tcp(l.saddr, l.options.family, net.ListenOptions{}) or {
		C.SSL_CTX_free(ctx)
		return err
	}
	l.sslctx = ctx
}

fn new_server_ssl_context(config SSLConnectConfig) !&C.SSL_CTX {
	if config.cert == '' || config.cert_key == '' {
		return error('net.openssl SSLListener.init, no certificate or key provided')
	}
	if config.validate && config.verify == '' {
		return error('net.openssl SSLListener.init, no root CA provided')
	}

	ctx := unsafe { C.SSL_CTX_new(C.v_net_openssl_TLS_server_method()) }
	if ctx == 0 {
		return error('net.openssl SSLListener.init, could not get ssl context')
	}
	mut ready := false
	defer {
		if !ready { C.SSL_CTX_free(ctx) }
	}
	C.SSL_CTX_set_options(ctx, C.SSL_OP_NO_COMPRESSION | C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3)

	if config.in_memory_verification {
		mut res := C.v_net_openssl_SSL_CTX_use_certificate_chain_memory(ctx,
			config.cert.str, uint(config.cert.len))
		if res != 1 {
			return error('net.openssl SSLListener.init, could not load certificate from memory')
		}
		res = C.v_net_openssl_SSL_CTX_use_PrivateKey_memory(ctx, config.cert_key.str,
			uint(config.cert_key.len))
		if res != 1 {
			return error('net.openssl SSLListener.init, could not load private key from memory')
		}
		if config.validate {
			res = C.v_net_openssl_SSL_CTX_load_verify_memory(ctx, config.verify.str,
				uint(config.verify.len))
			if res != 1 {
				return error('net.openssl SSLListener.init, could not load root CA from memory')
			}
		}
	} else {
		mut res := C.SSL_CTX_use_certificate_chain_file(voidptr(ctx), &char(config.cert.str))
		if res != 1 {
			return error('net.openssl SSLListener.init, SSL_CTX_use_certificate_chain_file failed')
		}
		res = C.SSL_CTX_use_PrivateKey_file(voidptr(ctx), &char(config.cert_key.str),
			C.SSL_FILETYPE_PEM)
		if res != 1 {
			return error('net.openssl SSLListener.init, SSL_CTX_use_PrivateKey_file failed')
		}
	}

	mut res := C.SSL_CTX_check_private_key(voidptr(ctx))
	if res != 1 {
		return error('net.openssl SSLListener.init, SSL_CTX_check_private_key failed')
	}

	if config.validate {
		if !config.in_memory_verification {
			res = C.SSL_CTX_load_verify_locations(voidptr(ctx), &char(config.verify.str),
				unsafe { nil })
			if res != 1 {
				return error('net.openssl SSLListener.init, SSL_CTX_load_verify_locations failed')
			}
			res = C.v_net_openssl_SSL_CTX_load_client_CA_file(ctx, &char(config.verify.str))
			if res != 1 {
				return error('net.openssl SSLListener.init, SSL_load_client_CA_file failed')
			}
		}
		C.SSL_CTX_set_verify(voidptr(ctx),
			C.SSL_VERIFY_PEER | C.SSL_VERIFY_FAIL_IF_NO_PEER_CERT, unsafe { nil })
	}

	if config.alpn_protocols.len > 0 {
		mut wire := []u8{cap: 64}
		for proto in config.alpn_protocols {
			if proto.len == 0 || proto.len > 255 {
				return error('net.openssl SSLListener.init, invalid ALPN protocol "${proto}"')
			}
			wire << u8(proto.len)
			wire << proto.bytes()
		}
		if C.v_net_openssl_SSL_CTX_set_alpn_select_protos(ctx, wire.data, u32(wire.len)) == unsafe { nil } {
			return error('net.openssl SSLListener.init, failed to configure ALPN protocols (requires OpenSSL >= 1.0.2)')
		}
	}
	ready = true
	return ctx
}

// reload_certificate rereads the configured certificate chain and private key files.
// A failed reload leaves the current TLS context active. SSL_new retains a
// reference to the context it uses, so existing sessions keep their context.
pub fn (mut l SSLListener) reload_certificate() ! {
	if l.config.in_memory_verification {
		return error('net.openssl SSLListener.reload_certificate: file-backed credentials are required')
	}
	l.reload_mu.@lock()
	defer { l.reload_mu.unlock() }
	candidate := new_server_ssl_context(l.config)!
	l.context_mu.@lock()
	previous := l.sslctx
	if previous == unsafe { nil } {
		l.context_mu.unlock()
		C.SSL_CTX_free(candidate)
		return error('net.openssl SSLListener.reload_certificate: listener is closed')
	}
	l.sslctx = candidate
	l.context_mu.unlock()
	C.SSL_CTX_free(previous)
}

fn (mut l SSLListener) new_ssl() &C.SSL {
	l.context_mu.@lock()
	defer { l.context_mu.unlock() }
	if l.sslctx == unsafe { nil } {
		return unsafe { nil }
	}
	return unsafe { &C.SSL(C.SSL_new(l.sslctx)) }
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

	ssl := l.new_ssl()
	if ssl == 0 {
		tcp_conn.close() or {}
		return error('net.openssl SSLListener.accept, could not create SSL instance')
	}

	if C.v_net_openssl_SSL_set_fd(voidptr(ssl), tcp_conn.sock.handle) != 1 {
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
		sslctx:      unsafe { nil } // Accepted connections must not free the listener's shared SSL context.
		ssl:         ssl
		handle:      tcp_conn.sock.handle
		duration:    tcp_conn.read_timeout()
		owns_socket: true
	}

	return conn
}

// accept_handshake performs the SSL handshake on the connection.
pub fn (mut conn SSLConn) accept_handshake() ! {
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
				return error_with_code(net.err_timed_out.msg(), net.err_timed_out.code())
			}
			conn.wait_for_read(timeout)!
			continue
		}
		if err_res == .ssl_error_want_write {
			timeout := ssl_remaining_timeout(deadline)
			if timeout <= 0 {
				return error_with_code(net.err_timed_out.msg(), net.err_timed_out.code())
			}
			conn.wait_for_write(timeout)!
			continue
		}

		return error('net.openssl SSLListener.accept, SSL handshake failed: ${err_res}')
	}
}

// shutdown shuts down the SSL listener and releases the context.
pub fn (mut l SSLListener) shutdown() ! {
	l.context_mu.@lock()
	ctx := l.sslctx
	l.sslctx = unsafe { nil }
	l.context_mu.unlock()
	if ctx != unsafe { nil } {
		C.SSL_CTX_free(ctx)
	}
	if l.tcp_listener != unsafe { nil } {
		l.tcp_listener.close()!
		l.tcp_listener = unsafe { nil }
	}
}
