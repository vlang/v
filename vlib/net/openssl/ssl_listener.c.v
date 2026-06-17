module openssl

import net
import time
import os

// SSLListener is the SSL listener implementation for OpenSSL.
pub struct SSLListener {
	saddr  string
	config SSLConnectConfig
mut:
	tcp_listener &net.TcpListener = unsafe { nil }
	sslctx       &C.SSL_CTX       = unsafe { nil }
}

// new_ssl_listener creates a new SSLListener binding to `saddr` with `config`.
pub fn new_ssl_listener(saddr string, config SSLConnectConfig) !&SSLListener {
	mut listener := &SSLListener{
		saddr:  saddr
		config: config
	}
	listener.init()!
	return listener
}

fn (mut l SSLListener) init() ! {
	if l.config.cert == '' || l.config.cert_key == '' {
		return error('net.openssl SSLListener.init, no certificate or key provided')
	}

	l.tcp_listener = net.listen_tcp(.ip, l.saddr, net.ListenOptions{})!

	l.sslctx = unsafe { C.SSL_CTX_new(C.TLS_server_method()) }
	if l.sslctx == 0 {
		l.tcp_listener.close() or {}
		return error('net.openssl SSLListener.init, could not get ssl context')
	}

	mut cert := l.config.cert
	mut cert_key := l.config.cert_key

	if l.config.in_memory_verification {
		now := time.now().unix().str()
		cert = os.temp_dir() + '/v_srv_cert' + now
		cert_key = os.temp_dir() + '/v_srv_cert_key' + now
		if l.config.cert != '' {
			os.write_file(cert, l.config.cert)!
		}
		if l.config.cert_key != '' {
			os.write_file(cert_key, l.config.cert_key)!
		}
	}

	mut res := C.SSL_CTX_use_certificate_file(voidptr(l.sslctx), &char(cert.str),
		C.SSL_FILETYPE_PEM)
	if res != 1 {
		C.ERR_print_errors_fp(C.stderr)
		l.shutdown() or {}
		return error('net.openssl SSLListener.init, SSL_CTX_use_certificate_file failed')
	}

	res = C.SSL_CTX_use_PrivateKey_file(voidptr(l.sslctx), &char(cert_key.str), C.SSL_FILETYPE_PEM)
	if res != 1 {
		l.shutdown() or {}
		return error('net.openssl SSLListener.init, SSL_CTX_use_PrivateKey_file failed')
	}

	res = C.SSL_CTX_check_private_key(voidptr(l.sslctx))
	if res != 1 {
		l.shutdown() or {}
		return error('net.openssl SSLListener.init, SSL_CTX_check_private_key failed')
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
		res := C.SSL_accept(voidptr(conn.ssl))
		if res == 1 {
			break
		}

		err_res := ssl_error(res, conn.ssl)!
		if err_res == .ssl_error_want_read {
			conn.wait_for_read(ssl_remaining_timeout(deadline))!
			continue
		}
		if err_res == .ssl_error_want_write {
			conn.wait_for_write(ssl_remaining_timeout(deadline))!
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
